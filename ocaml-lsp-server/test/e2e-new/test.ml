module Fiber = Ocaml_lsp_fiber

module Import = struct
  include struct
    module Array = struct
      include Core.Array

      module Iter : sig
        type 'a t

        val create : 'a array -> 'a t
        val has_next : 'a t -> bool
        val next : 'a t -> 'a option
        val next_exn : 'a t -> 'a
      end = struct
        type 'a t =
          { contents : 'a array
          ; mutable ix : int
          }

        let create contents = { contents; ix = 0 }
        let has_next t = t.ix < Core.Array.length t.contents

        let next_exn t =
          let { contents; ix } = t in
          let v = contents.(ix) in
          t.ix <- ix + 1;
          v
        ;;

        let next t = if has_next t then Some (next_exn t) else None
      end
    end
  end

  include Fiber.O
  module Client = Lsp_fiber.Client
  include Lsp.Types
  module Uri = Lsp.Uri
  module Position = Ocaml_lsp_server.Position
  module Option = Core.Option
  module List = Core.List
  module String = Core.String
end

open Import
module Fiber_async = Ocaml_lsp_fiber_shims.Fiber_async

module T : sig
  val run_with_status
    :  ?extra_env:string list
    -> ?cwd:string
    -> ?handler:unit Client.Handler.t
    -> ?timeout_s:float
    -> (unit Client.t -> 'a Fiber.t)
    -> (Unix.process_status * 'a) Async.Deferred.t

  val run
    :  ?extra_env:string list
    -> ?cwd:string
    -> ?handler:unit Client.Handler.t
    -> ?timeout_s:float
    -> (unit Client.t -> 'a Fiber.t)
    -> 'a Async.Deferred.t
end = struct
  let bin = Ocaml_lsp_test_lib.get_test_ocaml_lsp_bin ()

  let run_with_status ?(extra_env = []) ?cwd ?handler ?(timeout_s = 3.) f =
    let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
    let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
    let pid =
      let env =
        let current = Unix.environment () in
        Array.to_list current @ extra_env |> Spawn.Env.of_list
      in
      let cwd =
        match cwd with
        | None -> Spawn.Working_dir.Inherit
        | Some path -> Spawn.Working_dir.Path path
      in
      Spawn.spawn ~env ~cwd ~prog:bin ~argv:[ bin ] ~stdin:stdin_i ~stdout:stdout_o ()
    in
    Unix.close stdin_i;
    Unix.close stdout_o;
    let handler =
      match handler with
      | Some h -> h
      | None -> Client.Handler.make ()
    in
    let init =
      let blockity =
        if Stdlib.Sys.win32
        then `Blocking
        else (
          Unix.set_nonblock stdout_i;
          Unix.set_nonblock stdin_o;
          `Non_blocking true)
      in
      let make fd what =
        let fd = Lev_fiber.Fd.create fd blockity in
        Lev_fiber.Io.create fd what
      in
      let* in_ = make stdout_i Input in
      let* out = make stdin_o Output in
      let io = Lsp_fiber.Fiber_io.make in_ out in
      let client = Client.make handler io () in
      f client
    in
    (* TODO replace the wheel once we can cancel sleep *)
    let waitpid wheel =
      let* timeout = Lev_fiber.Timer.Wheel.task wheel in
      Fiber.finalize ~finally:(fun () -> Lev_fiber.Timer.Wheel.stop wheel)
      @@ fun () ->
      let cancelled = ref false in
      Fiber.fork_and_join_unit
        (fun () ->
          Lev_fiber.Timer.Wheel.await timeout
          >>| function
          | `Cancelled -> ()
          | `Ok ->
            Unix.kill pid Stdlib.Sys.sigkill;
            cancelled := true)
        (fun () ->
          let* (server_exit_status : Unix.process_status) = Lev_fiber.waitpid ~pid in
          let+ () =
            if !cancelled then Fiber.return () else Lev_fiber.Timer.Wheel.cancel timeout
          in
          server_exit_status)
    in
    let fiber =
      Fiber.of_thunk (fun () ->
        let* wheel = Lev_fiber.Timer.Wheel.create ~delay:timeout_s in
        let+ res = init
        and+ status =
          Fiber.fork_and_join_unit
            (fun () -> Lev_fiber.Timer.Wheel.run wheel)
            (fun () -> waitpid wheel)
        in
        status, res)
    in
    Fiber_async.deferred_of_fiber fiber ()
  ;;

  let run ?extra_env ?cwd ?handler ?timeout_s f =
    Async.Deferred.map (run_with_status ?extra_env ?cwd ?handler ?timeout_s f) ~f:snd
  ;;
end

include T

let drain_diagnostics () =
  let diagnostics = Fiber.Ivar.create () in
  let on_notification _ n ~event_index:_ =
    match n with
    | Lsp.Server_notification.PublishDiagnostics _ ->
      let* diag = Fiber.Ivar.peek diagnostics in
      let+ r =
        match diag with
        | Some _ -> Fiber.return ()
        | None -> Fiber.Ivar.fill diagnostics ()
      in
      r, None
    | _ -> Fiber.return ((), None)
  in
  on_notification, diagnostics
;;

let run_request ?(prep = fun _ -> Fiber.return ()) ?settings request =
  let on_notification, diagnostics = drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  run ~handler
  @@ fun client ->
  let run_client () =
    let capabilities =
      let window =
        let showDocument = ShowDocumentClientCapabilities.create ~support:true in
        WindowClientCapabilities.create ~showDocument ()
      in
      ClientCapabilities.create ~window ()
    in
    Client.start client (InitializeParams.create ~capabilities ())
  in
  let run =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let* () = prep client in
    let* () =
      match settings with
      | Some settings -> Client.notification client (ChangeConfiguration { settings })
      | None -> Fiber.return ()
    in
    Client.request client request
  in
  Fiber.fork_and_join_unit run_client (fun () ->
    let* ret = run in
    let* () = Fiber.Ivar.read diagnostics in
    let+ () = Client.stop client in
    ret)
;;

let openDocument ~client ~uri ~source =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:source
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

let humanDidOpen ~client ~uri ~source =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:source
  in
  Client.notification
    client
    (CustomNotification (HumanDidOpen (DidHumanOpenParams.create ~textDocument)))
;;

let print_result result =
  result |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;
