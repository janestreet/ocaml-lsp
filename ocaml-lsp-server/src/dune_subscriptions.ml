module Fiber = Ocaml_lsp_fiber
open! Core
open! Async
module Diagnostic_parser = Ocaml_lsp_dune_integration.Diagnostic_parser

open struct
  open Import
  module Diagnostic = Diagnostic
  module DiagnosticRelatedInformation = DiagnosticRelatedInformation
  module Diagnostics = Diagnostics
  module DiagnosticSeverity = DiagnosticSeverity
  module Drpc = Drpc
  module Dune_rpc_async = Drpc.Async
  module Fiber_async = Ocaml_lsp_fiber_shims.Fiber_async
  module File_path = File_path
  module Loc = Loc
  module Location = Location
  module Log = Log
  module String = String
  module Uri = Uri

  let inside_test = inside_test
end

module Dir_subscription = struct
  type t =
    { mutable refcount : int
    ; cancel : unit Ivar.t
    }
end

type t =
  { should_terminate : unit Ivar.t
  ; dir_subscriptions : Dir_subscription.t String.Table.t
  }

let create () =
  { should_terminate = Ivar.create (); dir_subscriptions = String.Table.create () }
;;

let close t = Ivar.fill_exn t.should_terminate ()

let is_dune_running () =
  Fiber_async.fiber_of_deferred
    (match%bind
       Dune_rpc_async.Connection.with_connection
         ~cleanup_timeout:(Time_float.Span.of_sec 0.)
         ~f:(fun _ -> return ())
         ()
     with
     | Ok _ -> return true
     | Server_not_running _ -> return false
     | Error e ->
       Ocaml_lsp_logging.log_debug
         ~info:[%message (e : Error.t)]
         "error while looking for dune";
       return false)
;;

let with_connection f =
  (* [with_connection] can raise when dune exits uncleanly and an orphaned rpc socket file
     is left behind. *)
  Monitor.try_with_join_or_error (fun () ->
    Dune_rpc_async.Connection.with_connection_or_error
      ~cleanup_timeout:(Time_float.Span.of_sec 5.)
      ~f
      ())
;;

let subscribe_impl should_terminate sub =
  let reader_or_error_ivar = Ivar.create () in
  don't_wait_for
    (with_connection (fun conn ->
       (* We prefer [Dune_rpc_async] to [Dune_rpc] because it tends to stay more up to
          date with the internal version of Dune. *)
       match%bind Dune_rpc_async.Connection.poll conn sub with
       | Error e ->
         Ivar.fill_if_empty reader_or_error_ivar (Error e);
         return ()
       | Ok stream ->
         let reader, writer = Pipe.create () in
         let () = Ivar.fill_exn reader_or_error_ivar (Ok reader) in
         let () =
           don't_wait_for
             (let%bind () = Ivar.read should_terminate in
              Dune_rpc_async.Stream.cancel stream)
         in
         Deferred.repeat_until_finished () (fun () ->
           match%bind Dune_rpc_async.Stream.next ~here:[%here] stream with
           | Some update ->
             let%bind () =
               (* We intentionally use [write] and not [write_without_pushback] here
                  because Dune RPC accounts for slow clients and skips unnecessary updates
                  if clients do not keep up. We previously attempted to implement
                  [subscribe] without using a pipe by just having it take an [f] and then
                  binding here on [f update], but this does not work because [f] would be
                  implemented in terms of fibers so you'd need to convert its result into
                  a deferred. [Fiber_async] is designed to have deferred values converted
                  into fibers throughout the app and only have the fiber scheduler
                  implemented in terms of deferreds at the app's outermost edge (in our
                  case, by [Ocaml_lsp_server.run]). The pipe enables us to push back here
                  while reading updates in a function implemented with fibers. *)
               Pipe.write writer update
             in
             return (`Repeat ())
           | None -> return (`Finished ())))
     >>| fun stream_ended_ok_or_error ->
     (* Need to make sure that reading the [reader_or_error] Fiber will resolve
        - this prevents tests from hanging *)
     let () = Ivar.fill_if_empty reader_or_error_ivar (Error (Base.Error.of_string "")) in
     let () =
       match Ivar.peek reader_or_error_ivar with
       | Some (Error _) -> ()
       | Some (Ok reader) -> Pipe.close_read reader
       | None ->
         (* We fill [reader_or_error] here to ensure that reading it doesn't hang. *)
         Ivar.fill_exn
           reader_or_error_ivar
           (Or_error.error_string "Failed to subscribe to dune updates")
     in
     match stream_ended_ok_or_error with
     | Error _ when not inside_test ->
       (* There's currently no way to have separate subsets of [Async.log.global] go to
          the editor and the log management system. The following uses [Import.Log] in the
          same manner as elsewhere in ocaml-lsp, but [Import.Log] doesn't actually log
          anything right now. *)
       Log.log ~section:"warning"
       @@ fun () -> Log.msg "Dune raised while reading stream" []
     | _ -> ());
  let open Fiber.O in
  let+ reader_or_error = Fiber_async.fiber_of_deferred (Ivar.read reader_or_error_ivar) in
  match reader_or_error with
  | Error _ as err -> err
  | Ok reader -> Ok (fun () -> Fiber_async.fiber_of_deferred (Pipe.read reader))
;;

let subscribe t sub = subscribe_impl t.should_terminate sub

(* Start a build-watch subscription for the merlin alias in a directory. The subscription
   will be cancelled when the cancel ivar is filled. *)
let start_build_watch_merlin_for_dir ~dir ~cancel =
  don't_wait_for
    (with_connection (fun conn ->
       let goals =
         [ Drpc.Build.Goal.Alias { dir; name = "merlin-nonrec"; promote = false } ]
       in
       let request : Drpc.Build.Watch.Request.t =
         { goals; relative_to = Drpc.Path.dune_root }
       in
       match%bind
         Dune_rpc_async.Connection.poll_with_init conn Drpc.Sub.build_watch request
       with
       | Error e ->
         Ocaml_lsp_logging.log_error
           ~info:[%message (dir : string) (e : Error.t)]
           "error starting build-watch for dir";
         return ()
       | Ok stream ->
         let () =
           don't_wait_for
             (let%bind () = Ivar.read cancel in
              Dune_rpc_async.Stream.cancel stream)
         in
         (* Read updates until the stream ends or is cancelled *)
         Deferred.repeat_until_finished () (fun () ->
           match%bind Dune_rpc_async.Stream.next ~here:[%here] stream with
           | Some _outcome ->
             (* We don't need to do anything with the outcome - we just want the build to
                happen so merlin files are up to date *)
             return (`Repeat ())
           | None -> return (`Finished ())))
     >>| fun _result -> ())
;;

let register_open_dir t ~dir =
  Fiber_async.fiber_of_deferred
    (match Hashtbl.find t.dir_subscriptions dir with
     | Some sub ->
       sub.refcount <- sub.refcount + 1;
       return ()
     | None ->
       let cancel = Ivar.create () in
       start_build_watch_merlin_for_dir ~dir ~cancel;
       let sub = { Dir_subscription.refcount = 1; cancel } in
       Hashtbl.set t.dir_subscriptions ~key:dir ~data:sub;
       return ())
;;

let unregister_open_dir t ~dir =
  Fiber_async.fiber_of_deferred
    (match Hashtbl.find t.dir_subscriptions dir with
     | None -> return ()
     | Some sub ->
       sub.refcount <- sub.refcount - 1;
       if sub.refcount <= 0
       then (
         Hashtbl.remove t.dir_subscriptions dir;
         Ivar.fill_if_empty sub.cancel ());
       return ())
;;

let set_dune_diagnostics diagnostics events =
  List.iter events ~f:(fun (event : Drpc.Diagnostic.Event.t) ->
    match event with
    | Remove dune_diagnostic ->
      let id = Drpc.Diagnostic.id dune_diagnostic in
      Diagnostics.remove_dune_diagnostics diagnostics id
    | Add dune_diagnostic ->
      let id = Drpc.Diagnostic.id dune_diagnostic in
      let new_diagnostics =
        Diagnostic_parser.of_dune_diagnostic dune_diagnostic
        |> List.map ~f:(fun (d : Diagnostic_parser.Diagnostic.t) ->
          let filename = File_path.Absolute.to_string d.location.filename in
          let lsp_diagnostic = Diagnostics.of_diagnostic_parser diagnostics d in
          Uri.of_path filename, lsp_diagnostic)
      in
      Diagnostics.add_dune_diagnostics diagnostics (id, new_diagnostics))
;;

module For_testing = struct
  let get_dir_refcount t ~dir =
    Hashtbl.find t.dir_subscriptions dir
    |> Option.map ~f:(fun (sub : Dir_subscription.t) -> sub.refcount)
  ;;

  let get_tracked_dirs t =
    Hashtbl.keys t.dir_subscriptions |> List.sort ~compare:String.compare
  ;;
end
