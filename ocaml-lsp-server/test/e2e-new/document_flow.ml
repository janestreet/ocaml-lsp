module Fiber = Ocaml_lsp_fiber
open Async
open Test.Import

let%expect_test "it should allow double opening the same document" =
  let diagnostics = Ocaml_lsp_fiber_shims.Mvar.create () in
  let drain_diagnostics () = Ocaml_lsp_fiber_shims.Mvar.take diagnostics in
  let handler =
    let on_request
      (type resp state)
      (client : state Client.t)
      (req : resp Lsp.Server_request.t)
      ~(request_time : Core.Time_ns.t option)
      ~(event_index : int option)
      : (resp Lsp_fiber.Rpc.Reply.t * state) Fiber.t
      =
      ignore request_time;
      ignore event_index;
      match req with
      | Lsp.Server_request.ClientUnregisterCapability _ ->
        let state = Client.state client in
        Fiber.return (Lsp_fiber.Rpc.Reply.now (), state)
      | _ -> assert false
    in
    Client.Handler.make
      ~on_notification:(fun _ n ~event_index:_ ->
        match n with
        | PublishDiagnostics _ ->
          let+ r = Ocaml_lsp_fiber_shims.Mvar.put diagnostics () in
          r, None
        | _ -> Fiber.return ((), None))
      ~on_request:{ Client.Handler.on_request }
      ()
  in
  let%map.Deferred () =
    Test.run ~handler
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
      let* () =
        let settings =
          `Assoc
            [ ( "whichDiagnostics"
              , `Assoc
                  [ "merlin_syntax", `Bool true
                  ; "merlin_typing", `Bool true
                  ; "dune", `Bool false
                  ] )
            ]
        in
        Client.notification client (ChangeConfiguration { settings })
      in
      let uri = DocumentUri.of_path "foo.ml" in
      let open_ text =
        let textDocument =
          TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text
        in
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* () = open_ "text 1" in
      let* () = drain_diagnostics () in
      let+ () = open_ "text 2" in
      ()
    in
    Fiber.fork_and_join_unit run_client (fun () ->
      run >>> drain_diagnostics () >>> Client.stop client)
  in
  [%expect {| |}]
;;
