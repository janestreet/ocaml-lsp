open Import
module Fiber_extensions = Ocaml_lsp_fiber_shims
module Config_data = Config_data
module Version = Version
module Diagnostics = Diagnostics
module Position = Position
module Doc_to_md = Doc_to_md
module Diff = Diff
module References_req = References_req
module Testing = Testing
module Stage = Stage
open Fiber.O

let make_error = Jsonrpc.Response.Error.make

let not_supported () =
  Jsonrpc.Response.Error.raise
    (make_error ~code:InternalError ~message:"Request not supported yet!" ())
;;

let view_metrics_command_name = "ocamllsp/view-metrics"

let view_metrics server =
  let* json = Metrics.dump () in
  let uri, chan =
    Filename.open_temp_file (sprintf "lsp-metrics.%d" (Unix.getpid ())) ".json"
  in
  output_string chan json;
  close_out_noerr chan;
  let req =
    let uri = Uri.of_path uri in
    Server_request.ShowDocumentRequest (ShowDocumentParams.create ~uri ~takeFocus:true ())
  in
  let+ { ShowDocumentResult.success = _ } = Server.request server req in
  `Null
;;

let initialize_info (client_capabilities : ClientCapabilities.t) : InitializeResult.t =
  let codeActionProvider =
    match client_capabilities.textDocument with
    | Some { codeAction = Some { codeActionLiteralSupport = Some _; _ }; _ } ->
      let codeActionKinds =
        Action_inferred_intf.kind
        :: Action_destruct.kind
        :: List.map
             ~f:(fun (c : Code_action.t) -> c.kind)
             [ Action_type_annotate.t
             ; Action_remove_type_annotation.t
             ; Action_refactor_open.unqualify
             ; Action_refactor_open.qualify
             ; Action_add_rec.t
             ; Action_inline.t
             ]
        |> List.dedup_and_sort ~compare:Poly.compare
      in
      `CodeActionOptions (CodeActionOptions.create ~codeActionKinds ())
    | _ -> `Bool true
  in
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create
         ~openClose:true
         ~change:TextDocumentSyncKind.Incremental
         ~willSave:false
         ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
         ~willSaveWaitUntil:false
         ())
  in
  let codeLensProvider = CodeLensOptions.create ~resolveProvider:false () in
  let completionProvider =
    CompletionOptions.create ~triggerCharacters:[ "."; "#" ] ~resolveProvider:true ()
  in
  let signatureHelpProvider =
    SignatureHelpOptions.create ~triggerCharacters:[ " "; "~"; "?"; ":"; "(" ] ()
  in
  let renameProvider = `RenameOptions (RenameOptions.create ~prepareProvider:true ()) in
  let workspace =
    let workspaceFolders =
      WorkspaceFoldersServerCapabilities.create
        ~supported:true
        ~changeNotifications:(`Bool true)
        ()
    in
    ServerCapabilities.create_workspace ~workspaceFolders ()
  in
  let capabilities =
    let experimental =
      `Assoc
        [ ( "ocamllsp"
          , `Assoc
              ([ "interfaceSpecificLangId", `Bool true
               ; Req_switch_impl_intf.capability
               ; Req_infer_intf.capability
               ; Req_typed_holes.capability
               ; Req_typed_holes.jump_capability
               ; Req_wrapping_ast_node.capability
               ; Req_hover_extended.capability
               ; Req_merlin_call_compatible.capability
               ; Req_type_enclosing.capability
               ; Req_get_documentation.capability
               ; Req_complete_prefix_at_pos.capability
               ]
               @ Client_notification.CustomNotification.capabilities) )
        ]
    in
    let executeCommandProvider =
      let commands =
        if Action_open_related.available
             (let open Option.O in
              let* window = client_capabilities.window in
              window.showDocument)
        then
          [ view_metrics_command_name
          ; Action_open_related.command_name
          ; Document_text_command.command_name
          ; Merlin_config_command.command_name
          ]
        else []
      in
      ExecuteCommandOptions.create ~commands ()
    in
    let semanticTokensProvider =
      let full = `Full (SemanticTokensOptions.create_full ~delta:true ()) in
      `SemanticTokensOptions
        (SemanticTokensOptions.create ~legend:Semantic_highlighting.legend ~full ())
    in
    let positionEncoding =
      let open Option.O in
      let* general = client_capabilities.general in
      let* options = general.positionEncodings in
      List.find_map
        ([ UTF8; UTF16 ] : PositionEncodingKind.t list)
        ~f:(fun encoding ->
          Option.some_if (List.mem options ~equal:Poly.equal encoding) encoding)
    in
    ServerCapabilities.create
      ~callHierarchyProvider:(`Bool true)
      ~textDocumentSync
      ~hoverProvider:(`Bool true)
      ~declarationProvider:(`Bool true)
      ~definitionProvider:(`Bool true)
      ~typeDefinitionProvider:(`Bool true)
      ~completionProvider
      ~signatureHelpProvider
      ~codeActionProvider
      ~codeLensProvider
      ~referencesProvider:(`Bool true)
      ~documentHighlightProvider:(`Bool true)
      ~documentFormattingProvider:(`Bool false)
      ~selectionRangeProvider:(`Bool true)
      ~documentSymbolProvider:(`Bool true)
      ~workspaceSymbolProvider:(`Bool true)
      ~foldingRangeProvider:(`Bool true)
      ~semanticTokensProvider
      ~experimental
      ~renameProvider
      ~inlayHintProvider:(`Bool true)
      ~workspace
      ~executeCommandProvider
      ~colorProvider:(`Bool true)
      ?positionEncoding
      ()
  in
  let serverInfo =
    let version = Version.get () in
    InitializeResult.create_serverInfo ~name:"ocamllsp" ~version ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()
;;

let ocamlmerlin_reason = "ocamlmerlin-reason"

(** The debounce flag controls whether to include delay (250ms with default config). This
    delay appears to have been implemented to avoid spamming updates while typing, but
    shouldn't be necessary on file open/save. *)
let set_diagnostics ?(debounce = false) ~(log_info : Log_info.t) state doc =
  let diagnostics = State.diagnostics state in
  let detached = state.detached in
  let uri = Document.uri doc in
  match Document.kind doc with
  | `Other -> Fiber.return ()
  | `Merlin merlin ->
    let send_with_debounce ~update_diagnostics =
      let log_info = Log_info.update_action ~suffix:"-sending" log_info in
      let send () =
        Ocaml_lsp_logging.with_fiber_logging
          ~f:(fun () ->
            let* merlin_diagnostics = update_diagnostics () in
            Diagnostics.send diagnostics uri ~merlin_diagnostics)
          log_info
      in
      match debounce with
      | false -> send ()
      | true ->
        let log_info = Log_info.update_action ~suffix:"-waiting" log_info in
        task_if_running detached ~f:(fun () ->
          Ocaml_lsp_logging.with_fiber_logging
            ~f:(fun () ->
              let timer = Document.Merlin.timer merlin in
              let* () = Lev_fiber.Timer.Wheel.cancel timer in
              let* () = Lev_fiber.Timer.Wheel.reset timer in
              let* res = Lev_fiber.Timer.Wheel.await timer in
              match res with
              | `Cancelled -> Fiber.return ()
              | `Ok -> send ())
            log_info)
    in
    (match Document.syntax doc with
     | Dune | Cram | Menhir | Ocamllex -> Fiber.return ()
     | Reason when Option.is_none (Ocaml_lsp_stdune.Bin.which ocamlmerlin_reason) ->
       let update_diagnostics () =
         let no_reason_merlin =
           let message =
             `String
               (sprintf "Could not detect %s. Please install reason" ocamlmerlin_reason)
           in
           Diagnostic.create
             ~source:Diagnostics.ocamllsp_source
             ~range:Range.first_line
             ~message
             ()
         in
         Fiber.return [ no_reason_merlin ]
       in
       send_with_debounce ~update_diagnostics
     | Reason | Ocaml ->
       let update_diagnostics () =
         Diagnostics.merlin_diagnostics_for_file ~log_info diagnostics merlin
       in
       send_with_debounce ~update_diagnostics)
;;

let init_logging state =
  let open Async in
  if Core.am_running_test
  then return ()
  else (
    let editor, editor_version = State.get_editor state in
    let%bind.Deferred logging_session =
      Ocaml_lsp_logging.init
        ~deployment_stage:(State.stage state)
        ~editor
        ~editor_version
        ~worker_name:state.worker_name
    in
    state.logging_session <- Some logging_session;
    return ())
;;

let refresh_merlin_diagnostics_if_build_finished (state : State.t) diagnostics progress =
  match progress with
  | Drpc.Progress.Success | Failed | Interrupted ->
    state.event_index <- state.event_index + 1;
    (* We suspect that this function is responsible for a number of observed cases of lsp
       slowness, because when someone has tons of open documents and a build completes,
       refreshing diagnostics for all of them backs up merlin. We therefore add a delay
       between refreshing each document to allow other work to interleave with the
       refreshes. We also order the documents according to a heuristic (see below). *)
    let docs = Document_store.docs_to_iter state.store in
    let log_info =
      Log_info.create
        state.logging_session
        ~event_index:state.event_index
        ~action:"event:buildCompletion/refreshDiagnostics"
        ~other_uris:(List.map docs ~f:Document.uri)
        ()
    in
    (* Our primary priority is first refreshing all files whose diagnostics changed in the
       build. Our secondary priority is refreshing files that were interacted with more
       recently. *)
    let updated_dune_uris = Diagnostics.updated_dune_uris diagnostics in
    Diagnostics.clear_updated_dune_uris diagnostics;
    let timestamp (doc : Document.t) =
      Document.uri doc
      |> Document_store.last_used state.store
      |> Option.value ~default:Core.Time_ns.epoch
    in
    let compare doc1 doc2 =
      let updated doc = List.mem updated_dune_uris (Document.uri doc) ~equal:Uri.equal in
      (* [descending] puts the most recent time first *)
      let descending_time t1 t2 = Core.Time_ns.compare t2 t1 in
      Base.Comparable.lexicographic
        [ Base.Comparable.lift Base.Bool.descending ~f:updated
        ; Base.Comparable.lift descending_time ~f:timestamp
        ]
        doc1
        doc2
    in
    Document_store.sequential_iter ~compare state.store ~f:(fun doc ->
      match Document.kind doc with
      | `Other -> Fiber.return ()
      | `Merlin merlin ->
        (* Sleep for as long as takes to refresh the document so that we don't spend more
           than 50% of the time refreshing docs and give merlin a chance to do other work. *)
        let start = Core.Time_ns.now () in
        let* merlin_diagnostics =
          Diagnostics.merlin_diagnostics_for_file ~log_info diagnostics merlin
        in
        let end_ = Core.Time_ns.now () in
        let diff = Core.Time_ns.diff end_ start |> Core.Time_ns.Span.to_sec in
        let uri = Document.uri doc in
        let* () = Diagnostics.send diagnostics uri ~merlin_diagnostics in
        Lev_fiber.Timer.sleepf diff)
  | Drpc.Progress.Waiting | Drpc.Progress.In_progress _ -> Fiber.return ()
;;

(** Helper function to subscribe to updates from the given sub, calling f on each update.
    Loops until the dune instance is closed, unless there's an error creating the pipe. *)
let iterate_over_dune_updates dune sub ~f =
  let* await_dune_notification_or_error = Dune_subscriptions.subscribe dune sub in
  match await_dune_notification_or_error with
  | Ok await_dune_notification ->
    Fiber.repeat_while ~init:() ~f:(fun () ->
      let* update = await_dune_notification () in
      match update with
      | `Eof -> Fiber.return None
      | `Ok dune_diagnostics ->
        let+ () = f dune_diagnostics in
        Some ())
    >>> Fiber.return (Ok ())
  | Error e -> Fiber.return (Error e)
;;

let on_initialize server (ip : InitializeParams.t) =
  let client_name, _ =
    Option.map ip.clientInfo ~f:InitializeParams.get_editor
    |> Option.value ~default:("", "")
  in
  let state : State.t = Server.state server in
  let workspaces = Workspaces.create ip in
  let diagnostics =
    let which_diagnostics = Configuration.which_diagnostics state.configuration in
    let shorten_merlin_diagnostics =
      Configuration.shorten_merlin_diagnostics state.configuration
    in
    Diagnostics.create
      ~which_diagnostics
      ~shorten_merlin_diagnostics
      ~client_name
      (let open Option.O in
       let* td = ip.capabilities.textDocument in
       td.publishDiagnostics)
      (fun diagnostics ->
        let state = Server.state server in
        task_if_running state.detached ~f:(fun () ->
          let batch = Server.Batch.create server in
          Server.Batch.notification batch (PublishDiagnostics diagnostics);
          Server.Batch.submit batch))
  in
  let dune_subscriptions = Dune_subscriptions.create () in
  let create_dune_subscriptions () =
    Fiber.fork_and_join
      (fun () ->
        iterate_over_dune_updates dune_subscriptions Drpc.Sub.progress ~f:(fun progress ->
          refresh_merlin_diagnostics_if_build_finished
            (Server.state server)
            diagnostics
            progress))
      (fun () ->
        iterate_over_dune_updates
          dune_subscriptions
          (Drpc.Sub.diagnostic : Drpc.Diagnostic.Event.t list Drpc.Sub.t)
          ~f:(fun dune_diagnostics ->
            Dune_subscriptions.set_dune_diagnostics diagnostics dune_diagnostics;
            Fiber.return ()))
  in
  let cancel_manage_dune_connection = Fiber.Cancel.create () in
  let manage_dune_connection () =
    let* wheel = Lev_fiber.Timer.Wheel.create ~delay:10. in
    let rec loop () =
      let* dune_is_running = Dune_subscriptions.is_dune_running () in
      if dune_is_running
      then
        let* (_ : (unit, Base.Error.t) result * (unit, Base.Error.t) result) =
          create_dune_subscriptions ()
        in
        (* If we get past the [let*], the dune build we were connected to most likely got
           killed/restarted, so we just try to connect again as normal. We make sure to
           sleep first so that we don't end up busy-waiting here if [is_dune_running]
           succeeds but the subscription somehow fails to connect *)
        sleep_then_retry ()
      else sleep_then_retry ()
    and sleep_then_retry () =
      let* sleep = Lev_fiber.Timer.Wheel.task wheel in
      let* (), outcome =
        Fiber.Cancel.with_handler
          cancel_manage_dune_connection
          (fun () ->
            let* outcome = Lev_fiber.Timer.Wheel.await sleep in
            match outcome with
            | `Ok -> loop ()
            | `Cancelled -> Fiber.return ())
          ~on_cancel:(fun () -> Lev_fiber.Timer.Wheel.cancel sleep)
      in
      (* We don't care if it got canceled, because it means we're shutting down anyway. *)
      ignore (outcome : unit Fiber.Cancel.outcome);
      Fiber.return ()
    in
    loop ()
  in
  let* () = Fiber.Pool.task state.detached ~f:manage_dune_connection in
  let initialize_info = initialize_info ip.capabilities in
  let state =
    let position_encoding =
      match initialize_info.capabilities.positionEncoding with
      | None | Some UTF16 -> `UTF16
      | Some UTF8 -> `UTF8
      | Some UTF32 | Some (Other _) -> assert false
    in
    State.initialize
      state
      ~position_encoding
      ~dune_subscriptions
      ~manage_dune_connection:cancel_manage_dune_connection
      ip
      workspaces
      diagnostics
  in
  let state =
    match ip.trace with
    | None -> state
    | Some trace -> { state with trace }
  in
  let* () = Fiber_async.fiber_of_deferred (init_logging state) in
  let () =
    Ocaml_lsp_logging.async_log_global_add_tags ~tags:[ "program", "ocaml-lsp-server" ]
  in
  let resp =
    match ip.capabilities.textDocument with
    | Some
        { TextDocumentClientCapabilities.synchronization =
            Some { TextDocumentSyncClientCapabilities.dynamicRegistration = Some true; _ }
        ; _
        } ->
      Reply.later (fun send ->
        let* () = send initialize_info in
        let register =
          RegistrationParams.create
            ~registrations:
              (let make method_ =
                 let id = "ocamllsp-cram-dune-files/" ^ method_ in
                 (* TODO not nice to copy paste *)
                 let registerOptions =
                   let documentSelector =
                     [ "cram"; "dune"; "dune-project"; "dune-workspace" ]
                     |> List.map ~f:(fun language ->
                       `TextDocumentFilter (TextDocumentFilter.create ~language ()))
                   in
                   TextDocumentRegistrationOptions.create ~documentSelector ()
                   |> TextDocumentRegistrationOptions.yojson_of_t
                 in
                 Registration.create ~id ~method_ ~registerOptions ()
               in
               [ make "textDocument/didOpen"; make "textDocument/didClose" ])
        in
        Server.request server (Server_request.ClientRegisterCapability register))
    | _ -> Reply.now initialize_info
  in
  Fiber.return (resp, state)
;;

module Formatter = struct
  let jsonrpc_error (e : Ocamlformat.error) =
    let message = Ocamlformat.message e in
    let code : Jsonrpc.Response.Error.Code.t =
      match e with
      | Unsupported_syntax _ | Unknown_extension _ | Missing_binary _ -> InvalidRequest
      | Unexpected_result _ -> InternalError
    in
    make_error ~code ~message ()
  ;;

  let run rpc doc =
    match Document.kind doc with
    | `Merlin _ ->
      let* res =
        let* cancel = Server.cancel_token () in
        Ocamlformat.run doc cancel
      in
      (match res with
       | Ok result -> Fiber.return (Some result)
       | Error e ->
         let+ () =
           let state : State.t = Server.state rpc in
           let msg =
             let message = Ocamlformat.message e in
             ShowMessageParams.create ~message ~type_:Warning
           in
           task_if_running state.detached ~f:(fun () ->
             Server.notification rpc (ShowMessage msg))
         in
         Jsonrpc.Response.Error.raise (jsonrpc_error e))
    | `Other -> Fiber.return None
  ;;
end

let text_document_lens
  ~log_info
  (state : State.t)
  { CodeLensParams.textDocument = { uri }; _ }
  =
  let store = state.store in
  let doc = Document_store.get store uri in
  match Document.kind doc with
  | `Other -> Fiber.return (Some [])
  | `Merlin m when Document.Merlin.kind m = Intf -> Fiber.return (Some [])
  | `Merlin merlin_doc ->
    let+ outline =
      Document.Merlin.dispatch_exn
        ~log_info
        ~priority:Priorities.code_lens
        merlin_doc
        (Outline { include_types = true })
    in
    let rec symbol_info_of_outline_item (item : Query_protocol.item) =
      let children = List.concat_map item.children ~f:symbol_info_of_outline_item in
      match item.outline_type with
      | None -> children
      | Some typ ->
        let loc = item.location in
        let info =
          let range = Range.of_loc loc in
          let command = Command.create ~title:typ ~command:"" () in
          CodeLens.create ~range ~command ()
        in
        info :: children
    in
    Some (List.concat_map ~f:symbol_info_of_outline_item outline)
;;

let selection_range
  ~log_info
  (state : State.t)
  { SelectionRangeParams.textDocument = { uri }; positions; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return (Some [])
  | `Merlin merlin ->
    let selection_range_of_shapes
      (cursor_position : Position.t)
      (shapes : Query_protocol.shape list)
      : SelectionRange.t option
      =
      let rec ranges_of_shape parent (s : Query_protocol.shape) =
        let selectionRange =
          let range = Range.of_loc s.shape_loc in
          { SelectionRange.range; parent }
        in
        match s.shape_sub with
        | [] -> [ selectionRange ]
        | xs -> List.concat_map xs ~f:(ranges_of_shape (Some selectionRange))
      in
      (* try to find the nearest range inside first, then outside *)
      let nearest_range =
        let ranges = List.concat_map ~f:(ranges_of_shape None) shapes in
        List.min_elt ranges ~compare:(fun r1 r2 ->
          let inc (r : SelectionRange.t) =
            Position.compare_inclusion cursor_position r.range
          in
          match inc r1, inc r2 with
          | `Outside x, `Outside y -> Position.compare x y
          | `Outside _, `Inside -> 1
          | `Inside, `Outside _ -> -1
          | `Inside, `Inside -> Range.compare_size r1.range r2.range)
      in
      nearest_range
    in
    let+ ranges =
      Fiber.sequential_map positions ~f:(fun p ->
        let+ shapes =
          Document.Merlin.dispatch_exn
            ~log_info
            ~priority:Priorities.selection_range
            merlin
            (Shape (Position.logical p))
        in
        selection_range_of_shapes p shapes)
    in
    Some (List.filter_opt ranges)
;;

let highlight
  ~log_info
  (state : State.t)
  { DocumentHighlightParams.textDocument = { uri }; position; _ }
  =
  let store = state.store in
  let doc = Document_store.get store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin m ->
    let+ occurrences, _synced =
      Document.Merlin.dispatch_exn
        ~log_info
        ~priority:Priorities.highlight
        m
        (Occurrences (`Ident_at (Position.logical position), `Buffer))
    in
    let lsp_locs =
      List.filter_map occurrences ~f:(fun { loc; is_stale = _ } ->
        let range = Range.of_loc loc in
        (* filter out multi-line ranges, since those are very noisy and happen a lot with
           certain PPXs *)
        match range.start.line = range.end_.line with
        | true ->
          (* using the default kind as we are lacking info to make a difference between
             assignment and usage. *)
          Some (DocumentHighlight.create ~range ~kind:DocumentHighlightKind.Text ())
        | false -> None)
    in
    Some lsp_locs
;;

let document_symbol ~log_info (state : State.t) uri =
  let doc =
    let store = state.store in
    Document_store.get store uri
  in
  Document_symbol.run ~log_info state doc uri
;;

let unkown_request_uri ~(meth : string) ~(params : Jsonrpc.Structured.t option) =
  let getters =
    [ Req_switch_impl_intf.meth, Req_switch_impl_intf.get_doc_id
    ; Req_infer_intf.meth, Req_infer_intf.get_doc_id
    ; Req_typed_holes.meth, Req_typed_holes.get_doc_id
    ; Req_typed_holes.jump, Req_typed_holes.get_doc_id
    ; Req_merlin_call_compatible.meth, Req_merlin_call_compatible.get_doc_id
    ; Req_type_enclosing.meth, Req_type_enclosing.get_doc_id
    ; Req_wrapping_ast_node.meth, Req_wrapping_ast_node.get_doc_id
    ; ( Semantic_highlighting.Debug.meth_request_full
      , Semantic_highlighting.Debug.get_doc_id )
    ; Req_hover_extended.meth, Req_hover_extended.get_doc_id
    ; Req_complete_prefix_at_pos.meth, Req_complete_prefix_at_pos.get_doc_id
    ]
  in
  let%bind.Option getter = List.Assoc.find ~equal:String.equal getters meth in
  getter ~params
;;

let unkown_request_position ~(meth : string) ~(params : Jsonrpc.Structured.t option) =
  let no_pos ~params:_ = None in
  let getters =
    [ Req_typed_holes.jump, Req_typed_holes.get_pos
    ; Req_type_enclosing.meth, Req_type_enclosing.get_pos
    ; Req_wrapping_ast_node.meth, Req_wrapping_ast_node.get_pos
    ; Req_hover_extended.meth, Req_hover_extended.get_pos
    ; Req_switch_impl_intf.meth, no_pos
    ; Req_infer_intf.meth, no_pos
    ; Req_typed_holes.meth, no_pos
    ; Req_merlin_call_compatible.meth, no_pos
    ; Semantic_highlighting.Debug.meth_request_full, no_pos
    ; Req_complete_prefix_at_pos.meth, Req_complete_prefix_at_pos.get_pos
    ]
  in
  let%bind.Option getter = List.Assoc.find ~equal:String.equal getters meth in
  getter ~params
;;

let get_text store uri = Document_store.get_opt store uri |> Option.map ~f:Document.text

let on_request
  : type resp.
    State.t Server.t
    -> resp Client_request.t
    -> request_time:Core.Time_ns.t option
    -> event_index:int option
    -> (resp Reply.t * State.t) Fiber.t
  =
  fun server req ~request_time ~event_index ->
  let rpc = server in
  let state : State.t = Server.state server in
  let event_index = State.decide_event_index state ~event_index in
  let primary_uri = Client_request.primary_uri req ~fallback:unkown_request_uri in
  let other_uris = Client_request.other_uris req in
  let store = state.store in
  Option.iter primary_uri ~f:(Document_store.update_last_used store);
  let text = Option.bind ~f:(get_text store) primary_uri in
  let position = Client_request.position req ~fallback:unkown_request_position in
  let log_info =
    Log_info.create
      state.logging_session
      ~event_index
      ~action:[%string "%{Client_request.method_ req}"]
      ?primary_uri
      ?other_uris
      ?text
      ?position
      ?request_time
      ()
  in
  Ocaml_lsp_logging.log_event ~category:"request" log_info;
  let now res =
    Fiber.return
      (Ocaml_lsp_logging.with_logging ~f:(fun () -> Reply.now res) log_info, state)
  in
  let later f req =
    Fiber.return
      ( Reply.later (fun k ->
          let* resp =
            Ocaml_lsp_logging.with_fiber_logging ~f:(fun () -> f state req) log_info
          in
          k resp)
      , state )
  in
  match req with
  | Client_request.UnknownRequest { meth; params } ->
    (match
       [ ( Req_switch_impl_intf.meth
         , fun ~params state ->
             Fiber.of_thunk (fun () ->
               Fiber.return (Req_switch_impl_intf.on_request ~params state)) )
       ; Req_infer_intf.meth, Req_infer_intf.on_request ~log_info
       ; Req_typed_holes.meth, Req_typed_holes.on_request ~log_info
       ; Req_typed_holes.jump, Req_typed_holes.on_jump_request ~log_info
       ; ( Req_merlin_call_compatible.meth
         , Req_merlin_call_compatible.on_request ~log_info server )
       ; Req_type_enclosing.meth, Req_type_enclosing.on_request ~log_info
       ; Req_get_documentation.meth, Req_get_documentation.on_request ~log_info
       ; Req_wrapping_ast_node.meth, Req_wrapping_ast_node.on_request ~log_info
       ; ( Semantic_highlighting.Debug.meth_request_full
         , Semantic_highlighting.Debug.on_request_full ~log_info )
       ; ( Req_hover_extended.meth
         , fun ~params _ -> Req_hover_extended.on_request ~log_info ~params rpc )
       ; Req_complete_prefix_at_pos.meth, Req_complete_prefix_at_pos.on_request ~log_info
       ]
       |> fun lst -> List.Assoc.find ~equal:String.equal lst meth
     with
     | None ->
       Jsonrpc.Response.Error.raise
         (make_error
            ~code:MethodNotFound
            ~message:"Unknown method"
            ~data:(`Assoc [ "method", `String meth ])
            ())
     | Some handler ->
       Fiber.return
         ( Reply.later (fun send ->
             let* res =
               Ocaml_lsp_logging.with_fiber_logging
                 ~f:(fun () -> handler ~params state)
                 log_info
             in
             send res)
         , state ))
  | Initialize ip -> on_initialize server ip
  | DebugTextDocumentGet { textDocument = { uri }; position = _ } ->
    (match Document_store.get_opt store uri with
     | None -> now None
     | Some doc ->
       let text = Msource.text (Document.source doc) in
       now (Some text))
  | DebugEcho params -> now params
  | Shutdown -> Fiber.return (Reply.now (), state)
  | WorkspaceSymbol req ->
    later (fun state () -> Workspace_symbol.run server state req) ()
  | CodeActionResolve ca -> now ca
  | ExecuteCommand command ->
    if String.equal command.command Merlin_config_command.command_name
    then
      later
        (fun _state server ->
          let+ () = Merlin_config_command.command_run server store in
          `Null)
        server
    else if String.equal command.command Document_text_command.command_name
    then
      later
        (fun state server ->
          let store = state.store in
          let+ () = Document_text_command.command_run server store command.arguments in
          `Null)
        server
    else if String.equal command.command view_metrics_command_name
    then later (fun _state server -> view_metrics server) server
    else if String.equal command.command Action_open_related.command_name
    then
      later (fun _state server -> Action_open_related.command_run server command) server
    else
      Jsonrpc.Response.Error.raise
        (make_error
           ~code:RequestFailed
           ~message:"Unknown command"
           ~data:(`String command.command)
           ())
  | CompletionItemResolve ci ->
    later
      (fun state () ->
        let markdown =
          ClientCapabilities.markdown_support
            (State.client_capabilities state)
            ~field:(fun d ->
              let open Option.O in
              let+ completion = d.completion in
              let* completion_item = completion.completionItem in
              completion_item.documentationFormat)
        in
        let resolve = Compl.Resolve.of_completion_item ci in
        match resolve with
        | None -> Fiber.return ci
        | Some resolve ->
          let doc =
            let uri = Compl.Resolve.uri resolve in
            Document_store.get state.store uri
          in
          (match Document.kind doc with
           | `Other -> Fiber.return ci
           | `Merlin doc ->
             Compl.resolve
               ~log_info
               doc
               ci
               resolve
               (Document.Merlin.doc_comment ~log_info ~priority:Priorities.completion)
               ~markdown))
      ()
  | CodeAction params ->
    Ocaml_lsp_logging.with_fiber_logging
      ~f:(fun () -> Code_actions.compute ~log_info server params)
      log_info
  | InlayHint params ->
    later (fun state () -> Inlay_hints.compute ~log_info state params) ()
  | TextDocumentColor (request : Lsp.Types.DocumentColorParams.t) ->
    Ocaml_lsp_logging.with_fiber_logging
      ~f:(fun () -> Color.compute ~log_info server request)
      log_info
  | TextDocumentColorPresentation _ -> now []
  | TextDocumentHover req ->
    later (fun (_ : State.t) () -> Hover_req.handle ~log_info rpc req) ()
  | TextDocumentReferences req ->
    later (fun (_ : State.t) () -> References_req.handle ~log_info rpc req) ()
  | TextDocumentCodeLensResolve codeLens -> now codeLens
  | TextDocumentCodeLens req ->
    (match state.configuration.data.codelens with
     | Some { enable = true } -> later (text_document_lens ~log_info) req
     | _ -> now (Some []))
  | TextDocumentHighlight req -> later (highlight ~log_info) req
  | DocumentSymbol { textDocument = { uri }; _ } -> later (document_symbol ~log_info) uri
  | TextDocumentDeclaration { textDocument = { uri }; position } ->
    later
      (fun _ () ->
        Definition_query.run
          ~log_info
          ~priority:Priorities.declaration
          Declaration
          server
          uri
          position)
      ()
  | TextDocumentDefinition { textDocument = { uri }; position; _ } ->
    later
      (fun _ () ->
        Definition_query.run
          ~log_info
          ~priority:Priorities.definition
          Definition
          server
          uri
          position)
      ()
  | TextDocumentTypeDefinition { textDocument = { uri }; position; _ } ->
    later
      (fun _ () ->
        Definition_query.run
          ~log_info
          ~priority:Priorities.type_definition
          Type_definition
          server
          uri
          position)
      ()
  | TextDocumentCompletion params ->
    later (fun _ () -> Compl.complete ~log_info state params) ()
  | TextDocumentPrepareRename req -> later (Rename.prepare_rename ~log_info) req
  | TextDocumentRename req -> later (Rename.rename ~log_info) req
  | TextDocumentFoldingRange req -> later (Folding_range.compute ~log_info) req
  | SignatureHelp req -> later (Signature_help.run ~log_info) req
  | TextDocumentLinkResolve l -> now l
  | TextDocumentLink _ -> now None
  | WillSaveWaitUntilTextDocument _ -> now None
  | TextDocumentFormatting { textDocument = { uri }; options = _; _ } ->
    later
      (fun _ () ->
        let doc = Document_store.get store uri in
        Formatter.run rpc doc)
      ()
  | TextDocumentOnTypeFormatting _ -> now None
  | SelectionRange req -> later (selection_range ~log_info) req
  | TextDocumentImplementation _ -> not_supported ()
  | SemanticTokensFull p -> later (Semantic_highlighting.on_request_full ~log_info) p
  | SemanticTokensDelta p ->
    later (Semantic_highlighting.on_request_full_delta ~log_info) p
  | TextDocumentMoniker _ -> not_supported ()
  | TextDocumentPrepareCallHierarchy req ->
    later (fun (_ : State.t) () -> Call_hierarchy.handle_prepare ~log_info rpc req) ()
  | TextDocumentRangeFormatting _ -> not_supported ()
  | CallHierarchyIncomingCalls req ->
    later (fun (_ : State.t) () -> Call_hierarchy.handle_incoming ~log_info rpc req) ()
  | CallHierarchyOutgoingCalls req ->
    later (fun (_ : State.t) () -> Call_hierarchy.handle_outgoing ~log_info rpc req) ()
  | SemanticTokensRange _ -> not_supported ()
  | LinkedEditingRange _ -> not_supported ()
  | WillCreateFiles _ -> not_supported ()
  | WillRenameFiles _ -> not_supported ()
  | WillDeleteFiles _ -> not_supported ()
  | InlayHintResolve _ -> not_supported ()
  | TextDocumentDiagnostic _ -> not_supported ()
  | TextDocumentInlineCompletion _ -> not_supported ()
  | TextDocumentInlineValue _ -> not_supported ()
  | WorkspaceSymbolResolve _ -> not_supported ()
  | WorkspaceDiagnostic _ -> not_supported ()
  | TextDocumentRangesFormatting _ -> not_supported ()
  | TextDocumentPrepareTypeHierarchy _ -> not_supported ()
  | TypeHierarchySupertypes _ -> not_supported ()
  | TypeHierarchySubtypes _ -> not_supported ()
;;

(* Compute the relative directory path from a file URI, relative to the workspace root.
   This is used for build-watch subscriptions. *)
let relative_dir_of_uri (state : State.t) uri =
  let file_path = Uri.drop_query uri |> Uri.to_path in
  let dir = Filename.dirname file_path in
  let workspace_root_path =
    match state.init with
    | Initialized { params = { rootUri; _ }; _ } ->
      Option.map rootUri ~f:(fun root -> Uri.to_path root)
    | Uninitialized -> None
  in
  match workspace_root_path with
  | Some root when String.is_prefix dir ~prefix:root ->
    let relative =
      String.chop_prefix_if_exists dir ~prefix:root
      |> String.chop_prefix_if_exists ~prefix:"/"
    in
    if String.is_empty relative then "." else relative
  | _ -> dir
;;

let on_notification
  server
  (notification : Client_notification.t)
  ~(event_index : int option)
  : (State.t * Notify.t option) Fiber.t
  =
  let state : State.t = Server.state server in
  let event_index = State.decide_event_index state ~event_index in
  let store = state.store in
  let primary_uri = Client_notification.primary_uri notification in
  let other_uris = Client_notification.other_uris notification in
  Option.iter primary_uri ~f:(Document_store.update_last_used store);
  let log_info =
    Log_info.create
      state.logging_session
      ~event_index
      ~action:[%string "%{Client_notification.method_ notification}"]
      ?primary_uri
      ?other_uris
      ()
  in
  Ocaml_lsp_logging.log_event ~category:"notification" log_info;
  Ocaml_lsp_logging.with_fiber_logging log_info ~f:(fun () ->
    match notification with
    | TextDocumentDidOpen params ->
      let* doc =
        let position_encoding = State.position_encoding state in
        Document.make
          ~position_encoding
          (State.wheel state)
          state.merlin_config
          state.merlin
          params
      in
      let* () = Document_store.open_document store doc in
      let* () = set_diagnostics ~log_info state doc in
      (* Register the file's directory for build-watch subscription. The VSCode AIDE
         integration is spammy about sending didOpen messages for unimportant files, so we
         only do this on [ocamllsp/humanDidOpen] instead. *)
      let* () =
        match State.dune_subscriptions state with
        | Some dune_subscriptions
          when (not (State.editor_is_vscode state))
               && Configuration.dune_build_on_open state.configuration ->
          let dir = relative_dir_of_uri state params.textDocument.uri in
          Dune_subscriptions.register_open_dir dune_subscriptions ~dir
        | _ -> Fiber.return ()
      in
      (* vim does not send a root uri during initialization so we extract the feature id
         from the first file opened. *)
      let feature_id =
        match state.feature_id with
        | None -> Ocaml_lsp_uri.feature_id params.textDocument.uri
        | Some feature_id -> Some feature_id
      in
      let state = { state with feature_id } in
      Fiber.return (state, None)
    | TextDocumentDidClose { textDocument = { uri } } ->
      let* () = Document_store.close_document store uri in
      let diagnostics = State.diagnostics state in
      let* () = Diagnostics.send diagnostics uri ~merlin_diagnostics:[] in
      (* Unregister the file's directory from build-watch subscription. On vscode, we skip
         this because we use [ocamllsp/humanDidClose] instead. *)
      let+ () =
        match State.dune_subscriptions state with
        | Some dune_subscriptions when not (State.editor_is_vscode state) ->
          let dir = relative_dir_of_uri state uri in
          Dune_subscriptions.unregister_open_dir dune_subscriptions ~dir
        | _ -> Fiber.return ()
      in
      state, None
    | TextDocumentDidChange { textDocument = { uri; version }; contentChanges } ->
      let doc =
        Document_store.change_document store uri ~f:(fun prev_doc ->
          Document.update_text ~version prev_doc contentChanges)
      in
      let+ () = set_diagnostics ~log_info ~debounce:true state doc in
      state, None
    | CancelRequest _ -> Fiber.return (state, None)
    | ChangeConfiguration req ->
      let prev_which_diagnostics = state.configuration.data.which_diagnostics in
      let prev_shorten_merlin =
        match state.configuration.data.shorten_merlin_diagnostics with
        | None -> false
        | Some { enable } -> enable
      in
      let* configuration = Configuration.update state.configuration req in
      let which_diagnostics = Configuration.which_diagnostics configuration in
      let shorten_merlin_diagnostics =
        Configuration.shorten_merlin_diagnostics configuration
      in
      let shorten_merlin_changed = prev_shorten_merlin <> shorten_merlin_diagnostics in
      let which_diagnostics_changed =
        match prev_which_diagnostics, configuration.data.which_diagnostics with
        | None, None -> false
        | Some which1, Some which2 ->
          not (Config_data.WhichDiagnostics.equal which1 which2)
        | None, Some _ | Some _, None -> true
      in
      let () =
        Diagnostics.set_which_diagnostics ~which_diagnostics (State.diagnostics state)
      in
      let () =
        Diagnostics.set_shorten_merlin_diagnostics
          ~shorten_merlin_diagnostics
          (State.diagnostics state)
      in
      let state = { state with configuration } in
      if shorten_merlin_changed || which_diagnostics_changed
      then
        let+ () =
          Document_store.parallel_iter store ~f:(set_diagnostics ~log_info state)
        in
        state, None
      else Fiber.return (state, None)
    | DidSaveTextDocument { textDocument = { uri }; _ } ->
      let state = Server.state server in
      (match Document_store.get_opt state.store uri with
       | None ->
         (Log.log ~section:"on receive DidSaveTextDocument"
          @@ fun () -> Log.msg "saved document is not in the store" []);
         Fiber.return (state, None)
       | Some doc ->
         let+ () = set_diagnostics ~log_info state doc in
         state, None)
    | ChangeWorkspaceFolders change ->
      let state =
        State.modify_workspaces state ~f:(fun ws -> Workspaces.on_change ws change)
      in
      Fiber.return (state, None)
    | DidChangeWatchedFiles _
    | DidCreateFiles _
    | DidDeleteFiles _
    | DidRenameFiles _
    | WillSaveTextDocument _
    | Initialized
    | WorkDoneProgressCancel _
    | WorkDoneProgress _
    | NotebookDocumentDidOpen _
    | NotebookDocumentDidChange _
    | NotebookDocumentDidSave _
    | NotebookDocumentDidClose _
    | Exit -> Fiber.return (state, None)
    | SetTrace { value } -> Fiber.return ({ state with trace = value }, None)
    | CustomNotification (HumanDidOpen params) ->
      (* On vscode, this notification is sent when a human explicitly opens a file (as
         opposed to programmatic opens like go-to-definition). We use this to register a
         build-watch subscription for that directory. *)
      let+ () =
        match State.dune_subscriptions state with
        | Some dune_subscriptions
          when Configuration.dune_build_on_open state.configuration ->
          let dir = relative_dir_of_uri state params.textDocument.uri in
          Dune_subscriptions.register_open_dir dune_subscriptions ~dir
        | _ -> Fiber.return ()
      in
      state, None
    | CustomNotification (HumanDidClose params) ->
      (* On vscode, this notification is sent when a human explicitly closes a file. We
         use this to unregister the build-watch subscription for that directory. *)
      let+ () =
        match State.dune_subscriptions state with
        | Some dune_subscriptions ->
          let dir = relative_dir_of_uri state params.textDocument.uri in
          Dune_subscriptions.unregister_open_dir dune_subscriptions ~dir
        | None -> Fiber.return ()
      in
      state, None
    | UnknownNotification req ->
      let+ () =
        State.log_msg server ~type_:Error ~message:("Unknown notication " ^ req.method_)
      in
      state, None)
;;

let start stream ~stage ~worker_name =
  let detached = Fiber.Pool.create () in
  let server = Fdecl.create [%sexp_of: _] in
  let store = Document_store.make server detached in
  let handler =
    let on_request = { Server.Handler.on_request } in
    Server.Handler.make ~on_request ~on_notification ()
  in
  let ocamlformat_rpc = Ocamlformat_rpc.create () in
  let* configuration = Configuration.default stage in
  let wheel = Configuration.wheel configuration in
  let server_ref = ref None in
  let get_logging_session () =
    match !server_ref with
    | None -> None
    | Some (server : _ Server.t) ->
      let state : State.t = Server.state server in
      state.logging_session
  in
  let* merlin = Priority_lsp_executor.create ~get_logging_session in
  let server =
    let symbols_thread = Lazy_fiber.create Lev_fiber.Thread.create in
    Fdecl.set
      server
      (Server.make
         handler
         stream
         (State.create
            ~store
            ~merlin
            ~ocamlformat_rpc
            ~configuration
            ~detached
            ~symbols_thread
            ~wheel
            ~worker_name));
    Fdecl.get ~here:[%here] server
  in
  let state = Server.state server in
  server_ref := Some server;
  let with_log_errors what f =
    let+ (_ : (unit, unit) result) =
      Fiber_extensions.map_reduce_errors_first f ~on_error:(fun exn ->
        Format.eprintf "%s: %a@." what Exn_with_backtrace.pp_uncaught exn;
        Fiber.return ())
    in
    ()
  in
  let run_ocamlformat_rpc () =
    let* state = Ocamlformat_rpc.run ~logger:(State.log_msg server) ocamlformat_rpc in
    let message =
      match state with
      | Error `Binary_not_found ->
        Some
          "Unable to find 'ocamlformat-rpc' binary. Types on hover may not be \
           well-formatted. You need to install either 'ocamlformat' of version > 0.21.0 \
           or, otherwise, 'ocamlformat-rpc' package."
      | Error `Disabled | Ok () -> None
    in
    match message with
    | None -> Fiber.return ()
    | Some message ->
      let* (_ : InitializeParams.t) = Server.initialized server in
      let state = Server.state server in
      task_if_running state.detached ~f:(fun () ->
        let log = ShowMessageParams.create ~type_:Info ~message in
        Server.notification server (Server_notification.ShowMessage log))
  in
  let run () =
    Fiber.all_concurrently_unit
      [ with_log_errors "detached" (fun () -> Fiber.Pool.run detached)
      ; Lev_fiber.Timer.Wheel.run wheel
      ; with_log_errors "merlin" (fun () -> Merlin_config.DB.run state.merlin_config)
      ; Priority_lsp_executor.run_scheduler merlin
      ; (let* () = Server.start server in
         let finalize_dune =
           match (Server.state server).init with
           | Uninitialized -> Fiber.return ()
           | Initialized init ->
             Fiber.of_thunk (fun () ->
               Dune_subscriptions.close init.dune_subscriptions;
               Fiber.Cancel.fire init.manage_dune_connection)
         in
         let finalize_logging =
           match (Server.state server).logging_session with
           | None -> Fiber.return ()
           | Some session ->
             Fiber_async.fiber_of_deferred
               (let open Async in
                let%map _ = Ocaml_lsp_logging.close session in
                ())
         in
         let finalize =
           [ Document_store.close_all store
           ; Ocaml_lsp_fiber_shims.close_fiber_pool detached
           ; Ocamlformat_rpc.stop ocamlformat_rpc
           ; Lev_fiber.Timer.Wheel.stop wheel
           ; Merlin_config.DB.stop state.merlin_config
           ; Fiber.of_thunk (fun () -> Priority_lsp_executor.close merlin)
           ; finalize_dune
           ; finalize_logging
           ]
         in
         Fiber.all_concurrently_unit finalize)
      ; with_log_errors "ocamlformat-rpc" run_ocamlformat_rpc
      ]
  in
  let metrics = Metrics.create () in
  Metrics.with_metrics metrics run
;;

let socket sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let fd =
    Lev_fiber.Fd.create
      (Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0)
      (`Non_blocking false)
  in
  let* () = Lev_fiber.Socket.connect fd sockaddr in
  Lev_fiber.Io.create_rw fd
;;

let stream_of_channel : Lsp.Cli.Channel.t -> _ = function
  | Stdio ->
    let* stdin = Lev_fiber.Io.stdin in
    let+ stdout = Lev_fiber.Io.stdout in
    stdin, stdout
  | Pipe path ->
    if Sys.win32
    then (
      Format.eprintf "windows pipes are not supported";
      exit 1)
    else (
      let sockaddr = Unix.ADDR_UNIX path in
      socket sockaddr)
  | Socket port ->
    let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    socket sockaddr
;;

(* Merlin uses [Sys.command] to run preprocessors and ppxes. We provide an alternative
   version using the Spawn library for unixes.

   TODO: Currently PPX config is passed to Merlin in the form of a quoted shell command.
   The [prog_is_quoted] argument in Merlin's API is meant to allow supporting a way to
   launch ppx executables without using the shell.

   This will require additionnal changes of the API so there is no need to deal with the
   [prog_is_quoted] argument until this happen. *)
let run_in_directory ~prog ~prog_is_quoted:_ ~args ~cwd ?stdin ?stdout ?stderr () =
  (* Currently we assume that [prog] is always quoted and might contain arguments such as
     [-as-ppx]. This is due to the way Merlin gets its configuration. Thus we cannot rely
     on [Filename.quote_command]. *)
  let args = String.concat ~sep:" " @@ List.map ~f:Filename.quote args in
  let cmd = Format.sprintf "%s %s" prog args in
  let prog = "/bin/sh" in
  let argv = [ "sh"; "-c"; cmd ] in
  let stdin =
    match stdin with
    | Some file -> Unix.openfile file [ Unix.O_RDONLY ] 0o664
    | None -> Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o777
  in
  let stdout, should_close_stdout =
    match stdout with
    | Some file -> Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT ] 0o664, true
    | None ->
      (* Runned programs should never output to stdout since it is the channel used by LSP
         to communicate with the editor *)
      Unix.stderr, false
  in
  let stderr =
    Option.map stderr ~f:(fun file ->
      Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT ] 0o664)
  in
  let pid =
    let cwd : Spawn.Working_dir.t = Path cwd in
    Spawn.spawn ~cwd ~prog ~argv ~stdin ~stdout ?stderr ()
  in
  let _, status = Unix.waitpid [] pid in
  let res =
    match (status : Unix.process_status) with
    | WEXITED n -> n
    | WSIGNALED _ -> -1
    | WSTOPPED _ -> -1
  in
  Unix.close stdin;
  if should_close_stdout then Unix.close stdout;
  `Finished res
;;

let run_in_directory =
  (* Merlin has specific stubs for Windows, we reuse them *)
  let for_windows = !Merlin_utils.Std.System.run_in_directory in
  fun () -> if Sys.win32 then for_windows else run_in_directory
;;

let run channel ~dot_merlin ~stage ~worker_name =
  Merlin_utils.Lib_config.set_program_name "ocamllsp";
  Merlin_utils.Lib_config.System.set_run_in_directory (run_in_directory ());
  Merlin_config.dot_merlin := dot_merlin;
  (Unix.putenv [@alert "-unsafe_multidomain"])
    "__MERLIN_MASTER_PID"
    (string_of_int (Unix.getpid ()));
  let main =
    Fiber.of_thunk (fun () ->
      let* input, output = stream_of_channel channel in
      start (Lsp_fiber.Fiber_io.make input output) ~stage ~worker_name)
  in
  Fiber_async.deferred_of_fiber main ()
;;

module Compl = Compl
module Custom_request = Custom_request
module Dune_subscriptions = Dune_subscriptions
