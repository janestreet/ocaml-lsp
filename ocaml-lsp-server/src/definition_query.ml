open Import
open Fiber.O

let location_of_merlin_loc ~log_info uri ~cursor_position : _ -> (_, string) result
  = function
  | `File_not_found s ->
    let () =
      Ocaml_lsp_logging.log_event
        ~message:"Merlin returned Locate* result: File_not_found"
        log_info
    in
    Error (sprintf "File_not_found: %s" s)
  | `At_origin ->
    (* Return the cursor position if we're at the item's origin. VSCode picks this up and
       will issue a references request. *)
    let () =
      Ocaml_lsp_logging.log_event
        ~message:"Merlin returned Locate* result: At_origin"
        log_info
    in
    let range = { Range.start = cursor_position; end_ = cursor_position } in
    Ok (Some (`Location [ { Location.uri; range } ]))
  | `Builtin _ ->
    let () =
      Ocaml_lsp_logging.log_event
        ~message:"Merlin returned Locate* result: Builtin"
        log_info
    in
    Ok None
  | `Invalid_context ->
    let () =
      Ocaml_lsp_logging.log_event
        ~message:"Merlin returned Locate* result: Invalid_context"
        log_info
    in
    Ok None
  | `Not_found _ ->
    let () =
      Ocaml_lsp_logging.log_event
        ~message:"Merlin returned Locate* result: Not_found"
        log_info
    in
    Ok None
  | `Not_in_env _ ->
    let () =
      Ocaml_lsp_logging.log_event
        ~message:"Merlin returned Locate* result: Not_in_env"
        log_info
    in
    Ok None
  | `Found (path, lex_position) ->
    let () =
      Ocaml_lsp_logging.log_event
        ~message:"Merlin returned Locate* result: Found"
        log_info
    in
    Ok
      (match Position.of_lexical_position lex_position with
       | None ->
         let () =
           Ocaml_lsp_logging.log_event
             ~message:
               "Position.of_lexical_position returned None on merlin-returned \
                lex_position"
             ~info:
               [%message
                 ""
                   ~pos_fname:(lex_position.pos_fname : String.t)
                   ~pos_lnum:(lex_position.pos_lnum : Int.t)
                   ~pos_bol:(lex_position.pos_bol : Int.t)
                   ~pos_cnum:(lex_position.pos_cnum : Int.t)]
             log_info
         in
         None
       | Some position ->
         Some
           (let range = { Range.start = position; end_ = position } in
            let uri =
              match path with
              | None -> uri
              | Some path when DocumentUri.same_path ~path uri ->
                (* If we land in the same file, preserve the original uri. This is
                   relevant for the VSCode Fe-review sidebar, which uses the query field
                   of the uri, and needs it to recognize the review-diff. *)
                uri
              | Some path -> Uri.of_path path
            in
            let locs = [ { Location.uri; range } ] in
            `Location locs))
;;

let query_merlin ~log_info pipeline uri position target =
  let command =
    let pos = Position.logical position in
    match (target : Lsp.Types.Go_to_target.t) with
    | Definition -> Query_protocol.Locate (None, `ML, pos, None)
    | Declaration -> Query_protocol.Locate (None, `MLI, pos, None)
    | Type_definition -> Query_protocol.Locate_type pos
  in
  let result =
    try
      Query_commands.dispatch pipeline command
      |> location_of_merlin_loc ~log_info uri ~cursor_position:position
    with
    | Ocaml_typing.Magic_numbers.Cmi.Error err ->
      Error
        (Format.asprintf
           "%a\n\n\
            Hint: Make sure Dune has built the relevant target(s). This error is usually \
            due to a rebase picking up a new compiler version without re-building."
           Ocaml_typing.Magic_numbers.Cmi.report_error
           err)
  in
  match result with
  | Ok loc -> loc
  | Error err_msg ->
    let target = Lsp.Types.Go_to_target.to_string target in
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:Jsonrpc.Response.Error.Code.RequestFailed
         ~message:(sprintf "Request \"Jump to " ^ target ^ "\" failed.")
         ~data:(`String (sprintf "'Locate' query to merlin returned error: %s" err_msg))
         ())
;;

let query_remote_lsp ~log_info server state uri position target =
  match Remote_lsp.go_to_query ~log_info state uri position target with
  | Some fiber ->
    let* location = fiber >>| Option.map ~f:(fun location -> `Location [ location ]) in
    (match location with
     | None -> Fiber.return None
     | Some location ->
       (* This message appears on the bottom line in vim and emacs, and as a popup in the
          bottom right in vscode. *)
       let+ () =
         task_if_running state.detached ~f:(fun () ->
           Server.notification
             server
             (ShowMessage
                (ShowMessageParams.create
                   ~message:"remote-lsp: location may be out of date"
                   ~type_:Info)))
       in
       Some location)
  | None -> Fiber.return None
;;

let run ~log_info ~priority target server uri position =
  let state : State.t = Server.state server in
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other ->
    let () =
      Ocaml_lsp_logging.log_event
        ~message:"not a merlin document kind in the definition_query"
        log_info
    in
    Fiber.return None
  | `Merlin doc ->
    let* result =
      Document.Merlin.with_pipeline_exn ~log_info ~priority doc (fun pipeline ->
        (* Two important notes:
           1. If we are in a test, merlin will not be configured, but will still be able
              answer queries as it won't have to cross file boundaries. Querying
              remote-lsp is thus a) unnecessary and b) likely to fail as the test LSP was
              probably not first passed a file with a valid feature-id (e.g. "foo.ml").
           2. Using Fiber in a merlin pipeline causes assertions to fail in merlin, so we
              defer falling back to remote-lsp until exiting the pipeline. *)
        match
          Merlin_status.merlin_pipeline_can_answer_queries ~log_info pipeline
          || Core.am_running_test
        with
        | true -> Some (query_merlin ~log_info pipeline uri position target)
        | false ->
          let () =
            Ocaml_lsp_logging.log_event
              ~message:"Merlin cannot answer the definition_query"
              log_info
          in
          None)
    in
    (match result with
     | Some res -> Fiber.return res
     | None ->
       (* Fall back to remote-lsp if enabled and feasible. *)
       if State.should_fall_back state
       then query_remote_lsp ~log_info server state uri position target
       else Fiber.return None)
;;
