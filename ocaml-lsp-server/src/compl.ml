open Import
open Fiber.O

let priority = Priorities.completion

module Resolve = struct
  type t = CompletionParams.t

  let uri (t : t) = t.textDocument.uri
  let yojson_of_t = CompletionParams.yojson_of_t
  let t_of_yojson = CompletionParams.t_of_yojson
  let of_completion_item (ci : CompletionItem.t) = Option.map ci.data ~f:t_of_yojson
end

let completion_kind kind : CompletionItemKind.t option =
  match kind with
  | `Value -> Some Value
  | `Variant -> Some EnumMember
  | `Label -> Some Field
  | `Module -> Some Module
  | `Modtype -> Some Interface
  | `MethodCall -> Some Method
  | `Keyword -> Some Keyword
  | `Constructor -> Some Constructor
  | `Type -> Some TypeParameter
;;

let prefix_of_position ~(short_path : [ `None | `Prefix | `Suffix ]) source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let end_of_prefix =
      let (`Offset index) = Msource.get_offset source position in
      min (String.length text - 1) (index - 1)
    in
    let pos =
      (* clamp the length of a line to process at 500 chars, this is just a reasonable
         limit for regex performance *)
      max 0 (end_of_prefix - 500)
    in
    let reconstructed_prefix =
      Prefix_parser.parse ~pos ~len:(end_of_prefix + 1 - pos) text
      |> Option.value ~default:""
      (* We remove the whitespace because merlin expects no whitespace and it's
         semantically meaningless *)
      |> String.filter ~f:(fun x -> not (x = ' ' || x = '\n' || x = '\t'))
    in
    (match short_path, String.rsplit2 reconstructed_prefix ~on:'.' with
     | `None, _ -> reconstructed_prefix
     | `Prefix, Some (prefix, _) -> prefix ^ "."
     | `Prefix, None -> ""
     | `Suffix, Some (_, suffix) -> suffix
     | `Suffix, None -> reconstructed_prefix)
;;

let suffix_of_position source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let (`Offset index) = Msource.get_offset source position in
    let len = String.length text in
    if index >= len
    then ""
    else (
      let pos = index in
      let len =
        let ident_char = function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' | '_' -> true
          | _ -> false
        in
        let until =
          String.lfindi ~pos text ~f:(fun _idx c -> not (ident_char c))
          |> Option.value ~default:len
        in
        until - pos
      in
      String.sub text ~pos ~len)
;;

let reconstruct_ident source position =
  let prefix = prefix_of_position ~short_path:`None source position in
  let suffix = suffix_of_position source position in
  let ident = prefix ^ suffix in
  Option.some_if (ident <> "") ident
;;

let range_prefix (lsp_position : Position.t) prefix : Range.t =
  let start =
    let len = String.length prefix in
    let character = lsp_position.character - len in
    { lsp_position with character }
  in
  { Range.start; end_ = lsp_position }
;;

let sortText_of_index idx = Printf.sprintf "%04d" idx

module Complete_by_prefix = struct
  let completionItem_of_completion_entry
    idx
    (entry : Query_protocol.Compl.entry)
    ~compl_params
    ~range
    ~deprecated
    =
    let kind = completion_kind entry.kind in
    let textEdit = `TextEdit { TextEdit.range; newText = entry.name } in
    CompletionItem.create
      ~label:entry.name
      ?kind
      ~detail:entry.desc
      ?deprecated:(Option.some_if deprecated entry.deprecated)
        (* Without this field the client is not forced to respect the order provided by
           merlin. *)
      ~sortText:(sortText_of_index idx)
      ?data:compl_params
      ~textEdit
      ()
  ;;

  let dispatch_cmd ~prefix position pipeline =
    let complete = Query_protocol.Complete_prefix (prefix, position, [], false, true) in
    Query_commands.dispatch pipeline complete
  ;;

  let process_entries
    ~deprecated
    ~resolve
    ~suffix
    (state : State.t)
    doc
    pos
    parsetree
    (completion_entries : Query_protocol.Compl.entry list)
    =
    let source = Document.Merlin.source doc in
    let range =
      let logical_pos = Position.logical pos in
      range_prefix pos (prefix_of_position ~short_path:`Suffix source logical_pos)
    in
    (* we need to json-ify completion params to put them in completion item's [data] field
       to keep it across [textDocument/completion] and the following
       [completionItem/resolve] requests *)
    let compl_params =
      match resolve with
      | false -> None
      | true ->
        Some
          (let textDocument =
             TextDocumentIdentifier.create
               ~uri:(Document.uri (Document.Merlin.to_doc doc))
           in
           CompletionParams.create ~textDocument ~position:pos ()
           |> CompletionParams.yojson_of_t)
    in
    let ppx_completionItems =
      match parsetree with
      | `Interface _ -> []
      | `Implementation structure ->
        (* Catch-all so that these completions don't accidentally crash the OCaml LSP. *)
        (try Css_lsp.complete ~source ~pos structure with
         | _ -> [])
    in
    let filter_ppx_template_items completion_items =
      (* Filter out all the ppx_template-generated completions, such as
         [Option.value__'value_bits32'], by removing any containing a double underscore *)
      List.filter completion_items ~f:(fun ({ textEdit; _ } : CompletionItem.t) ->
        match textEdit with
        | Some (`TextEdit { newText; _ }) ->
          String.substr_index newText ~pattern:"__" |> Option.is_none
        | _ -> true)
    in
    let completion_items =
      ppx_completionItems
      @ List.mapi
          completion_entries
          ~f:(completionItem_of_completion_entry ~deprecated ~range ~compl_params)
    in
    let user_typed_double_underscore =
      String.substr_index suffix ~pattern:"__" |> Option.is_some
    in
    match state.configuration.data.filter_double_underscore with
    | Some { enable } when enable && not user_typed_double_underscore ->
      filter_ppx_template_items completion_items
    | None | Some _ -> completion_items
  ;;

  let process_dispatch_resp_squash
    ~deprecated
    ~resolve
    ~prefix
    ~suffix
    (state : State.t)
    doc
    pos
    parsetree
    (completion : Query_protocol.completions)
    =
    let completion_entries =
      (* Squash any function argument names into the completions list for the sake of LSP
         protocol compatibility. *)
      match completion.context with
      | `Unknown -> completion.entries
      | `Application { Query_protocol.Compl.labels; argument_type = _ } ->
        completion.entries
        @ List.map labels ~f:(fun (name, typ) ->
          let name =
            if String.is_prefix prefix ~prefix:"~" && String.is_prefix name ~prefix:"?"
            then "~" ^ String.chop_prefix_if_exists name ~prefix:"?"
            else name
          in
          { Query_protocol.Compl.name
          ; kind = `Label
          ; desc = typ
          ; info = ""
          ; deprecated = false (* TODO this is wrong *)
          ; ppx_template_generated = false
          })
    in
    process_entries
      ~deprecated
      ~resolve
      ~suffix
      state
      doc
      pos
      parsetree
      completion_entries
  ;;

  let process_dispatch_resp_nosquash
    ~deprecated
    ~resolve
    ~suffix
    (state : State.t)
    doc
    pos
    parsetree
    (completion : Query_protocol.completions)
    =
    (* Don't squash any function argument names into the completions, instead return both
       the completions and any such function argument names. *)
    ( process_entries
        ~deprecated
        ~resolve
        ~suffix
        state
        doc
        pos
        parsetree
        completion.entries
    , match completion.context with
      | `Unknown -> None
      | `Application { Query_protocol.Compl.labels; argument_type = _ } -> Some labels )
  ;;

  let completion_and_parsetree ~log_info doc ~prefix pos =
    Document.Merlin.with_pipeline_exn ~log_info ~priority doc (fun pipeline ->
      let dispatch_result = dispatch_cmd ~prefix (Position.logical pos) pipeline in
      let parsetree = Mpipeline.reader_parsetree pipeline in
      dispatch_result, parsetree)
  ;;

  let complete_squash ~log_info state doc ~prefix ~suffix pos ~deprecated ~resolve =
    let+ completion, parsetree = completion_and_parsetree ~log_info doc ~prefix pos in
    process_dispatch_resp_squash
      ~deprecated
      ~resolve
      ~prefix
      ~suffix
      state
      doc
      pos
      parsetree
      completion
  ;;

  let complete_nosquash ~log_info state doc ~prefix ~suffix pos ~deprecated ~resolve =
    let+ completion, parsetree = completion_and_parsetree ~log_info doc ~prefix pos in
    process_dispatch_resp_nosquash
      ~deprecated
      ~resolve
      ~suffix
      state
      doc
      pos
      parsetree
      completion
  ;;
end

module Complete_with_construct = struct
  let dispatch_cmd position pipeline =
    match
      Exn_with_backtrace.try_with (fun () ->
        let command = Query_protocol.Construct (position, None, None) in
        Query_commands.dispatch pipeline command)
    with
    | Ok (loc, exprs) -> Some (loc, exprs)
    | Error { Exn_with_backtrace.exn = Merlin_analysis.Construct.Not_a_hole; _ } -> None
    | Error exn -> Exn_with_backtrace.reraise exn
  ;;

  let process_dispatch_resp ~supportsJumpToNextHole = function
    | None -> []
    | Some (loc, constructed_exprs) ->
      let range = Range.of_loc loc in
      let deparen_constr_expr expr =
        if (not (String.equal expr "()"))
           && String.is_prefix expr ~prefix:"("
           && String.is_suffix expr ~suffix:")"
        then String.sub expr ~pos:1 ~len:(String.length expr - 2)
        else expr
      in
      let completionItem_of_constructed_expr idx expr =
        let expr_wo_parens = deparen_constr_expr expr in
        let edit = { TextEdit.range; newText = expr } in
        let command =
          if supportsJumpToNextHole
          then
            Some
              (Client.Custom_commands.next_hole
                 ~in_range:(Range.resize_for_edit edit)
                 ~notify_if_no_hole:false
                 ())
          else None
        in
        CompletionItem.create
          ~label:expr_wo_parens
          ~textEdit:(`TextEdit edit)
          ~filterText:("_" ^ expr)
          ~kind:CompletionItemKind.Text
          ~sortText:(sortText_of_index idx)
          ?command
          ()
      in
      List.mapi constructed_exprs ~f:completionItem_of_constructed_expr
  ;;
end

let complete
  ~log_info
  (state : State.t)
  ({ textDocument = { uri }; position = pos; context; _ } : CompletionParams.t)
  =
  Fiber.of_thunk (fun () ->
    let doc = Document_store.get state.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return None
    | `Merlin merlin ->
      let completion_item_capability =
        let open Option.O in
        let capabilities = State.client_capabilities state in
        let* td = capabilities.textDocument in
        let* compl = td.completion in
        compl.completionItem
      in
      let resolve =
        match
          let open Option.O in
          let* item = completion_item_capability in
          item.resolveSupport
        with
        | None -> false
        | Some { properties } -> List.mem properties ~equal:String.equal "documentation"
      in
      let* should_provide_completions =
        match context with
        | Some context ->
          (match context.triggerKind with
           | TriggerCharacter ->
             let+ inside_comment =
               Check_for_comments.position_in_comment
                 ~log_info
                 ~position:pos
                 ~merlin
                 ~priority:Priorities.completion
             in
             (match inside_comment with
              | true -> `Ignore
              | false -> `Provide_completions)
           | Invoked | TriggerForIncompleteCompletions ->
             Fiber.return `Provide_completions)
        | None -> Fiber.return `Provide_completions
      in
      (match should_provide_completions with
       | `Ignore -> Fiber.return None
       | `Provide_completions ->
         let+ items =
           let position = Position.logical pos in
           let short_path =
             (* send text before "." as prefix to merlin, to enable fuzzy search after
                ".". E.g. `List.mp` should match `List.map`. *)
             match state.configuration.data.fuzzy_completion with
             | None | Some { enable = false } -> `None
             | Some { enable = true } -> `Prefix
           in
           let prefix = prefix_of_position ~short_path (Document.source doc) position in
           let suffix =
             prefix_of_position ~short_path:`Suffix (Document.source doc) position
           in
           let deprecated =
             Option.value
               ~default:false
               (let open Option.O in
                let* item = completion_item_capability in
                item.deprecatedSupport)
           in
           if not (Typed_hole.can_be_hole prefix)
           then
             Complete_by_prefix.complete_squash
               ~log_info
               state
               merlin
               ~prefix
               ~suffix
               pos
               ~resolve
               ~deprecated
           else (
             let reindex_sortText completion_items =
               List.mapi completion_items ~f:(fun idx (ci : CompletionItem.t) ->
                 let sortText = Some (sortText_of_index idx) in
                 { ci with sortText })
             in
             let preselect_first =
               match
                 let open Option.O in
                 let* item = completion_item_capability in
                 item.preselectSupport
               with
               | None | Some false -> fun x -> x
               | Some true ->
                 (function
                   | [] -> []
                   | ci :: rest ->
                     { ci with CompletionItem.preselect = Some true } :: rest)
             in
             let+ construct_cmd_resp, compl_by_prefix_resp, parsetree =
               Document.Merlin.with_pipeline_exn
                 ~log_info
                 ~priority
                 merlin
                 (fun pipeline ->
                    let construct_cmd_resp =
                      Complete_with_construct.dispatch_cmd position pipeline
                    in
                    let compl_by_prefix_resp =
                      Complete_by_prefix.dispatch_cmd ~prefix position pipeline
                    in
                    let parsetree = Mpipeline.reader_parsetree pipeline in
                    construct_cmd_resp, compl_by_prefix_resp, parsetree)
             in
             let construct_completionItems =
               let supportsJumpToNextHole =
                 State.experimental_client_capabilities state
                 |> Client.Experimental_capabilities.supportsJumpToNextHole
               in
               Complete_with_construct.process_dispatch_resp
                 ~supportsJumpToNextHole
                 construct_cmd_resp
             in
             let compl_by_prefix_completionItems =
               Complete_by_prefix.process_dispatch_resp_squash
                 ~resolve
                 ~deprecated
                 ~prefix
                 ~suffix
                 state
                 merlin
                 pos
                 parsetree
                 compl_by_prefix_resp
             in
             construct_completionItems @ compl_by_prefix_completionItems
             |> reindex_sortText
             |> preselect_first)
         in
         Some (`CompletionList (CompletionList.create ~isIncomplete:false ~items ()))))
;;

let format_doc ~markdown doc =
  match markdown with
  | false -> `String doc
  | true ->
    `MarkupContent
      (match Doc_to_md.translate doc with
       | Markdown value -> { kind = MarkupKind.Markdown; MarkupContent.value }
       | Raw value -> { kind = MarkupKind.PlainText; MarkupContent.value })
;;

let resolve
  ~log_info
  doc
  (compl : CompletionItem.t)
  (resolve : Resolve.t)
  query_doc
  ~markdown
  =
  let query_type merlin_doc pos =
    Document.Merlin.with_pipeline_exn
      ~log_info
      ~priority:Priorities.completion
      merlin_doc
      (fun pipeline ->
         let command = Query_protocol.Type_enclosing (None, pos, Some 0) in
         match Query_commands.dispatch pipeline command with
         | (_, `String typ, _) :: _ -> Some typ
         | _ -> None)
  in
  Fiber.of_thunk (fun () ->
    (* Due to merlin's API, we create a version of the given document with the applied
       completion item and pass it to merlin to get the docs for the [compl.label] *)
    let position : Position.t = resolve.position in
    let logical_position = Position.logical position in
    let doc =
      let complete =
        let start =
          let prefix =
            prefix_of_position
              ~short_path:`Suffix
              (Document.Merlin.source doc)
              logical_position
          in
          { position with character = position.character - String.length prefix }
        in
        let end_ =
          let suffix = suffix_of_position (Document.Merlin.source doc) logical_position in
          { position with character = position.character + String.length suffix }
        in
        let range = Range.create ~start ~end_ in
        TextDocumentContentChangeEvent.create ~range ~text:compl.label ()
      in
      Document.update_text (Document.Merlin.to_doc doc) [ complete ]
    in
    let merlin_doc = Document.merlin_exn doc in
    let* documentation =
      let+ documentation = query_doc merlin_doc logical_position in
      Option.map ~f:(format_doc ~markdown) documentation
    in
    (* Fill in type detail if not already present *)
    let+ detail =
      match compl.detail with
      | Some d when not (String.is_empty d) -> Fiber.return compl.detail
      | _ ->
        let+ typ = query_type merlin_doc logical_position in
        (match typ with
         | Some typ -> Some typ
         | None -> compl.detail)
    in
    { compl with documentation; detail; data = None })
;;
