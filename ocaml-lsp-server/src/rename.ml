open Import
open Fiber.O
module Map = Core.Map

let priority = Priorities.rename

(** Given a [Loc.t] for an identifier path like [Foo.bar.baz], returns the loc of the last
    segment ([baz] in this case). If the path doesn't contain a [.], the same loc is
    returned. Given the current behavior of merlin occurrences, we only want to allow
    renaming when the item being renamed is a lowercase identifier, or the last element in
    the identifier (e.g., [Bar] in [Foo.Bar]), so we check for that here. *)
let loc_of_changeable_segment doc loc =
  let%bind.Option text = Document.get_source_text doc loc in
  let next_char =
    Document.get_source_text
      doc
      { loc_start = loc.loc_end
      ; loc_end = { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum + 1 }
      ; loc_ghost = loc.loc_ghost
      }
  in
  let next_char_is_period =
    Option.value_map ~default:false next_char ~f:(String.equal ".")
  in
  let is_lowercase s = Base.Char.is_lowercase s.[0] in
  match String.rsplit2 text ~on:'.' with
  | None when is_lowercase text || not next_char_is_period -> Some loc
  | Some (_, last) when is_lowercase last || not next_char_is_period ->
    (* Since an identifier can't span multiple lines, all fields except [loc_cnum] will be
       the same as in [loc.loc_end]. *)
    let loc_start =
      { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - String.length last }
    in
    Some { loc with loc_start }
  | _ -> None
;;

let prepare_rename
  ~log_info
  (state : State.t)
  ({ textDocument = { uri }; position; _ } : PrepareRenameParams.t)
  =
  let store = state.store in
  let doc = Document_store.get store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin_doc ->
    if String.is_suffix (Uri.to_path uri) ~suffix:".mli"
    then Fiber.return None
    else
      let+ ranges =
        Document.Merlin.with_pipeline_exn ~log_info ~priority merlin_doc (fun pipeline ->
          Merlin_analysis.Misc_utils.reconstruct_identifier
            pipeline
            (Position.to_lexical_position position)
            None)
      in
      List.hd ranges
      |> Option.map ~f:(fun ({ txt = _; loc } : _ Merlin_parsing.Location.loc) ->
        loc_of_changeable_segment doc loc)
      |> Option.join
      |> Option.map ~f:Range.of_loc
;;

let make_doc state uri text =
  let position_encoding = State.position_encoding state in
  Document.make
    ~position_encoding
    (State.wheel state)
    state.merlin_config
    state.merlin
    { textDocument = { languageId = "ocaml"; text; uri; version = 0 } }
;;

(* Checks if we're renaming a labeled or optional argument and avoids changing the
   function signature. *)
let create_edit (state : State.t) new_name loc =
  let range = Range.of_loc loc in
  let make_default_edit () = TextEdit.create ~range ~newText:new_name in
  (* [of_lexical_position] only returns [None] for the "dummy" position line 0, column -1. *)
  let ocurrence_start = Position.of_lexical_position loc.loc_start |> Option.value_exn in
  (* Renaming labelled and optional arguments shouldn't change function signatures, so if
     [x] is renamed to [y], a call like [let x = ... in f ~x] should become
     [let y = ... in f ~x:y]. To detect when this transformation is necessary, we check if
     the token before the identifier being renamed is [~] or [?]. If the document hasn't
     yet been opened, we first need to load it from disk. *)
  let uri = Uri.of_path loc.loc_start.pos_fname in
  let+ doc =
    match Document_store.get_opt state.store uri with
    | Some doc -> Fiber.return doc
    | None ->
      let text = Core.In_channel.read_all loc.loc_start.pos_fname in
      make_doc state uri text
  in
  let source = Document.source doc in
  let mpos = Position.logical ocurrence_start in
  let (`Offset index) = Msource.get_offset source mpos in
  match index with
  | 0 -> make_default_edit ()
  | _ ->
    let source_txt = Msource.text source in
    (match source_txt.[index - 1] with
     | '~' (* a named argument *) | '?' (* an optional argument *) ->
       let empty_range_at_occurrence_end = { range with start = range.Range.end_ } in
       TextEdit.create ~range:empty_range_at_occurrence_end ~newText:(":" ^ new_name)
     | _ -> make_default_edit ())
;;

let rename
  ~log_info
  (state : State.t)
  { RenameParams.textDocument = { uri }; position; newName; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return (Some (WorkspaceEdit.create ()))
  | `Merlin merlin ->
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position), `Renaming)
    in
    let* occurrences, _desync =
      Document.Merlin.dispatch_exn ~log_info merlin command ~priority
    in
    let version = Document.version doc in
    let+ edits =
      fold_left_fiber
        occurrences
        ~init:(Map.empty (module Core.String))
        ~f:(fun edits ({ loc; is_stale = _ } : Query_protocol.occurrence) ->
          let+ edit = create_edit state newName loc in
          Map.update edits loc.loc_start.pos_fname ~f:(function
            | None -> [ edit ]
            | Some edits -> edit :: edits))
    in
    let workspace_edits =
      let documentChanges =
        let open Option.O in
        Option.value
          ~default:false
          (let client_capabilities = State.client_capabilities state in
           let* workspace = client_capabilities.workspace in
           let* edit = workspace.workspaceEdit in
           edit.documentChanges)
      in
      if documentChanges
      then (
        let documentChanges =
          Map.to_alist edits
          |> List.map ~f:(fun (uri, edits) ->
            let uri = Uri.of_path uri in
            let edits = List.map edits ~f:(fun e -> `TextEdit e) in
            let textDocument =
              OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
            in
            `TextDocumentEdit (TextDocumentEdit.create ~textDocument ~edits))
        in
        WorkspaceEdit.create ~documentChanges ())
      else (
        let changes =
          Map.to_alist edits |> List.map ~f:(fun (uri, edits) -> Uri.of_path uri, edits)
        in
        WorkspaceEdit.create ~changes ())
    in
    Some workspace_edits
;;
