open Import
module Diagnostic_parser = Ocaml_lsp_dune_integration.Diagnostic_parser

let priority = Priorities.diagnostics

(* NB: Changing when diagnostics are sent, in particular, eliminating instances when
   diagnostics are sent, can cause tests to hang. This is because they wait for
   diagnostics to arrive as a signal that the LSP is done processing. *)

let ocamllsp_source = "ocamllsp"
let merlin_source = "lsp/merlin"
let dune_source = "lsp/dune"

module Uri_c = Comparable.Make_plain (Uri)
module Uri_set = Uri_c.Set

module Id = struct
  module T = Drpc.Diagnostic.Id
  include T
  module Table = Core.Hashtbl.Make_plain (T)
end

let remove_errno msg =
  (* Merlin errors are sometimes prefixed with "Error (warning ..): ..." as opposed to
     Dune's, which are sometimes prefixed with "(warning .. [...]): ...". Perhaps it may
     be better to investigate why this is, but it doesn't hurt to just filter that out. *)
  match
    Re.Str.bounded_split
      (Re.Str.regexp {|^Error (warning [0-9]+): \|^(warning [0-9]+ \[[a-z-]+\]): |})
      msg
      1
  with
  | [ rest ] when not (String.equal rest msg) -> rest
  | _ -> msg
;;

let equal_message =
  (* because the compiler and merlin wrap messages differently *)
  let is_space = function
    | ' ' | '\r' | '\n' | '\t' -> true
    | _ -> false
  in
  let eat_space s i =
    while !i < String.length s && is_space s.[!i] do
      incr i
    done
  in
  fun m1 m2 ->
    (* Merlin errors that are warnings start with a message of the form Error (warning ..)
       while Dune errors omit that part - filter that out for the messages *)
    let m1 = remove_errno m1 in
    let m2 = remove_errno m2 in
    let i = ref 0 in
    let j = ref 0 in
    try
      eat_space m1 i;
      eat_space m2 j;
      while !i < String.length m1 && !j < String.length m2 do
        let ci = m1.[!i] in
        let cj = m2.[!j] in
        if is_space ci && is_space cj
        then (
          eat_space m1 i;
          eat_space m2 j)
        else if Char.equal ci cj
        then (
          incr i;
          incr j)
        else raise_notrace Exit
      done;
      eat_space m1 i;
      eat_space m2 j;
      (* we make sure that everything is consumed *)
      !i = String.length m1 && !j = String.length m2
    with
    | Exit -> false
;;

type t =
  { dune : (Uri.t * Diagnostic.t) list Id.Table.t
  (** Dune sends diagnostics in batches which are keyed by unique id *)
  ; send : PublishDiagnosticsParams.t -> unit Fiber.t
  (** Callback to send diagnostics to client. *)
  ; mutable updated_dune_uris : Uri.t list
  (** URIs whose dune diagnostics were changed since the last build. This is used to
      determine which files to refresh diagnostics for after a build finishes. This field
      can be slightly incorrect if dune diagnostics for a build arrive after the progress
      notification indicating the build is done. *)
  ; related_information : bool
  ; supported_tags : DiagnosticTag.t list
  ; client_name : string
  ; mutable which_diagnostics : Config_data.WhichDiagnostics.t
  ; mutable shorten_merlin_diagnostics : bool
  (** Whether to shorten the range of merlin diagnostics we send to the client. *)
  }

let create
  (capabilities : PublishDiagnosticsClientCapabilities.t option)
  send
  ~which_diagnostics
  ~shorten_merlin_diagnostics
  ~client_name
  =
  let related_information, supported_tags =
    match capabilities with
    | None -> false, []
    | Some c ->
      ( Option.value ~default:false c.relatedInformation
      , (match c.tagSupport with
         | None -> []
         | Some { valueSet } -> valueSet) )
  in
  { dune = Id.Table.create ~size:32 ()
  ; send
  ; updated_dune_uris = []
  ; related_information
  ; supported_tags
  ; which_diagnostics
  ; shorten_merlin_diagnostics
  ; client_name = String.lowercase client_name
  }
;;

let send t uri ~merlin_diagnostics =
  let module Range_map = Map.Make_plain (Range) in
  (* TODO deduplicate related errors as well *)
  let dune_diagnostics_for_uri =
    Hashtbl.fold t.dune ~init:[] ~f:(fun ~key:_ ~data:batch acc ->
      let diagnostics_for_uri =
        List.filter_map batch ~f:(fun (dune_uri, diagnostic) ->
          if Uri.equal dune_uri uri then Some diagnostic else None)
      in
      diagnostics_for_uri @ acc)
  in
  let dune_diagnostics_by_range =
    dune_diagnostics_for_uri
    |> List.map ~f:(fun (d : Diagnostic.t) -> d.range, d)
    |> Range_map.of_alist_multi
  in
  (* Checks if [diagnostic] is a duplicate of a preexisting dune diagnostic. *)
  let is_a_duplicate (merlin_diagnostic : Diagnostic.t) =
    match Map.find dune_diagnostics_by_range merlin_diagnostic.range with
    | None -> false
    | Some dune_diagnostics ->
      let equiv_message (d1 : Diagnostic.t) (d2 : Diagnostic.t) =
        match d1.message, d2.message with
        | `String m1, `String m2 -> equal_message m1 m2
        | `MarkupContent { kind; value }, `MarkupContent mc ->
          Poly.equal kind mc.kind && equal_message value mc.value
        | _, _ -> false
      in
      List.exists dune_diagnostics ~f:(fun (d : Diagnostic.t) ->
        equiv_message d merlin_diagnostic)
  in
  let deduplicated_merlin_diagnostics =
    if String.is_prefix t.client_name ~prefix:"vscode"
    then List.filter merlin_diagnostics ~f:(fun d -> not (is_a_duplicate d))
    else merlin_diagnostics
  in
  (* Add outbound dune diagnostics only if asked for. *)
  let outbound_dune_diagnostics =
    if t.which_diagnostics.dune then dune_diagnostics_for_uri else []
  in
  let outbound_diagnostics =
    (* We don't include a version because some of the diagnostics might come from dune
       which reads from the file system and not from the editor's view. *)
    PublishDiagnosticsParams.create
      ~uri
      ~diagnostics:(outbound_dune_diagnostics @ deduplicated_merlin_diagnostics)
      ()
  in
  t.send outbound_diagnostics
;;

let add_dune_diagnostics t (batch_id, batch) =
  List.iter batch ~f:(fun (uri, _) -> t.updated_dune_uris <- uri :: t.updated_dune_uris);
  Hashtbl.set t.dune ~key:batch_id ~data:batch
;;

let remove_dune_diagnostics t batch_id =
  Hashtbl.find t.dune batch_id
  |> Option.iter ~f:(fun batch ->
    List.iter batch ~f:(fun (uri, _) -> t.updated_dune_uris <- uri :: t.updated_dune_uris);
    Hashtbl.remove t.dune batch_id)
;;

(** Get the supported diagnostic tags of a message. *)
let tags_of_message t ~src message =
  let (tag : DiagnosticTag.t option) =
    match src with
    | `Dune when String.is_prefix message ~prefix:"unused" -> Some Unnecessary
    | `Merlin when Diagnostic_util.is_unused_var_warning message -> Some Unnecessary
    | `Merlin when Diagnostic_util.is_deprecated_warning message -> Some Deprecated
    | `Dune | `Merlin -> None
  in
  match tag with
  | Some tag when List.mem t.supported_tags tag ~equal:Poly.equal -> Some [ tag ]
  | None | Some _ -> None
;;

let extract_related_errors uri raw_message =
  match Ocamlc_loc.parse_raw raw_message with
  | `Message message :: related ->
    let string_of_message message = String.strip message in
    let related =
      let rec loop acc = function
        | `Loc loc :: `Message m :: xs -> loop ((loc, m) :: acc) xs
        | [] -> List.rev acc
        | _ ->
          (* give up when we see something unexpected *)
          Log.log ~section:"debug" (fun () ->
            Log.msg "unable to parse error" [ "error", `String raw_message ]);
          []
      in
      loop [] related
    in
    let related =
      match related with
      | [] -> None
      | related ->
        let make_related ({ Ocamlc_loc.path = _; lines; chars }, message) =
          let location =
            let start, end_ =
              let line_start, line_end =
                match lines with
                | Single i -> i, i
                | Range (i, j) -> i, j
              in
              let char_start, char_end =
                match chars with
                | None -> 1, 1
                | Some (x, y) -> x, y
              in
              ( Position.create ~line:line_start ~character:char_start
              , Position.create ~line:line_end ~character:char_end )
            in
            let range = Range.create ~start ~end_ in
            Location.create ~range ~uri
          in
          let message = string_of_message message in
          DiagnosticRelatedInformation.create ~location ~message
        in
        Some (List.map related ~f:make_related)
    in
    string_of_message message, related
  | _ -> raw_message, None
;;

let first_n_lines_of_range (range : Range.t) n : Range.t =
  if range.end_.line - range.start.line < n
  then range
  else
    { start = { character = range.start.character; line = range.start.line }
    ; end_ = { character = 0; line = range.start.line + n }
    }
;;

let merlin_report_to_diagnostic ~diagnostics ~merlin ~error =
  let doc = Document.Merlin.to_doc merlin in
  let uri = Document.uri doc in
  let loc = Loc.loc_of_report error in
  let original_range = Range.of_loc loc in
  let range =
    if diagnostics.shorten_merlin_diagnostics
    then first_n_lines_of_range original_range 1
    else original_range
  in
  let severity =
    match error.source with
    | Warning -> DiagnosticSeverity.Warning
    | _ -> DiagnosticSeverity.Error
  in
  let make_message ppf m = String.strip (Format.asprintf "%a@." ppf m) in
  (* NB: as of 2025-06-09 the formatter [Loc.print_main] can raise due to a bug in the
     short-paths logic in merlin *)
  let message = make_message Loc.print_main error in
  let message, relatedInformation =
    match diagnostics.related_information with
    | false -> message, None
    | true ->
      (match error.sub with
       | [] -> extract_related_errors uri message
       | _ :: _ ->
         ( message
         , Some
             (List.map error.sub ~f:(fun (sub : Loc.msg) ->
                let location =
                  let range = Range.of_loc sub.loc in
                  Location.create ~range ~uri
                in
                let message = make_message Loc.print_sub_msg sub in
                DiagnosticRelatedInformation.create ~location ~message)) ))
  in
  let maybe_extra_range_information =
    match diagnostics.shorten_merlin_diagnostics with
    | false -> None
    | true ->
      let start_location = Location.create ~range:original_range ~uri in
      Some
        [ DiagnosticRelatedInformation.create
            ~location:start_location
            ~message:"Original error span"
        ]
  in
  let relatedInformation =
    Option.merge maybe_extra_range_information relatedInformation ~f:( @ )
  in
  let tags = tags_of_message diagnostics ~src:`Merlin message in
  Diagnostic.create
    ?tags
    ?relatedInformation
    ~range
    ~source:merlin_source
    ~message:(`String message)
    ~severity
    ()
;;

let diagnostic_of_lrgrep ~start ~end_ message =
  Lsp.Types.Diagnostic.create
    ~message:(`String message)
    ~range:(Range.create ~start ~end_)
    ~source:"lsp/lrgrep"
    ()
;;

(* Result of asking lrgrep to describe the first syntax error:
   - [`No_error]: no error fond
   - [`Diagnostic d]: error found and diagnostic created
   - [`Error_producing_diagnostic]: lrgrep reported an error but at a location we can't
     turn into an LSP range. For instance, the OSS lrgrep shim is a stub that always
     reports a dummy location *)
let first_syntax_error ~path ~contents =
  match Ocaml_lsp_lrgrep_shim.parse_file ~path (Lexing.from_string contents) with
  | Ok () -> `No_error
  | Error { msg; loc = { loc_start; loc_end; loc_ghost = _ } } ->
    (match
       Position.of_lexical_position loc_start, Position.of_lexical_position loc_end
     with
     | Some start, Some end_ -> `Diagnostic (diagnostic_of_lrgrep ~start ~end_ msg)
     | None, _ | _, None -> `Error_producing_diagnostic)
;;

(** Get merlin diagnostics for a single file. *)
let query_merlin_for_diagnostics ~log_info diagnostics merlin =
  let create_diagnostic ~message =
    Diagnostic.create ~source:merlin_source ~message:(`String message)
  in
  let command = Query_protocol.Errors { lexing = true; parsing = true; typing = true } in
  Document.Merlin.with_pipeline_exn ~log_info ~priority merlin (fun pipeline ->
    match Query_commands.dispatch pipeline command with
    | exception Merlin_extend.Extend_main.Handshake.Error error ->
      let message =
        sprintf "%s.\nHint: install the following packages: merlin-extend, reason" error
      in
      [ create_diagnostic ~range:Range.first_line ~message () ]
    | all_errors ->
      let merlin_diagnostics =
        match (Mpipeline.final_config pipeline).merlin.failures with
        | _ :: _ as config_failures ->
          (* Always return configuration errors -- if the user requested syntax or typing
             errors, they'll probably need to fix their configuration to get them. *)
          List.map config_failures ~f:(fun failure ->
            create_diagnostic ~range:Range.first_line ~message:failure ())
        | [] ->
          let syntax_diagnostics, typer_diagnostics =
            Base.List.partition_map all_errors ~f:(fun (error : Loc.error) ->
              let diagnostic = merlin_report_to_diagnostic ~diagnostics ~merlin ~error in
              match error.source with
              | Lexer | Parser -> First diagnostic
              | _ -> Second diagnostic)
          in
          (* Only show syntax errors if there are any, otherwise show the other
             diagnostics *)
          (match syntax_diagnostics with
           | [] ->
             let holes_as_err_diags =
               Query_commands.dispatch pipeline Holes
               |> List.rev_map ~f:(fun (loc, typ) ->
                 let range = Range.of_loc loc in
                 let severity = DiagnosticSeverity.Error in
                 let message =
                   "This typed hole should be replaced with an expression of type " ^ typ
                 in
                 (* we set specific diagnostic code = "hole" to be able to filter through
                    diagnostics easily *)
                 create_diagnostic ~code:(`String "hole") ~range ~message ~severity ())
             in
             (* Can we use [List.merge] instead? *)
             if diagnostics.which_diagnostics.merlin_typing
             then List.rev_append holes_as_err_diags typer_diagnostics
             else []
           | _ ->
             (match diagnostics.which_diagnostics.merlin_syntax with
              | false -> []
              | true ->
                (* Use lrgrep to get a better syntax error message. When lrgrep is fails
                   to produce a diagnostic, fall back to merlin's own syntax diagnostics
                   rather than raising. *)
                let doc = Document.Merlin.to_doc merlin in
                let path = Document.uri doc |> Uri.to_path in
                let contents = Document.text doc in
                (match first_syntax_error ~path ~contents with
                 | `Diagnostic lrgrep_diagnostic -> [ lrgrep_diagnostic ]
                 | `No_error -> []
                 | `Error_producing_diagnostic -> syntax_diagnostics)))
      in
      List.sort
        merlin_diagnostics
        ~compare:(fun (d1 : Diagnostic.t) (d2 : Diagnostic.t) ->
          Range.compare d1.range d2.range))
;;

let merlin_diagnostics_for_file ~log_info diagnostics merlin =
  (* Avoid calling merlin if we're not going to display any diagnostics. This has the
     added benefit of avoiding a merlin short-paths bug triggered by the
     [e2e-new/semantic_hl_data.ml] tests. *)
  if diagnostics.which_diagnostics.merlin_typing
     || diagnostics.which_diagnostics.merlin_syntax
  then query_merlin_for_diagnostics ~log_info diagnostics merlin
  else Fiber.return []
;;

let updated_dune_uris t = t.updated_dune_uris
let clear_updated_dune_uris t = t.updated_dune_uris <- []
let set_which_diagnostics t ~which_diagnostics = t.which_diagnostics <- which_diagnostics

let set_shorten_merlin_diagnostics t ~shorten_merlin_diagnostics =
  (* The ocaml-lsp ChangeConfiguration command handles re-sending diagnostics on
     configuration changes *)
  t.shorten_merlin_diagnostics <- shorten_merlin_diagnostics
;;

let of_diagnostic_parser t (dp : Diagnostic_parser.Diagnostic.t) =
  let tags = tags_of_message t ~src:`Dune dp.message in
  Diagnostic.of_diagnostic_parser ?tags ~source:dune_source dp
;;

module For_testing = struct
  let remove_errno = remove_errno
  let equal_message = equal_message
end
