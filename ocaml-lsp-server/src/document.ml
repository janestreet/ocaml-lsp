open! Import
open Fiber.O

module Kind = struct
  type t =
    | Intf
    | Impl

  let of_fname_opt p =
    match Filename.extension p with
    | ".ml" | ".eliom" | ".re" -> Some Impl
    | ".mli" | ".eliomi" | ".rei" -> Some Intf
    | _ -> None
  ;;

  let unsupported uri =
    let p = Uri.to_path uri in
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:InvalidRequest
         ~message:"unsupported file extension"
         ~data:(`Assoc [ "extension", `String (Filename.extension p) ])
         ())
  ;;
end

module Syntax = struct
  type t =
    | Ocaml
    | Reason
    | Ocamllex
    | Menhir
    | Cram
    | Dune

  let human_name = function
    | Ocaml -> "OCaml"
    | Reason -> "Reason"
    | Ocamllex -> "OCamllex"
    | Menhir -> "Menhir/ocamlyacc"
    | Cram -> "Cram"
    | Dune -> "Dune"
  ;;

  let all =
    [ "ocaml.interface", Ocaml
    ; "ocaml", Ocaml
    ; "reason", Reason
    ; "ocaml.ocamllex", Ocamllex
    ; "ocaml.menhir", Menhir
    ; "cram", Cram
    ; "dune", Dune
    ; "dune-project", Dune
    ; "dune-workspace", Dune
    ]
  ;;

  let of_fname =
    let of_fname_res = function
      | "dune" | "dune-workspace" | "dune-project" -> Ok Dune
      | s ->
        (match Filename.extension s with
         | ".eliomi" | ".eliom" | ".mli" | ".ml" -> Ok Ocaml
         | ".rei" | ".re" -> Ok Reason
         | ".mll" -> Ok Ocamllex
         | ".mly" -> Ok Menhir
         | ".t" -> Ok Cram
         | ext -> Error ext)
    in
    fun s ->
      match of_fname_res s with
      | Ok x -> x
      | Error ext ->
        Jsonrpc.Response.Error.raise
          (Jsonrpc.Response.Error.make
             ~code:InvalidRequest
             ~message:(Printf.sprintf "unsupported file extension")
             ~data:(`Assoc [ "extension", `String ext ])
             ())
  ;;

  let to_language_id x =
    List.find_map all ~f:(fun (k, v) -> Option.some_if (v = x) k) |> Option.value_exn
  ;;

  let markdown_name = function
    | Ocaml -> "ocaml"
    | Reason -> "reason"
    | s -> to_language_id s
  ;;

  let of_text_document (td : Text_document.t) =
    match List.Assoc.find ~equal:String.equal all (Text_document.languageId td) with
    | Some s -> s
    | None -> Text_document.documentUri td |> Uri.to_path |> of_fname
  ;;
end

let await ~log_info task =
  let* cancel_token = Server.cancel_token () in
  let f () = Priority_lsp_executor.await_noninterleaving_task task in
  let without_cancellation res =
    match res with
    | Ok s -> Ok s
    | Error (`Exn exn) -> Error exn
    | Error `Cancelled ->
      let exn = Code_error.E (Code_error.create [%message "unexpected cancellation"]) in
      let backtrace = Printexc.get_callstack 10 in
      Error { Exn_with_backtrace.exn; backtrace }
  in
  match cancel_token with
  | None -> f () |> Fiber.map ~f:without_cancellation
  | Some t ->
    let+ res, outcome =
      Fiber.Cancel.with_handler t f ~on_cancel:(fun () ->
        Priority_lsp_executor.cancel_noninterleaving_task task)
    in
    (match outcome with
     | Not_cancelled -> without_cancellation res
     | Cancelled () ->
       let () =
         Ocaml_lsp_logging.log_event
           ~message:"cancelled by the client"
           ~category:"merlin cancellation"
           log_info
       in
       let e =
         Jsonrpc.Response.Error.make ~code:RequestCancelled ~message:"cancelled" ()
       in
       raise (Jsonrpc.Response.Error.E e))
;;

module Single_pipeline : sig
  type t

  val create : Priority_lsp_executor.t -> t

  val use
    :  t
    -> log_info:Log_info.t
    -> doc:Text_document.t
    -> config:Merlin_config.t
    -> f:(Mpipeline.t -> 'a)
    -> priority:Priority.t
    -> ('a, Exn_with_backtrace.t) result Fiber.t

  val use_with_config
    :  t
    -> log_info:Log_info.t
    -> doc:Text_document.t
    -> config:Mconfig.t
    -> f:(Mpipeline.t -> 'a)
    -> priority:Priority.t
    -> ('a, Exn_with_backtrace.t) result Fiber.t
end = struct
  type t = { thread : Priority_lsp_executor.t } [@@unboxed]

  let create thread = { thread }

  let use_with_config t ~(log_info : Log_info.t) ~doc ~config ~f ~priority =
    let make_pipeline =
      let source = Msource.make (Text_document.text doc) in
      fun () -> Mpipeline.make config source
    in
    let enqueue_time = Core.Time_ns.now () in
    let* task =
      let* tr =
        Priority_lsp_executor.schedule_noninterleaving_task
          t.thread
          ~priority
          ~f:(fun () ->
            let exec_start_time = Core.Time_ns.now () in
            let pipeline = make_pipeline () in
            let res = Mpipeline.with_pipeline pipeline (fun () -> f pipeline) in
            let exec_stop_time = Core.Time_ns.now () in
            ( res
            , exec_start_time
            , exec_stop_time
            , Mpipeline.timing_information pipeline
            , Mpipeline.cache_information pipeline ))
      in
      match tr with
      | Error `Stopped -> assert false
      | Ok tt -> Fiber.return tt
    in
    let* res = await ~log_info task in
    match res with
    | Error exn -> Fiber.return (Error exn)
    | Ok (res, exec_start_time, exec_stop_time, timing_breakdown, cache_information) ->
      let () =
        Ocaml_lsp_logging.log_merlin_timing
          ~enqueue_time
          ~exec_start_time
          ~exec_stop_time
          ~timing_breakdown
          ~cache_information
          log_info
      in
      let dur = Core.Time_ns.diff exec_stop_time exec_start_time in
      let+ () =
        Metrics.report
          ~cat:[ "merlin" ]
          ~ts:exec_start_time
          ~dur
          ~name:log_info.event.action
          ()
      in
      Ok res
  ;;

  let use t ~log_info ~doc ~config ~f ~priority =
    let* config = Merlin_config.config config in
    use_with_config t ~log_info ~doc ~config ~f ~priority
  ;;
end

type merlin =
  { tdoc : Text_document.t
  ; pipeline : Single_pipeline.t
  ; timer : Lev_fiber.Timer.Wheel.task
  ; merlin_config : Merlin_config.t
  ; syntax : Syntax.t
  ; kind : Kind.t option
  }

type t =
  | Other of
      { tdoc : Text_document.t
      ; syntax : Syntax.t
      }
  | Merlin of merlin

let tdoc = function
  | Other d -> d.tdoc
  | Merlin m -> m.tdoc
;;

let uri t = Text_document.documentUri (tdoc t)

let syntax = function
  | Merlin m -> m.syntax
  | Other t -> t.syntax
;;

let text t = Text_document.text (tdoc t)
let source t = Msource.make (text t)
let version t = Text_document.version (tdoc t)

let make_merlin wheel merlin_db pipeline tdoc syntax =
  let* timer = Lev_fiber.Timer.Wheel.task wheel in
  let uri = Text_document.documentUri tdoc in
  let path = Uri.drop_query uri |> Uri.to_path in
  let merlin_config = Merlin_config.DB.get merlin_db uri in
  let* mconfig = Merlin_config.config merlin_config in
  let kind =
    let ext = Filename.extension path in
    List.find_map mconfig.merlin.suffixes ~f:(fun (impl, intf) ->
      if String.equal ext intf
      then Some Kind.Intf
      else if String.equal ext impl
      then Some Kind.Impl
      else None)
  in
  let kind =
    match kind with
    | Some _ as k -> k
    | None -> Kind.of_fname_opt path
  in
  Fiber.return (Merlin { merlin_config; tdoc; pipeline; timer; syntax; kind })
;;

let make wheel config pipeline (doc : DidOpenTextDocumentParams.t) ~position_encoding =
  Fiber.of_thunk (fun () ->
    let tdoc = Text_document.make ~position_encoding doc in
    let syntax = Syntax.of_text_document tdoc in
    match syntax with
    | Ocaml | Reason -> make_merlin wheel config pipeline tdoc syntax
    | Ocamllex | Menhir | Cram | Dune -> Fiber.return (Other { tdoc; syntax }))
;;

let update_text ?version t changes =
  match Text_document.apply_content_changes ?version (tdoc t) changes with
  | exception Text_document.Invalid_utf error ->
    Log.log ~section:"warning" (fun () ->
      let error =
        match error with
        | Malformed input ->
          [ "message", `String "malformed input"; "input", `String input ]
        | Insufficient_input -> [ "message", `String "insufficient input" ]
      in
      Log.msg
        "dropping update due to invalid utf8"
        (( "changes"
         , Json.yojson_of_list TextDocumentContentChangeEvent.yojson_of_t changes )
         :: error));
    t
  | tdoc ->
    (match t with
     | Other o -> Other { o with tdoc }
     | Merlin t -> Merlin { t with tdoc })
;;

module Merlin = struct
  type t = merlin

  let to_doc t = Merlin t
  let source t = Msource.make (text (Merlin t))
  let timer (t : t) = t.timer

  let kind t =
    match t.kind with
    | Some k -> k
    | None -> Kind.unsupported (Text_document.documentUri t.tdoc)
  ;;

  let with_pipeline ~log_info ~priority (t : t) f =
    Single_pipeline.use
      ~log_info
      t.pipeline
      ~doc:t.tdoc
      ~config:t.merlin_config
      ~f
      ~priority
  ;;

  let with_configurable_pipeline ~log_info ~config ~priority (t : t) f =
    Single_pipeline.use_with_config ~log_info t.pipeline ~doc:t.tdoc ~config ~f ~priority
  ;;

  let mconfig (t : t) = Merlin_config.config t.merlin_config

  let with_pipeline_exn ~log_info ~priority doc f =
    let+ res = with_pipeline ~log_info ~priority doc f in
    match res with
    | Ok s -> s
    | Error exn -> Exn_with_backtrace.reraise exn
  ;;

  let with_configurable_pipeline_exn ~log_info ~config ~priority doc f =
    let+ res = with_configurable_pipeline ~log_info ~config ~priority doc f in
    match res with
    | Ok s -> s
    | Error exn -> Exn_with_backtrace.reraise exn
  ;;

  let dispatch ~log_info ~priority t command =
    with_pipeline ~log_info ~priority t (fun pipeline ->
      Query_commands.dispatch pipeline command)
  ;;

  let dispatch_exn ~log_info ~priority t command =
    with_pipeline_exn ~log_info ~priority t (fun pipeline ->
      Query_commands.dispatch pipeline command)
  ;;

  let doc_comment pipeline pos =
    let res =
      let command = Query_protocol.Document (None, pos) in
      Query_commands.dispatch pipeline command
    in
    match res with
    | `Found s | `Builtin s -> Some s
    | _ -> None
  ;;

  let syntax_doc pipeline pos =
    let res =
      let command = Query_protocol.Syntax_document pos in
      Query_commands.dispatch pipeline command
    in
    match res with
    | `Found s ->
      (match s.level with
       | Simple -> None
       | Advanced -> Some s)
    | `No_documentation -> None
  ;;

  let dispatch_enclosing_command pipeline command loc ~f =
    let res = Query_commands.dispatch pipeline command in
    List.find_map res ~f:(fun (loc', data) ->
      match Loc.compare loc loc' with
      | 0 ->
        (* matches type-enclosing range *)
        f data
      | _ -> None)
  ;;

  let stack_or_heap_enclosing pipeline pos loc =
    (* passing [true] here makes the request in "lsp compatibility" mode, which adjusts
       some ranges to better align with type-enclosing behavior *)
    let command = Query_protocol.Stack_or_heap_enclosing (pos, true, Some 0) in
    dispatch_enclosing_command pipeline command loc ~f:(function
      | `String msg ->
        (* has a stack-or-heap message *)
        Some msg
      | _ -> None)
  ;;

  let kind_enclosing pipeline pos loc ~verbosity =
    let command =
      Query_protocol.Kind_enclosing
        { position = pos; index = Some 0; override_verbosity = Some (Lvl verbosity) }
    in
    dispatch_enclosing_command pipeline command loc ~f:(function
      | `Kind msg ->
        (* has a kind message *)
        Some msg
      | _ -> None)
  ;;

  let mode_enclosing pipeline pos loc ~verbosity =
    let command =
      Query_protocol.Mode_enclosing
        { position = pos; override_verbosity = Some (Lvl verbosity) }
    in
    dispatch_enclosing_command pipeline command loc ~f:Option.some
  ;;

  type type_enclosing =
    { loc : Loc.t
    ; typ : string
    ; doc : string option
    ; stack_or_heap : string option
    ; kind : string option
    ; mode : string option
    ; syntax_doc : string option
    }

  let type_enclosing ~(log_info : Log_info.t) doc pos verbosity ~syntax_doc ~priority =
    with_pipeline_exn ~log_info ~priority doc (fun pipeline ->
      let command = Query_protocol.Type_enclosing (None, pos, Some 0) in
      let pipeline =
        match verbosity with
        | 0 -> pipeline
        | verbosity ->
          let source = source doc in
          let config = Mpipeline.final_config pipeline in
          let config =
            { config with query = { config.query with verbosity = Lvl verbosity } }
          in
          Mpipeline.make config source
      in
      let res = Query_commands.dispatch pipeline command in
      match res with
      | [] | (_, `Index _, _) :: _ -> None
      | (loc, `String typ, _) :: _ ->
        let doc = doc_comment pipeline pos in
        let stack_or_heap = stack_or_heap_enclosing pipeline pos loc in
        let on_verbose_enclosing enclosing_fn =
          (* We only display the kind and mode if the verbosity is 1 or more to avoid
             adding noise. Because of this, we subtract 1 from the verbosity so that the
             first kind we display as we increase verbosity is the least verbose one. *)
          if verbosity > 0
          then enclosing_fn pipeline pos loc ~verbosity:(verbosity - 1)
          else None
        in
        let kind = on_verbose_enclosing kind_enclosing in
        let mode = on_verbose_enclosing mode_enclosing in
        Some { loc; typ; doc; stack_or_heap; kind; mode; syntax_doc })
  ;;

  let doc_comment ~(log_info : Log_info.t) ~(priority : Priority.t) doc pos =
    with_pipeline_exn ~log_info ~priority doc (fun pipeline -> doc_comment pipeline pos)
  ;;
end

let edit t text_edits =
  let version = version t in
  let textDocument =
    OptionalVersionedTextDocumentIdentifier.create ~uri:(uri t) ~version ()
  in
  let edit =
    TextDocumentEdit.create
      ~textDocument
      ~edits:(List.map text_edits ~f:(fun text_edit -> `TextEdit text_edit))
  in
  WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
;;

let kind = function
  | Merlin merlin -> `Merlin merlin
  | Other _ -> `Other
;;

let merlin_exn t =
  match kind t with
  | `Merlin m -> m
  | `Other ->
    Code_error.raise_s [%message "Document.merlin_exn" ~t:(uri t : DocumentUri.t)]
;;

let close t =
  match t with
  | Other _ -> Fiber.return ()
  | Merlin t ->
    Fiber.fork_and_join_unit
      (fun () -> Merlin_config.destroy t.merlin_config)
      (fun () -> Lev_fiber.Timer.Wheel.cancel t.timer)
;;

let get_impl_intf_counterparts m uri =
  let fpath = Uri.to_path uri in
  let fname = Filename.basename fpath in
  let ml, mli, eliom, eliomi, re, rei, mll, mly =
    "ml", "mli", "eliom", "eliomi", "re", "rei", "mll", "mly"
  in
  let exts_to_switch_to =
    let kind =
      match m with
      | Some m -> Merlin.kind m
      | None ->
        (* still try to guess the kind *)
        (match Kind.of_fname_opt fpath with
         | Some k -> k
         | None -> Kind.unsupported uri)
    in
    match Syntax.of_fname fname with
    | Dune | Cram -> []
    | Ocaml ->
      (match kind with
       | Intf -> [ ml; mly; mll; eliom; re ]
       | Impl -> [ mli; mly; mll; eliomi; rei ])
    | Reason ->
      (match kind with
       | Intf -> [ re; ml ]
       | Impl -> [ rei; mli ])
    | Ocamllex -> [ mli; rei ]
    | Menhir -> [ mli; rei ]
  in
  let fpath_w_ext ext = Filename.remove_extension fpath ^ "." ^ ext in
  let find_switch exts =
    List.filter_map exts ~f:(fun ext ->
      let file_to_switch_to = fpath_w_ext ext in
      Option.some_if (Sys.file_exists file_to_switch_to) file_to_switch_to)
  in
  let files_to_switch_to =
    match find_switch exts_to_switch_to with
    | [] ->
      let switch_to_ext = List.hd_exn exts_to_switch_to in
      let switch_to_fpath = fpath_w_ext switch_to_ext in
      [ switch_to_fpath ]
    | to_switch_to -> to_switch_to
  in
  List.map ~f:Uri.of_path files_to_switch_to
;;

let substring doc range =
  let start, end_ = Text_document.absolute_range (tdoc doc) range in
  let text = text doc in
  if start < 0 || start > end_ || end_ > String.length text
  then None
  else Some (String.sub text ~pos:start ~len:(end_ - start))
;;

let get_source_text doc (loc : Loc.t) =
  let open Option.O in
  let source = source doc in
  let* start = Position.of_lexical_position loc.loc_start in
  let+ end_ = Position.of_lexical_position loc.loc_end in
  let (`Offset start) = Msource.get_offset source (Position.logical start) in
  let (`Offset end_) = Msource.get_offset source (Position.logical end_) in
  String.sub (Msource.text source) ~pos:start ~len:(end_ - start)
;;
