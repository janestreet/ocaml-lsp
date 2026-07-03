open Import
open Fiber.O

let priority = Priorities.references

let handle_using_remote_lsp
  ~(log_info : Log_info.t)
  (state : State.t)
  (uri : DocumentUri.t)
  (position : Position.t)
  (context : Lsp.Types.ReferenceContext.t)
  : Location.t list option Fiber.t
  =
  Ocaml_lsp_logging.with_fiber_logging
    ~message:"handle references using remote lsp"
    log_info
    ~f:(fun () ->
      let res = Remote_lsp.references_query ~log_info state uri position context in
      match res with
      | None -> Fiber.return None
      | Some references -> references)
;;

let send_notification
  ?(type_ : MessageType.t = Warning)
  (server : State.t Server.t)
  (message : string)
  =
  let state = Server.state server in
  let msg = ShowMessageParams.create ~message ~type_ in
  task_if_running state.detached ~f:(fun () ->
    Server.notification server (ShowMessage msg))
;;

let handle_using_local_merlin
  ~(log_info : Log_info.t)
  (rpc : State.t Server.t)
  (state : State.t)
  (uri : DocumentUri.t)
  (position : Position.t)
  : Location.t list option Fiber.t
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin_doc ->
    let* occurrences, synced =
      Document.Merlin.dispatch_exn
        ~log_info
        ~priority
        merlin_doc
        (Occurrences (`Ident_at (Position.logical position), `Project))
    in
    let+ () =
      match synced with
      | `Out_of_sync _ ->
        send_notification
          rpc
          "The index might be out-of-sync. Make sure you are building the index file."
      | _ -> Fiber.return ()
    in
    Some
      (List.map occurrences ~f:(fun { loc; is_stale = _ } ->
         let range = Range.of_loc loc in
         let uri =
           match loc.loc_start.pos_fname with
           | "" -> uri
           | path -> Uri.of_path path
         in
         Log.log ~section:"debug" (fun () ->
           Log.msg
             "merlin returned fname %a"
             [ "pos_fname", `String loc.loc_start.pos_fname
             ; "uri", `String (Uri.to_string uri)
             ]);
         { Location.uri; range }))
;;

let uri_directory (uri : DocumentUri.t) = DocumentUri.to_path uri |> Filename.dirname

let deduplicate_remote_by_directory ~(local : Location.t list) ~(remote : Location.t list)
  : Location.t list
  =
  let local_dirs =
    List.map local ~f:(fun (loc : Location.t) -> uri_directory loc.uri)
    |> Core.Set.of_list (module Core.String)
  in
  List.filter remote ~f:(fun (loc : Location.t) ->
    not (Core.Set.mem local_dirs (uri_directory loc.uri)))
;;

let merge_results
  ~(log_info : Log_info.t)
  ~(local : Location.t list option)
  ~(remote : Location.t list option)
  : Location.t list option
  =
  let merged =
    match local, remote with
    | None, _ -> remote
    | _, None -> local
    | Some local, Some remote ->
      let remote_without_local = deduplicate_remote_by_directory ~local ~remote in
      Some (local @ remote_without_local)
  in
  let count c = Option.map c ~f:List.length |> Option.value ~default:0 in
  Ocaml_lsp_logging.log_reference_counts
    ~local_count:(count local)
    ~remote_count:(count remote)
    ~merged_count:(count merged)
    log_info;
  merged
;;

let handle
  ~(log_info : Log_info.t)
  (server : State.t Server.t)
  ({ textDocument = { uri }; position; context; _ } : ReferenceParams.t)
  : Location.t list option Fiber.t
  =
  Ocaml_lsp_logging.with_fiber_logging log_info ~f:(fun () ->
    let state : State.t = Server.state server in
    let* merlin_results = handle_using_local_merlin ~log_info server state uri position
    and* remote_results =
      if State.should_fall_back state
      then handle_using_remote_lsp ~log_info state uri position context
      else Fiber.return None
    in
    let* () =
      match remote_results with
      | None | Some [] -> Fiber.return ()
      | _ ->
        send_notification
          server
          "Some of the references are served by the remote-lsp and may be stale."
    in
    merge_results ~log_info ~local:merlin_results ~remote:remote_results |> Fiber.return)
;;

module For_testing = struct
  let deduplicate_remote_by_directory = deduplicate_remote_by_directory
end
