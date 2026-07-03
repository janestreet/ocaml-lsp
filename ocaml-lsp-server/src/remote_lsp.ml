open Import
module Metadata_for_query = Ocaml_lsp_remote_lsp.Metadata_for_query

(* Sadly, there are no fiber combinators for working with options, so we isolate this
   fallible logic into a helper that can be called before every request. *)
let metadata_for_query (state : State.t) uri =
  let%bind.Option feature_id = state.feature_id in
  let%map.Option workspace, file_path = Ocaml_lsp_uri.workspace_and_relative_path uri in
  let file_contents = Document.text (Document_store.get state.store uri) in
  let file_path = File_path.of_relative file_path in
  { Metadata_for_query.feature_id; workspace; file_contents; file_path }
;;

(* Some remote-lsp errors (like UnknownRepo) indicate that we should no longer issue
   remote-lsp queries from this client. *)
let disable_remote_lsp (state : State.t) () = state.remote_lsp_available <- false

let go_to_query ~log_info (state : State.t) uri position target =
  let%map.Option metadata = metadata_for_query state uri in
  let disable_remote_lsp = disable_remote_lsp state in
  let stage = state.configuration.stage in
  Ocaml_lsp_remote_lsp.go_to_query
    ~log_info
    ~stage
    ~disable_remote_lsp
    ~metadata
    position
    target
;;

let hover_query ~log_info (state : State.t) uri position =
  let%map.Option metadata = metadata_for_query state uri in
  let disable_remote_lsp = disable_remote_lsp state in
  let stage = state.configuration.stage in
  Ocaml_lsp_remote_lsp.hover_query ~log_info ~stage ~disable_remote_lsp ~metadata position
;;

let references_query ~log_info (state : State.t) uri position context =
  let%map.Option metadata = metadata_for_query state uri in
  let disable_remote_lsp = disable_remote_lsp state in
  let stage = state.configuration.stage in
  Ocaml_lsp_remote_lsp.references_query
    ~log_info
    ~stage
    ~disable_remote_lsp
    ~metadata
    position
    context
;;

let call_compatible_query ~log_info (state : State.t) uri ~params =
  let%map.Option metadata = metadata_for_query state uri in
  let disable_remote_lsp = disable_remote_lsp state in
  let stage = state.configuration.stage in
  Ocaml_lsp_remote_lsp.call_compatible_query
    ~log_info
    ~stage
    ~disable_remote_lsp
    ~metadata
    ~params
;;
