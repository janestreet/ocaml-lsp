(** Module for interacting with remote-lsp. This is useful for files that aren't being
    built locally. *)
open Import

(** Perform a remote-lsp go to definition/declaration query. *)
val go_to_query
  :  log_info:Log_info.t
  -> State.t
  -> DocumentUri.t
  -> Position.t
  -> Lsp.Types.Go_to_target.t
  -> Location.t option Fiber.t option

(** Perform a remote-lsp hover query. *)
val hover_query
  :  log_info:Log_info.t
  -> State.t
  -> DocumentUri.t
  -> Position.t
  -> Hover.t option Fiber.t option

(** Perform a remote-lsp references query. *)
val references_query
  :  log_info:Log_info.t
  -> State.t
  -> DocumentUri.t
  -> Position.t
  -> Lsp.Types.ReferenceContext.t
  -> Lsp.Types.Location.t list option Fiber.t option

(** Perform a remote-lsp merlin-call-compatible query. This is used by emacs to provide
    go-to functionality. *)
val call_compatible_query
  :  log_info:Log_info.t
  -> State.t
  -> Uri.t
  -> params:Jsonrpc.Structured.t option
  -> Json.t option Fiber.t option
