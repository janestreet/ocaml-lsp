open Import

(** This module contains functionality to handle `textDocument/references` LSP request. *)

(** [handle server reference_params] provides a response for LSP request
    `textDocument/references` *)
val handle
  :  log_info:Log_info.t
  -> State.t Server.t
  -> ReferenceParams.t
  -> Location.t list option Fiber.t

module For_testing : sig
  (** Filter out remote references from directories that already have local references. *)
  val deduplicate_remote_by_directory
    :  local:Location.t list
    -> remote:Location.t list
    -> Location.t list
end
