open! Core

(** Per-event fields that are logged *)
type t =
  { index : int
  (** Global index tracked in the lsp-server state; lets us associate multiple log entries
      to the same lsp request. *)
  ; action : string (** Name of the request/notification/etc. the lsp is handling. *)
  ; feature : string option (** Feature-id if it can be extracted and isn't sensitive. *)
  ; file : string option
  (** Path to the primary file from the repo-root if the feature is known. *)
  ; other_files : string list option
  (** Paths to other involved files from the repo-root if the feature is known. *)
  ; lines : int option (** Number of lines in the primary file. *)
  ; hash : int option (** A [String.hash] of the contents of the primary file. *)
  ; pos_line : int option (** Line of the cursor position in the request. *)
  ; pos_char : int option (** Character of the cursor position in the request. *)
  ; enqueue_time : Core.Time_ns.t (** Time when the request was enqueued. *)
  ; request_time : Core.Time_ns.t option
  (** Time when the request arrived. It is calculated by the wrapper, so might not be
      availble if ocaml-lsp is ran w/out one. *)
  }

val create
  :  index:int
  -> action:string
  -> ?log_paths_and_features:bool (* ignored *)
  -> ?primary_uri:Lsp.Uri.t (* ignored *)
  -> ?other_uris:Lsp.Uri.t list (* ignored *)
  -> ?text:string
  -> ?position:Lsp.Types.Position.t
  -> ?request_time:Core.Time_ns.t
  -> unit
  -> t

(** Returns a new [t] with [suffix] appended to the action name. *)
val update_action : suffix:string -> t -> t
