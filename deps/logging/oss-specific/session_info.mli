(** Session-level metadata included in every log row. This is common between all users of
    [ocaml-lsp-logging] *)
type t =
  { username : string
  ; deployment_stage : Stage.t
  ; log_paths_and_features : bool
  ; editor : string
  ; editor_version : string
  ; host : string
  ; ocaml_version : string
  ; build_version_jane : string option
  ; worker_name : string option
  (** Logical name of the ocaml-lsp worker process behind this session (as passed via
      [ocaml-lsp -worker-name]). Populated by [ocaml-lsp-wrapper] when it spawns workers
      so that each worker's logs can be distinguished. *)
  }
[@@deriving sexp_of]

val create
  :  deployment_stage:Stage.t
  -> editor:string
  -> editor_version:string
  -> worker_name:string option
  -> t
