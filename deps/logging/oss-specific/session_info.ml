open! Core

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
  }
[@@deriving sexp_of]

let lookup_username () = Option.value (Sys.getenv "USER") ~default:"unknown"

let create ~deployment_stage ~editor ~editor_version ~worker_name =
  { username = lookup_username ()
  ; deployment_stage
  ; log_paths_and_features = false
  ; editor
  ; editor_version
  ; host = Core_unix.gethostname ()
  ; ocaml_version = Sys.ocaml_version
  ; build_version_jane = None
  ; worker_name
  }
;;
