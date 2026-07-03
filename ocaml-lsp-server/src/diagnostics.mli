open Import
module Diagnostic_parser = Ocaml_lsp_dune_integration.Diagnostic_parser

val ocamllsp_source : string
val dune_source : string

(** A [t] manages the diagnostics that ocaml-lsp knows about for each file. *)
type t

val create
  :  PublishDiagnosticsClientCapabilities.t option
  -> (PublishDiagnosticsParams.t -> unit Fiber.t)
  -> which_diagnostics:Config_data.WhichDiagnostics.t
  -> shorten_merlin_diagnostics:bool
  -> client_name:string
  -> t

(** Send merlin diagnostics for a given [Uri.t] to the LSP client. Also sends dune
    diagnostics for the URI if [t.whichDiagnostics.dune] is set. Doesn't send merlin
    diagnostics that are already covered by a dune diagnostic (regardless of whether dune
    diagnostics are sent). *)
val send : t -> Uri.t -> merlin_diagnostics:Diagnostic.t list -> unit Fiber.t

(** Adds a batch of diagnostics to those that the [t] knows about. *)
val add_dune_diagnostics : t -> Drpc.Diagnostic.Id.t * (Uri.t * Diagnostic.t) list -> unit

(** Remove a batch of diagnostics from the [t]. *)
val remove_dune_diagnostics : t -> Drpc.Diagnostic.Id.t -> unit

val tags_of_message
  :  t
  -> src:[< `Dune | `Merlin ]
  -> string
  -> DiagnosticTag.t list option

(** Queries Merlin for diagnostics if [merlin_syntax] or [merlin_typing] is set to true in
    [t.whichDiagnostics]; otherwise, acts as if Merlin returns [[]]. *)
val merlin_diagnostics_for_file
  :  log_info:Log_info.t
  -> t
  -> Document.Merlin.t
  -> Diagnostic.t list Fiber.t

(** Turn on/off each category of errors: syntax or typing. *)
val set_which_diagnostics : t -> which_diagnostics:Config_data.WhichDiagnostics.t -> unit

(** Uri's whose dune diagnostics were changed since the last build. *)
val updated_dune_uris : t -> Uri.t list

(** Reset the list of uri's marked as having received dune diagnostics since the last
    build. *)
val clear_updated_dune_uris : t -> unit

val set_shorten_merlin_diagnostics : t -> shorten_merlin_diagnostics:bool -> unit

(** Convert a the Diagnostic.t type from the Diagnostic_parser library to the
    corresponding LSP type. *)
val of_diagnostic_parser : t -> Diagnostic_parser.Diagnostic.t -> Diagnostic.t

module For_testing : sig
  (** Removes an error number indicator from the front of an error message. Useful for
      deduplicating diagnostics because dune and merlin have slightly different formats *)
  val remove_errno : string -> string

  val equal_message : string -> string -> bool
end
