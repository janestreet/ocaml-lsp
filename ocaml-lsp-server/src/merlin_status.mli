open! Import

(** Routines for checking whether merlin is able to properly answer queries (e.g. provide
    informative hovers, go to definitions, etc.). We determine this by checking whether
    merlin's config has any failures. Checks must occur on a uri by uri basis as some
    uri's may be included in the current build and some may not be.

    These routines are used when checking whether we should fall back to remote-lsp. *)

open! Import

(** Indicates whether merlin can answer queries for the given uri. *)
val merlin_can_answer_queries
  :  log_info:Log_info.t
  -> State.t
  -> Lsp.Uri.t
  -> bool Fiber.t

(** Identical to [merlin_can_answer_queries], except uses a preexisting pipeline to check
    merlin's status. This is more efficient when the caller already has a pipeline for the
    uri they want to check. *)
val merlin_pipeline_can_answer_queries : log_info:Log_info.t -> Mpipeline.t -> bool
