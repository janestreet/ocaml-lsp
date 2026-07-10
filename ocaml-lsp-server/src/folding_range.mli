(** This module is responsible for computing the folding range for OCaml source *)

open Import

(** Compute the folding range *)
val compute
  :  log_info:Log_info.t
  -> State.t
  -> FoldingRangeParams.t
  -> FoldingRange.t list option Fiber.t
