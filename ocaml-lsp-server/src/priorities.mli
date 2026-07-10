open! Import

(** Realtime queries *)

val call_hierarchy : Priority.t
val completion : Priority.t
val declaration : Priority.t
val definition : Priority.t
val diagnostics : Priority.t
val get_documentation : Priority.t
val hover : Priority.t
val merlin_call_compatible : Priority.t
val merlin_can_answer_queries : Priority.t
val references : Priority.t
val rename : Priority.t
val type_definition : Priority.t
val type_enclosing : Priority.t
val typed_holes : Priority.t
val wrapping_ast_node : Priority.t
val signature_help : Priority.t

(** Background queries *)

val code_action : Priority.t
val code_lens : Priority.t
val color : Priority.t
val document_symbol : Priority.t
val folding_range : Priority.t
val highlight : Priority.t
val inlay_hint : Priority.t
val selection_range : Priority.t
val semantic_highlighting : Priority.t
