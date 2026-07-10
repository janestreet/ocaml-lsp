open! Import
open Priority

(* Realtime *)
let call_hierarchy = Realtime
let completion = Realtime
let declaration = Realtime
let definition = Realtime
let diagnostics = Realtime
let get_documentation = Realtime
let hover = Realtime
let merlin_call_compatible = Realtime
let merlin_can_answer_queries = Realtime
let references = Realtime
let rename = Realtime
let type_definition = Realtime
let type_enclosing = Realtime
let typed_holes = Realtime
let wrapping_ast_node = Realtime
let signature_help = Realtime

(* Background *)
let code_action = Background
let code_lens = Background
let color = Background
let document_symbol = Background
let folding_range = Background
let highlight = Background
let inlay_hint = Background
let selection_range = Background
let semantic_highlighting = Background
