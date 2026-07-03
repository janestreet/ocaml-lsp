val get_test_ocaml_lsp_bin : unit -> string

module Expect_test_helpers : sig
  val require_does_raise : here:Lexing.position -> (unit -> 'a) -> unit
end
