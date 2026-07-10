module Fiber = Ocaml_lsp_fiber

val test : ?expect_never:bool -> ('a -> Base.Sexp.t) -> (unit -> 'a Fiber.t) -> unit
