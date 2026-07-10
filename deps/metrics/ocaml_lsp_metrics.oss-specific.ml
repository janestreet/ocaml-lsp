module Fiber = Ocaml_lsp_fiber

type t = unit

let create () = ()
let with_metrics _ f = f ()
let report ?cat:_ ~name:_ ~ts:_ ~dur:_ () = Fiber.return ()
let dump () = Fiber.return {|{"traceEvents":[]}|}
