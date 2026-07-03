module Fiber = Ocaml_lsp_fiber
open Core

type t

val create : unit -> t
val with_metrics : t -> (unit -> 'a Fiber.t) -> 'a Fiber.t

val report
  :  ?cat:string list
  -> name:string
  -> ts:Time_ns.t
  -> dur:Time_ns.Span.t
  -> unit
  -> unit Fiber.t

val dump : unit -> string Fiber.t
