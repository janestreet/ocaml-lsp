module Fiber = Ocaml_lsp_fiber
open Test.Import

val uri : Uri.t

val test
  :  ?extra_env:string list
  -> string
  -> (unit Client.t -> unit Fiber.t)
  -> unit Async.Deferred.t
