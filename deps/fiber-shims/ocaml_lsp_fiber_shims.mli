module Fiber = Ocaml_lsp_fiber
open Core
open Async

val map_reduce_errors_first
  :  (unit -> 'a Fiber.t)
  -> on_error:(Ocaml_lsp_stdune.Exn_with_backtrace.t -> unit Fiber.t)
  -> ('a, unit) result Fiber.t

val map_reduce_errors_with_monoid
  :  (module Ocaml_lsp_stdune.Monoid with type t = 'err)
  -> (unit -> 'a Fiber.t)
  -> on_error:(Ocaml_lsp_stdune.Exn_with_backtrace.t -> 'err Fiber.t)
  -> ('a, 'err) result Fiber.t

val collect_errors
  :  (unit -> 'a Fiber.t)
  -> ('a, Ocaml_lsp_stdune.Exn_with_backtrace.t list) Result.t Fiber.t

val with_error_handler
  :  (unit -> 'a Fiber.t)
  -> on_error:(Ocaml_lsp_stdune.Exn_with_backtrace.t -> Nothing.t Fiber.t)
  -> 'a Fiber.t

val close_fiber_pool : Fiber.Pool.t -> unit Fiber.t

module Var_optional : sig
  type 'a t

  val create_none : unit -> 'a t
  val get : 'a t -> 'a option Fiber.t
  val set : 'a t -> 'a option -> (unit -> 'b Fiber.t) -> 'b Fiber.t
end

module Mvar : sig
  type 'a t

  val create : unit -> 'a t
  val take : 'a t -> 'a Fiber.t
  val put : 'a t -> 'a -> unit Fiber.t
end

module Fiber_async : sig
  (** Interoperation between fibers and Async. This library assumes that the outer program
      is running with Async and that the fibers will be interpreted by
      [deferred_of_fiber].

      The most important difference between fibers and deferreds is that fibers are just
      continuations and do not store the value they compute, so if you bind twice on a
      given fiber the computation will be run twice. A [Fiber.Ivar.t] can be used to save
      the result of a computation. *)

  module Fiber : sig
    include module type of struct
      include Fiber
    end

    include Monad.S with type 'a t := 'a t
  end

  (** Convert a fiber to a computation that returns a deferred when run. *)
  val deferred_of_fiber : 'a Fiber.t -> unit -> 'a Deferred.t

  (** Convert a deferred to a fiber that stores the result in a [Fiber.Ivar.t]. This fiber
      can only be interpreted by [deferred_of_fiber] - using a different scheduler to run
      it will fail. *)
  val fiber_of_deferred : 'a Deferred.t -> 'a Fiber.t
end
