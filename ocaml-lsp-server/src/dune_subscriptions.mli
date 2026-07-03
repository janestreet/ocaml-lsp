open! Core
open! Async
open Import

(** [t] is a collection of dune subscriptions that are all closed together. *)
type t

val create : unit -> t

(** Used to detect if a build is running, so that we can connect to Dune even when builds
    start after the LSP starts up or when they die *)
val is_dune_running : unit -> bool Fiber.t

(** Subscribe to receive updates from a [Drpc.Sub.t] source, returning a callback that can
    be called to get the next update (or [`Eof] if the connection is closed). *)
val subscribe
  :  t
  -> 'a Drpc.Sub.t
  -> (unit -> [ `Eof | `Ok of 'a ] Fiber.t) Or_error.t Fiber.t

(** Close all connections that were made with [subscribe t sub]. *)
val close : t -> unit

(** Processes diagnostic events from dune and adds them to (or removes them from) the set
    of LSP diagnostics. *)
val set_dune_diagnostics : Diagnostics.t -> Drpc.Diagnostic.Event.t list -> unit

(** Register that a file in the given directory has been opened. If [start_build] is
    false, this is a no-op. Otherwise, this will start a build-watch subscription for the
    "merlin-nonrec" alias in that directory if one doesn't already exist. The [dir] should
    be a path relative to the dune root. *)
val register_open_dir : t -> dir:string -> unit Fiber.t

(** Unregister that a file in the given directory has been closed. If this was the last
    open file in the directory, the build-watch subscription will be cancelled. *)
val unregister_open_dir : t -> dir:string -> unit Fiber.t

module For_testing : sig
  (** Get the refcount for a directory, or None if not tracked *)
  val get_dir_refcount : t -> dir:string -> int option

  (** Get the list of all tracked directories *)
  val get_tracked_dirs : t -> string list
end
