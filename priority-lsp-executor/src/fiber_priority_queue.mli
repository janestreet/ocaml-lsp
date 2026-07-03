(** A Fiber-based priority queue with element type ['a] *)

open Import

type 'a t

module Queue_size : sig
  type t =
    { realtime_size : int
    ; background_size : int
    }
end

(** Create a new instance of [t] *)
val create : unit -> 'a t

(** Add a new item with a given priority to the queue, if the queue hasn't been closed.
    Throw [Code_error] otherwise *)
val enqueue : 'a t -> 'a -> Priority.t -> unit Fiber.t

(** Remove the next highest-priority item from the queue. Within a given priority, items
    are returned in the same order in which they were enqueued *)
val dequeue : 'a t -> [ `Ok of 'a | `Eof ] Fiber.t

(** Close the queue for writing and await until all items have been dequeued. *)
val close_and_await_drained : 'a t -> unit Fiber.t

(** Check if there are any items remaining in the queue *)
val is_empty : 'a t -> bool

val is_closed : 'a t -> bool

(** Get the size of the queue, broken down by priorities *)
val size : 'a t -> Queue_size.t
