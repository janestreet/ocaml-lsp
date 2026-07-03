(** A wrapper around [Lev_fiber.Thread.t] capable of queueing tasks and executing them
    one-by-one according to priorities. NB: Since the tasks are executed one-by-one, the
    next task is not dequeued before the previous one has finished execution. Therefore,
    scheduled tasks *must not* try to schedule-and-await on other tasks, otherwise this
    will deadlock. *)

open Import

type t

(** Create a new system thread and a queue of jobs it will run. *)
val create : get_logging_session:(unit -> Ocaml_lsp_logging.t option) -> t Fiber.t

type 'a task

(** Enqueue a task to the thread using a given priority. *)
val schedule_noninterleaving_task
  :  t
  -> f:(unit -> 'a)
  -> priority:Priority.t
  -> ('a task, [ `Stopped ]) result Fiber.t

(** Cancel a task if it has not run yet. *)
val cancel_noninterleaving_task : 'a task -> unit Fiber.t

(** Wait for a task to finish. *)
val await_noninterleaving_task
  :  'a task
  -> ('a, [ `Exn of Ocaml_lsp_stdune.Exn_with_backtrace.t | `Cancelled ]) result Fiber.t

(** Close the queue, await for it to be drained, then also close the inner [Lev_fiber].
    This willk return the thread to Async's thread pool. After [close] is called, calls to
    [task] will return [Error `Stopped]. *)
val close : t -> unit Fiber.t

val run_scheduler : t -> unit Fiber.t

module For_testing : sig
  module Fiber_priority_queue : module type of Fiber_priority_queue
end
