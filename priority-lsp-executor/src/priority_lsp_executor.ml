open! Import
open Fiber.O
open Lev_fiber_async
module Exn_with_backtrace = Ocaml_lsp_stdune.Exn_with_backtrace

type 'a task =
  { f : unit -> 'a
  ; ivar : ('a, [ `Exn of Exn_with_backtrace.t | `Cancelled ]) Result.t Fiber.Ivar.t
  }

type packed_task = Task : 'a task -> packed_task

type t =
  { tasks : packed_task Fiber_priority_queue.t
  ; executor : Lev_fiber.Thread.t
  ; get_logging_session : unit -> Ocaml_lsp_logging.t option
  (* The executor is created by [Ocaml_lsp_server.start], so the [logging_session],
     created by [Ocaml_lsp_server.on_initialize] is not yet available. *)
  }

let log_queue_stats t action =
  let { realtime_size; background_size } : Fiber_priority_queue.Queue_size.t =
    Fiber_priority_queue.size t.tasks
  in
  let total_size = background_size + realtime_size in
  Ocaml_lsp_logging.log_queue_stats
    ~action
    ~realtime_size
    ~background_size
    ~total_size
    (t.get_logging_session ())
;;

let fill_if_empty
  (ivar : ('a, [ `Exn of Exn_with_backtrace.t | `Cancelled ]) Result.t Fiber.Ivar.t)
  (f : unit -> ('a, [ `Exn of Exn_with_backtrace.t | `Cancelled ]) Result.t Fiber.t)
  =
  let* maybe_filled = Fiber.Ivar.peek ivar in
  match maybe_filled with
  | None ->
    let* v = f () in
    let* maybe_filled_while_executing = Fiber.Ivar.peek ivar in
    (match maybe_filled_while_executing with
     | None -> Fiber.Ivar.fill ivar v
     | Some (Ok _) ->
       (* This should be impossible and is only included for match exhaustiveness *)
       Code_error.raise_s
         [%message "BUG: Task ivar was filled with an Ok result while being executed"]
     | Some (Error (`Exn _)) ->
       (* This should be impossible and is only included for match exhaustiveness *)
       Code_error.raise_s
         [%message "BUG: Task ivar was filled with an `Exn result while being executed"]
     | Some (Error `Cancelled) ->
       (* This happens when task cancellation happened during execution. It is a perfectly
          possible scenario and we should just discard any execution results in this case. *)
       Fiber.return ())
  | Some _ -> Fiber.return ()
;;

let schedule_and_await_single_task (type a) t (f : unit -> a) ()
  : (a, [ `Exn of Exn_with_backtrace.t | `Cancelled ]) Result.t Fiber.t
  =
  let inner_task = Lev_fiber.Thread.task t.executor ~f in
  match inner_task with
  | Ok inner_task -> Lev_fiber.Thread.await inner_task
  | Error `Stopped ->
    (* This should really not happen by design: [task] will reject new tasks once the
       inner [Fiber_priority_queue.t] is closed, and the inner [Lev_fiber.Thread.t] is
       closed only once the queue is both closed and drained. So this should be
       unreachable. *)
    Code_error.raise_s
      [%message "BUG: Executor is stopped, while there are tasks in the queue"]
;;

let run_scheduler t =
  let rec loop () =
    let* dequeued = Fiber_priority_queue.dequeue t.tasks in
    match dequeued with
    | `Eof ->
      let () = log_queue_stats t "just after dequeue: Eof" in
      Lev_fiber.Thread.close t.executor;
      Fiber.return ()
    | `Ok (Task { f; ivar }) ->
      let () = log_queue_stats t "just after dequeue: Next task" in
      let* () = fill_if_empty ivar (schedule_and_await_single_task t f) in
      (loop [@tailcall]) ()
  in
  (loop [@tailcall]) ()
;;

let create ~get_logging_session =
  let tasks = Fiber_priority_queue.create () in
  let+ executor = Lev_fiber.Thread.create () in
  { tasks; executor; get_logging_session }
;;

let schedule_noninterleaving_task t ~f ~priority =
  if Fiber_priority_queue.is_closed t.tasks
  then Fiber.return (Error `Stopped)
  else (
    let ivar = Fiber.Ivar.create () in
    let task = { f; ivar } in
    let pt = Task task in
    let+ () = Fiber_priority_queue.enqueue t.tasks pt priority in
    let () = log_queue_stats t "just after enqueue" in
    Ok task)
;;

let cancel_noninterleaving_task task =
  fill_if_empty task.ivar (fun () -> Fiber.return (Error `Cancelled))
;;

let await_noninterleaving_task task = Fiber.Ivar.read task.ivar
let close t = Fiber_priority_queue.close_and_await_drained t.tasks

module For_testing = struct
  module Fiber_priority_queue = Fiber_priority_queue
end
