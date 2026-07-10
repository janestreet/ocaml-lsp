open! Import
open Fiber.O
open Core

type 'a t =
  { realtime : 'a Queue.t
  ; background : 'a Queue.t
  ; waiting_readers : [ `Ok of 'a | `Eof ] Fiber.Ivar.t Queue.t
  ; mutable closed : bool
  ; drained : unit Fiber.Ivar.t
  }

module Queue_size = struct
  type t =
    { realtime_size : int
    ; background_size : int
    }
end

let create () =
  { realtime = Queue.create ()
  ; background = Queue.create ()
  ; waiting_readers = Queue.create ()
  ; closed = false
  ; drained = Fiber.Ivar.create ()
  }
;;

let is_empty t = Queue.is_empty t.background && Queue.is_empty t.realtime
let is_closed t = t.closed

let size t : Queue_size.t =
  { realtime_size = Queue.length t.realtime; background_size = Queue.length t.background }
;;

let finalize_draining t =
  let* drained_already = Fiber.Ivar.peek t.drained in
  match drained_already with
  | Some () -> Fiber.return ()
  | None ->
    let waiting_readers = Queue.to_list t.waiting_readers in
    let () = Queue.clear t.waiting_readers in
    let* () =
      Fiber.parallel_iter waiting_readers ~f:(fun wr_ivar -> Fiber.Ivar.fill wr_ivar `Eof)
    in
    let* () = Fiber.Ivar.fill t.drained () in
    Fiber.return ()
;;

let maybe_finalize_draining t =
  if t.closed && Queue.is_empty t.realtime && Queue.is_empty t.background
  then finalize_draining t
  else Fiber.return ()
;;

let enqueue t item priority =
  if t.closed
  then
    Code_error.raise_s [%message "Trying to enqueue into a closed Fiber_priority_queue"]
  else (
    match Queue.dequeue t.waiting_readers with
    | Some wr_ivar -> Fiber.Ivar.fill wr_ivar (`Ok item)
    | None ->
      let q =
        match priority with
        | Priority.Background -> t.background
        | Priority.Realtime -> t.realtime
      in
      Queue.enqueue q item;
      Fiber.return ())
;;

let dequeue t =
  let* ret =
    match Queue.dequeue t.realtime with
    | Some realtime_item -> Fiber.return (`Ok realtime_item)
    | None ->
      (match Queue.dequeue t.background with
       | Some background_item -> Fiber.return (`Ok background_item)
       | None ->
         if t.closed
         then Fiber.return `Eof
         else (
           let wr_ivar = Fiber.Ivar.create () in
           let () = Queue.enqueue t.waiting_readers wr_ivar in
           Fiber.Ivar.read wr_ivar))
  in
  let* () = maybe_finalize_draining t in
  Fiber.return ret
;;

let close_and_await_drained t =
  t.closed <- true;
  let* () = maybe_finalize_draining t in
  Fiber.Ivar.read t.drained
;;
