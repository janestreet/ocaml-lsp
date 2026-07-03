module Fiber = Ocaml_lsp_fiber
open! Core
open! Async
open Fiber.O
open Priority_lsp_executor_lib
module Fiber_async = Ocaml_lsp_fiber_shims.Fiber_async

let test f =
  let fiber = Fiber.of_thunk f in
  Fiber_async.deferred_of_fiber fiber ()
;;

let await t =
  let+ result = Priority_lsp_executor.await_noninterleaving_task t in
  Result.ok result |> Option.value_exn
;;

let get_logging_session () = None

let%expect_test "create instance, run_scheduler, then schedule and await a single task" =
  let%map () =
    test (fun () ->
      let* t = Priority_lsp_executor.create ~get_logging_session in
      let scheduler_fiber () = Priority_lsp_executor.run_scheduler t in
      let work_fiber () =
        let* task_result =
          Priority_lsp_executor.schedule_noninterleaving_task
            t
            ~f:Fn.id
            ~priority:Realtime
        in
        match task_result with
        | Error `Stopped -> failwith "Unexpected stopped"
        | Ok task ->
          let* result = Priority_lsp_executor.await_noninterleaving_task task in
          (match result with
           | Ok value ->
             print_endline [%string "Task result: %{value#Unit}"];
             Priority_lsp_executor.close t
           | Error `Cancelled -> failwith "Task was cancelled"
           | Error (`Exn exn) -> Ocaml_lsp_stdune.Exn_with_backtrace.reraise exn)
      in
      Fiber.fork_and_join_unit scheduler_fiber work_fiber)
  in
  [%expect {| Task result: () |}]
;;

let%expect_test "create instance, schedule and await a single task, then run_scheduler" =
  let%map () =
    test (fun () ->
      let* t = Priority_lsp_executor.create ~get_logging_session in
      let scheduler_fiber () = Priority_lsp_executor.run_scheduler t in
      let work_fiber () =
        let* task_result =
          Priority_lsp_executor.schedule_noninterleaving_task
            t
            ~f:Fn.id
            ~priority:Realtime
        in
        match task_result with
        | Error `Stopped -> failwith "Unexpected stopped"
        | Ok task ->
          let* result = Priority_lsp_executor.await_noninterleaving_task task in
          (match result with
           | Ok value ->
             print_endline [%string "Task result: %{value#Unit}"];
             Priority_lsp_executor.close t
           | Error `Cancelled -> failwith "Task was cancelled"
           | Error (`Exn exn) -> Ocaml_lsp_stdune.Exn_with_backtrace.reraise exn)
      in
      Fiber.fork_and_join_unit work_fiber scheduler_fiber)
  in
  [%expect {| Task result: () |}]
;;

let%expect_test "priority ordering: realtime tasks execute before background tasks, FIFO \
                 within priority (except the very first scheduled task), if we first run \
                 scheduler, then schedule tasks"
  =
  let%map () =
    test (fun () ->
      let* t = Priority_lsp_executor.create ~get_logging_session in
      let execution_order = ref [] in
      let scheduler_fiber () = Priority_lsp_executor.run_scheduler t in
      let work_fiber () =
        let make_task name priority =
          let* task_result =
            Priority_lsp_executor.schedule_noninterleaving_task
              t
              ~f:(fun () -> execution_order := name :: !execution_order)
              ~priority
          in
          match task_result with
          | Error `Stopped -> failwith "Unexpected stopped"
          | Ok task -> Fiber.return task
        in
        (* Schedule all tasks first before awaiting any. Note: The first task
           (background1) will start executing immediately when scheduled, but subsequent
           tasks will be queued according to priority. *)
        let* bg1 = make_task "background1" Background
        and* rt1 = make_task "realtime1" Realtime
        and* bg2 = make_task "background2" Background
        and* rt2 = make_task "realtime2" Realtime
        and* bg3 = make_task "background3" Background
        and* rt3 = make_task "realtime3" Realtime in
        (* Now await in the same order they were scheduled *)
        let* () = await bg1
        and* () = await rt1
        and* () = await bg2
        and* () = await rt2
        and* () = await bg3
        and* () = await rt3 in
        let* () = Priority_lsp_executor.close t in
        (* Print execution order (reversed because we prepended). *)
        List.iter (List.rev !execution_order) ~f:print_endline;
        Fiber.return ()
      in
      Fiber.fork_and_join_unit scheduler_fiber work_fiber)
  in
  (* Expected: background1 executes first (it was scheduled first), then realtime1-3
     (higher priority), then background2-3 (lower priority). *)
  [%expect
    {|
    background1
    realtime1
    realtime2
    realtime3
    background2
    background3
    |}]
;;

let%expect_test "priority ordering: realtime tasks execute before background tasks, FIFO \
                 within priority , if we first schedule tasks, then run scheduler"
  =
  let%map () =
    test (fun () ->
      let* t = Priority_lsp_executor.create ~get_logging_session in
      let execution_order = ref [] in
      let scheduler_fiber () = Priority_lsp_executor.run_scheduler t in
      let work_fiber () =
        let make_task name priority =
          let* task_result =
            Priority_lsp_executor.schedule_noninterleaving_task
              t
              ~f:(fun () -> execution_order := name :: !execution_order)
              ~priority
          in
          match task_result with
          | Error `Stopped -> failwith "Unexpected stopped"
          | Ok task -> Fiber.return task
        in
        (* Schedule all tasks first before awaiting any. Note: The first task
           (background1) will start executing immediately when scheduled, but subsequent
           tasks will be queued according to priority. *)
        let* bg1 = make_task "background1" Background
        and* rt1 = make_task "realtime1" Realtime
        and* bg2 = make_task "background2" Background
        and* rt2 = make_task "realtime2" Realtime
        and* bg3 = make_task "background3" Background
        and* rt3 = make_task "realtime3" Realtime in
        (* Now await in the same order they were scheduled *)
        let* () = await bg1
        and* () = await rt1
        and* () = await bg2
        and* () = await rt2
        and* () = await bg3
        and* () = await rt3 in
        let* () = Priority_lsp_executor.close t in
        (* Print execution order (reversed because we prepended). *)
        List.iter (List.rev !execution_order) ~f:print_endline;
        Fiber.return ()
      in
      Fiber.fork_and_join_unit work_fiber scheduler_fiber)
  in
  (* Expected: tasks execute in FIFO priority order (realtime1-3, then background1-3). *)
  [%expect
    {|
    realtime1
    realtime2
    realtime3
    background1
    background2
    background3
    |}]
;;

let%expect_test "cancel a task" =
  let%map () =
    test (fun () ->
      let* t = Priority_lsp_executor.create ~get_logging_session in
      let scheduler_fiber () = Priority_lsp_executor.run_scheduler t in
      let work_fiber () =
        let* task_result =
          Priority_lsp_executor.schedule_noninterleaving_task
            t
            ~f:(fun () ->
              (* This should not execute *)
              print_endline "Task executed")
            ~priority:Background
        in
        match task_result with
        | Error `Stopped -> failwith "Unexpected stopped"
        | Ok task ->
          let* () = Priority_lsp_executor.cancel_noninterleaving_task task in
          let* result = Priority_lsp_executor.await_noninterleaving_task task in
          (match result with
           | Ok _ -> failwith "Task should have been cancelled"
           | Error `Cancelled ->
             print_endline "Task was cancelled successfully";
             Priority_lsp_executor.close t
           | Error (`Exn exn) -> Ocaml_lsp_stdune.Exn_with_backtrace.reraise exn)
      in
      Fiber.fork_and_join_unit scheduler_fiber work_fiber)
  in
  [%expect {| Task was cancelled successfully |}]
;;

let%expect_test "close after scheduling and awaiting for a task - task executes, no new \
                 tasks allowed"
  =
  let%map () =
    test (fun () ->
      let* t = Priority_lsp_executor.create ~get_logging_session in
      let scheduler_fiber () = Priority_lsp_executor.run_scheduler t in
      let work_fiber () =
        let* task_result =
          Priority_lsp_executor.schedule_noninterleaving_task
            t
            ~f:(fun () -> print_endline "Task executed")
            ~priority:Realtime
        in
        match task_result with
        | Error `Stopped -> failwith "Unexpected stopped"
        | Ok task ->
          let* result = Priority_lsp_executor.await_noninterleaving_task task in
          (match result with
           | Ok value ->
             print_endline [%string "Task result: %{value#Unit}"];
             let* () = Priority_lsp_executor.close t in
             (* Try to schedule a new task after closing *)
             let* new_task_result =
               Priority_lsp_executor.schedule_noninterleaving_task
                 t
                 ~f:Fn.id
                 ~priority:Realtime
             in
             (match new_task_result with
              | Error `Stopped ->
                print_endline "Cannot schedule tasks after close";
                Fiber.return ()
              | Ok _ -> failwith "Should not be able to schedule after close")
           | Error `Cancelled -> failwith "Task should not be cancelled"
           | Error (`Exn exn) -> Ocaml_lsp_stdune.Exn_with_backtrace.reraise exn)
      in
      Fiber.fork_and_join_unit scheduler_fiber work_fiber)
  in
  [%expect
    {|
    Task executed
    Task result: ()
    Cannot schedule tasks after close
    |}]
;;

let%expect_test "close after scheduling, but before awaiting for a task - task executes, \
                 no new tasks allowed"
  =
  let%map () =
    test (fun () ->
      let* t = Priority_lsp_executor.create ~get_logging_session in
      let scheduler_fiber () = Priority_lsp_executor.run_scheduler t in
      let work_fiber () =
        let* scheduling_result =
          Priority_lsp_executor.schedule_noninterleaving_task
            t
            ~f:(fun () -> print_endline "Task executed")
            ~priority:Realtime
        in
        match scheduling_result with
        | Error `Stopped -> failwith "Unexpected stopped"
        | Ok task ->
          print_endline "Task successfully scheduled";
          let* () = Priority_lsp_executor.close t in
          print_endline "Priority_lsp_executor is closed";
          (* Try to schedule a new task after closing *)
          let* new_task_result =
            Priority_lsp_executor.schedule_noninterleaving_task
              t
              ~f:Fn.id
              ~priority:Realtime
          in
          let () =
            match new_task_result with
            | Error `Stopped -> print_endline "Cannot schedule tasks after close"
            | Ok _ -> failwith "Should not be able to schedule after close"
          in
          let* result = Priority_lsp_executor.await_noninterleaving_task task in
          let () =
            match result with
            | Ok value -> print_endline [%string "Task result: %{value#Unit}"]
            | Error `Cancelled -> failwith "Task should not be cancelled"
            | Error (`Exn exn) -> Ocaml_lsp_stdune.Exn_with_backtrace.reraise exn
          in
          Fiber.return ()
      in
      Fiber.fork_and_join_unit scheduler_fiber work_fiber)
  in
  [%expect
    {|
    Task successfully scheduled
    Priority_lsp_executor is closed
    Cannot schedule tasks after close
    Task executed
    Task result: ()
    |}]
;;

let%expect_test "close without scheduling any tasks - scheduling not possible" =
  let%map () =
    test (fun () ->
      let* t = Priority_lsp_executor.create ~get_logging_session in
      let scheduler_fiber () = Priority_lsp_executor.run_scheduler t in
      let work_fiber () =
        let* () = Priority_lsp_executor.close t in
        print_endline "Closed successfully";
        (* Try to schedule a task after closing *)
        let* task_result =
          Priority_lsp_executor.schedule_noninterleaving_task
            t
            ~f:Fn.id
            ~priority:Realtime
        in
        match task_result with
        | Error `Stopped ->
          print_endline "Cannot schedule tasks after close";
          Fiber.return ()
        | Ok _ -> failwith "Should not be able to schedule after close"
      in
      Fiber.fork_and_join_unit scheduler_fiber work_fiber)
  in
  [%expect
    {|
    Closed successfully
    Cannot schedule tasks after close
    |}]
;;

let%expect_test "task canceled during execution does not cause double ivar fill" =
  let%map () =
    test (fun () ->
      let task_should_finish = ref false in
      let task_started = ref false in
      let wait_for_task_should_finish () =
        task_started := true;
        prerr_endline "Task: started execution";
        (* NB: the iteration counter [i] exists so that if things are broken, test fails
           rather than hangs. Hung tests are killed after a timeout, but they do not
           produce output, and therefore are hard to debug *)
        let i = ref 0 in
        while (not !task_should_finish) && !i < 10 do
          i := !i + 1;
          let _ = Core_unix.nanosleep 0.1 in
          ()
        done;
        prerr_endline "Task: finished execution"
      in
      let* t = Priority_lsp_executor.create ~get_logging_session in
      let scheduler_fiber =
        Fiber.of_thunk (fun () -> Priority_lsp_executor.run_scheduler t)
      in
      let cancel_task_once_started task =
        Fiber.of_thunk (fun () ->
          (* NB: the iteration counter [i] exists so that if things are broken, test fails
             rather than hangs. Hung tests are killed after a timeout, but they do not
             produce output, and therefore are hard to debug *)
          let i = ref 0 in
          let rec loop () =
            if (not !task_started) && !i < 10
            then (
              i := !i + 1;
              let open Lev_fiber_async in
              let* () = Lev_fiber.Timer.sleepf 0.1 in
              loop ())
            else Fiber.return ()
          in
          let* () = loop () in
          let* () = Priority_lsp_executor.cancel_noninterleaving_task task in
          task_should_finish := true;
          Fiber.return ())
      in
      let work_fiber =
        Fiber.of_thunk (fun () ->
          let* task_result =
            Priority_lsp_executor.schedule_noninterleaving_task
              t
              ~f:wait_for_task_should_finish
              ~priority:Realtime
          in
          match task_result with
          | Error `Stopped -> failwith "Unexpected stopped"
          | Ok task ->
            let* () = cancel_task_once_started task in
            let* result = Priority_lsp_executor.await_noninterleaving_task task in
            (match result with
             | Ok value ->
               print_endline [%string "Unexpectesd: Task result: %{value#Unit}"];
               Priority_lsp_executor.close t
             | Error `Cancelled ->
               print_endline [%string "Expected: Task was cancelled"];
               Priority_lsp_executor.close t
             | Error (`Exn exn) -> Ocaml_lsp_stdune.Exn_with_backtrace.reraise exn))
      in
      Fiber.fork_and_join_unit (fun () -> scheduler_fiber) (fun () -> work_fiber))
  in
  [%expect
    {|
    Task: started execution
    Expected: Task was cancelled
    Task: finished execution
    |}]
;;
