open Core
open Async
open Lev_fiber_async
open Ocaml_lsp_fiber_shims.Fiber_async
module Thread_safe_ivar = Ocaml_lsp_thread_safe_ivar

let%expect_test "Timer.sleepf" =
  let time_source = Time_source.create ~now:Time_ns.epoch () in
  let (module Lev_fiber) =
    Lev_fiber.make ~time_source:(Time_source.read_only time_source)
  in
  let fiber =
    let%map.Fiber () = Lev_fiber.Timer.sleepf 1. in
    print_endline "Done!"
  in
  don't_wait_for (deferred_of_fiber fiber ());
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  [%expect {| |}];
  let%bind () = Time_source.advance_by_alarms_by time_source Time_ns.Span.second in
  [%expect {| Done! |}];
  return ()
;;

let%expect_test "Timer.Wheel.{task,await,cancel}" =
  let time_source = Time_source.create ~now:Time_ns.epoch () in
  let (module Lev_fiber) =
    Lev_fiber.make ~time_source:(Time_source.read_only time_source)
  in
  let fiber =
    let open Fiber.Let_syntax in
    let%bind wheel = Lev_fiber.Timer.Wheel.create ~delay:1. in
    let%bind event1 = Lev_fiber.Timer.Wheel.task wheel in
    let%bind event2 = Lev_fiber.Timer.Wheel.task wheel in
    let%bind () = Lev_fiber.Timer.Wheel.cancel event1 in
    let result1 = Lev_fiber.Timer.Wheel.await event1 in
    let result2 = Lev_fiber.Timer.Wheel.await event2 in
    let%bind () =
      Time_source.advance_by_alarms_by time_source Time_ns.Span.second
      |> fiber_of_deferred
    in
    Fiber.both result1 result2
  in
  let%bind event1, event2 = deferred_of_fiber fiber () in
  print_s [%sexp { event1 : [ `Cancelled | `Ok ]; event2 : [ `Cancelled | `Ok ] }];
  [%expect {| ((event1 Cancelled) (event2 Ok)) |}];
  return ()
;;

let%expect_test "Timer.reset" =
  let time_source = Time_source.create ~now:Time_ns.epoch () in
  let (module Lev_fiber) =
    Lev_fiber.make ~time_source:(Time_source.read_only time_source)
  in
  let fiber =
    let open Fiber.Let_syntax in
    let%bind wheel = Lev_fiber.Timer.Wheel.create ~delay:2. in
    let%bind event = Lev_fiber.Timer.Wheel.task wheel in
    let advance () =
      Time_source.advance_by_alarms_by time_source Time_ns.Span.second
      |> Deferred.map ~f:(fun () -> print_endline "Advanced time.")
      |> fiber_of_deferred
    in
    let await_event () =
      print_endline "Waiting for event...";
      let%map (`Ok | `Cancelled) = Lev_fiber.Timer.Wheel.await event in
      print_endline "Event fired!"
    in
    let reset_event () =
      print_endline "Resetting event.";
      Lev_fiber.Timer.Wheel.reset event
    in
    let result = await_event () in
    let%bind () = advance () in
    let%bind () = advance () in
    let%bind () = result in
    let result = await_event () in
    let%bind () = reset_event () in
    let%bind () = advance () in
    let%bind () = reset_event () in
    let%bind () = advance () in
    let%bind () = advance () in
    result
  in
  let%bind () = deferred_of_fiber fiber () in
  [%expect
    {|
    Waiting for event...
    Advanced time.
    Advanced time.
    Event fired!
    Waiting for event...
    Resetting event.
    Advanced time.
    Resetting event.
    Advanced time.
    Advanced time.
    Event fired!
    |}];
  return ()
;;

let%expect_test "waitpid" =
  let%bind process = Process.create_exn ~prog:"/bin/false" ~args:[] () in
  let fiber = Lev_fiber.waitpid ~pid:(Process.pid process |> Pid.to_int) in
  let%bind () =
    match%map deferred_of_fiber fiber () with
    | WEXITED code -> print_s [%message "Exited" (code : int)]
    | WSIGNALED signal -> print_s [%message "Killed by signal" (signal : int)]
    | WSTOPPED signal -> print_s [%message "Stopped by signal" (signal : int)]
  in
  [%expect {| (Exited (code 1)) |}];
  return ()
;;

let%expect_test "signal" =
  let received_signal_ivar = Ivar.create () in
  let fiber =
    let%map.Fiber () = Lev_fiber.signal ~signal:Signal.(to_caml_int usr1) in
    print_endline "Received signal";
    Ivar.fill_exn received_signal_ivar ()
  in
  don't_wait_for (deferred_of_fiber fiber ());
  let%bind () = Clock_ns.after Time_ns.Span.millisecond in
  [%expect {| |}];
  Signal_unix.send_exn Signal.usr1 (`Pid (Unix.getpid ()));
  let%bind () = Ivar.read received_signal_ivar in
  [%expect {| Received signal |}];
  return ()
;;

let%expect_test "Thread" =
  let fiber =
    let open Fiber.Let_syntax in
    let%bind thread = Lev_fiber.Thread.create () in
    let new_task ~f =
      match Lev_fiber.Thread.task thread ~f with
      | Ok task -> task
      | Error `Stopped -> raise_s [%message "Cannot schedule task - thread is stopped"]
    in
    let ivar = Thread_safe_ivar.create () in
    let task0 =
      (* We use this task to be sure that the tasks that follow it have not been run until
         the ivar is filled. This gives us determinstic cancellation in the test. *)
      new_task ~f:(fun () -> Thread_safe_ivar.read ivar)
    in
    let task1 = new_task ~f:(fun () -> "Task 1") in
    let task2 = new_task ~f:(fun () -> "Task 2") in
    let task3 = new_task ~f:(fun () -> failwith "Task 3 (failed)") in
    let%bind () = Lev_fiber.Thread.cancel task2 in
    Thread_safe_ivar.fill ivar ();
    let%bind result0 = Lev_fiber.Thread.await task0 in
    let%bind result1 = Lev_fiber.Thread.await task1 in
    let%bind result2 = Lev_fiber.Thread.await task2 in
    let%bind result3 = Lev_fiber.Thread.await task3 in
    let open struct
      type exn_with_backtrace = Ocaml_lsp_stdune.Exn_with_backtrace.t =
        { exn : exn
        ; backtrace : (Backtrace.t[@sexp.opaque])
        }
      [@@deriving sexp_of]

      type 'a t = ('a, [ `Exn of exn_with_backtrace | `Cancelled ]) Result.t
      [@@deriving sexp_of]
    end in
    print_s [%message (result0 : unit t)];
    print_s [%message (result1 : string t)];
    print_s [%message (result2 : string t)];
    print_s [%message (result3 : Nothing.t t)];
    Lev_fiber.Thread.close thread;
    Ocaml_lsp_test_lib.Expect_test_helpers.require_does_raise ~here:[%here] (fun () ->
      new_task ~f:ignore);
    return ()
  in
  let%bind () = deferred_of_fiber fiber () in
  [%expect
    {|
    (result0 (Ok ()))
    (result1 (Ok "Task 1"))
    (result2 (Error Cancelled))
    (result3
     (Error (Exn ((exn (Failure "Task 3 (failed)")) (backtrace <opaque>)))))
    "Cannot schedule task - thread is stopped"
    |}];
  return ()
;;

let%expect_test "Io" =
  let fiber =
    let open Fiber.Let_syntax in
    let%bind pipe_r, pipe_w = Lev_fiber.Io.pipe () in
    let read =
      Lev_fiber.Io.with_read pipe_r ~f:(fun reader ->
        let%bind line1 = Lev_fiber.Io.Reader.read_line reader in
        print_s [%sexp (line1 : (string, [ `Partial_eof of string ]) Result.t)];
        let%bind line2 = Lev_fiber.Io.Reader.read_line reader in
        print_s [%sexp (line2 : (string, [ `Partial_eof of string ]) Result.t)];
        let%bind remainder = Lev_fiber.Io.Reader.read_exactly reader 1 in
        print_s [%sexp (remainder : (string, [ `Partial_eof of string ]) Result.t)];
        return ())
    in
    let write =
      Lev_fiber.Io.with_write pipe_w ~f:(fun writer ->
        Lev_fiber.Io.Writer.add_string writer "Hello\n";
        Lev_fiber.Io.Writer.add_string writer "World!\n";
        return ())
    in
    let%bind () = write in
    Lev_fiber.Io.close pipe_w;
    let%map () = read in
    Lev_fiber.Io.close pipe_r
  in
  let%bind () = deferred_of_fiber fiber () in
  [%expect
    {|
    (Ok Hello)
    (Ok World!)
    (Error (Partial_eof ""))
    |}];
  return ()
;;
