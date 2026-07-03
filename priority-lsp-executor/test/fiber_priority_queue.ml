module Fiber = Ocaml_lsp_fiber
open Fiber.O

module Fpq =
  Priority_lsp_executor_lib.Priority_lsp_executor.For_testing.Fiber_priority_queue

let sexp_of_unit () = Core.Sexp.List []

let%expect_test "enqueue and dequeue single item" =
  let run () =
    let queue = Fpq.create () in
    let* () = Fpq.enqueue queue "item1" Realtime in
    let* result = Fpq.dequeue queue in
    match result with
    | `Ok item ->
      print_endline item;
      Fiber.return ()
    | `Eof -> failwith "Unexpected EOF"
  in
  Fiber_test.test sexp_of_unit run;
  [%expect
    {|
    item1
    ()
    |}]
;;

let%expect_test "enqueue Realtime and Background items - Realtime comes first" =
  let run () =
    let queue = Fpq.create () in
    let* () = Fpq.enqueue queue "background_item" Background in
    let* () = Fpq.enqueue queue "realtime_item" Realtime in
    let* result1 = Fpq.dequeue queue in
    let* result2 = Fpq.dequeue queue in
    (match result1 with
     | `Ok item -> print_endline [%string "First: %{item}"]
     | `Eof -> failwith "First: Unexpected EOF");
    (match result2 with
     | `Ok item -> print_endline [%string "Second: %{item}"]
     | `Eof -> failwith "Second: Unexpected EOF");
    Fiber.return ()
  in
  Fiber_test.test sexp_of_unit run;
  [%expect
    {|
    First: realtime_item
    Second: background_item
    ()
    |}]
;;

let%expect_test "close_and_await_drained with empty queue" =
  let run () =
    let queue = Fpq.create () in
    let* () = Fpq.close_and_await_drained queue in
    print_endline "Queue drained successfully";
    Fiber.return ()
  in
  Fiber_test.test sexp_of_unit run;
  [%expect
    {|
    Queue drained successfully
    ()
    |}]
;;

let%expect_test "close_and_await_drained waits for items to be dequeued" =
  let run () =
    let queue = Fpq.create () in
    let* () = Fpq.enqueue queue "item1" Realtime in
    let* () = Fpq.enqueue queue "item2" Background in
    let drain_fiber () =
      let* () = Fpq.close_and_await_drained queue in
      print_endline "Queue drained";
      Fiber.return ()
    in
    let dequeue_fiber () =
      let* result1 = Fpq.dequeue queue in
      (match result1 with
       | `Ok item -> print_endline [%string "Dequeued: %{item}"]
       | `Eof -> failwith "Dequeued: EOF");
      let* result2 = Fpq.dequeue queue in
      (match result2 with
       | `Ok item -> print_endline [%string "Dequeued: %{item}"]
       | `Eof -> failwith "Dequeued: EOF");
      Fiber.return ()
    in
    Fiber.fork_and_join_unit drain_fiber dequeue_fiber
  in
  Fiber_test.test sexp_of_unit run;
  [%expect
    {|
    Dequeued: item1
    Dequeued: item2
    Queue drained
    ()
    |}]
;;

let%expect_test "dequeue returns Eof after close_and_await_drained" =
  let run () =
    let queue = Fpq.create () in
    let* () = Fpq.enqueue queue "item1" Realtime in
    let drain_and_dequeue () =
      let* result1 = Fpq.dequeue queue in
      (match result1 with
       | `Ok item -> print_endline [%string "First dequeue: %{item}"]
       | `Eof -> failwith "First dequeue: EOF");
      let* () = Fpq.close_and_await_drained queue in
      print_endline "Queue closed";
      let* result2 = Fpq.dequeue queue in
      (match result2 with
       | `Ok item -> failwith [%string "Second dequeue: %{item}"]
       | `Eof -> print_endline "Second dequeue: EOF");
      Fiber.return ()
    in
    drain_and_dequeue ()
  in
  Fiber_test.test sexp_of_unit run;
  [%expect
    {|
    First dequeue: item1
    Queue closed
    Second dequeue: EOF
    ()
    |}]
;;

let%expect_test "mixed priority items - all Realtime before Background, FIFO order \
                 within priority"
  =
  let run () =
    let queue = Fpq.create () in
    let* () = Fpq.enqueue queue "background1" Background in
    let* () = Fpq.enqueue queue "realtime1" Realtime in
    let* () = Fpq.enqueue queue "background2" Background in
    let* () = Fpq.enqueue queue "realtime2" Realtime in
    let rec dequeue_all acc =
      let* result = Fpq.dequeue queue in
      match result with
      | `Ok item -> dequeue_all (item :: acc)
      | `Eof -> Fiber.return (List.rev acc)
    in
    let close_fiber () = Fpq.close_and_await_drained queue in
    let dequeue_fiber () = dequeue_all [] in
    let* items, () = Fiber.fork_and_join dequeue_fiber close_fiber in
    Core.List.iter items ~f:print_endline;
    Fiber.return ()
  in
  Fiber_test.test sexp_of_unit run;
  [%expect
    {|
    realtime1
    realtime2
    background1
    background2
    ()
    |}]
;;
