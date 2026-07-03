open Ocaml_lsp_server.Diagnostics.For_testing

let%expect_test "remove_errno tests" =
  let test m = remove_errno m |> print_endline in
  (* merlin-style warning *)
  test "Error (warning 26): unused variable a.";
  [%expect {| unused variable a. |}];
  (* dune-style warning *)
  test "(warning 26 [unused-var]): unused variable a.";
  [%expect {| unused variable a. |}]
;;

let%expect_test "equal_message tests" =
  let test e1 e2 expected =
    let result = equal_message e1 e2 in
    if result = expected then print_endline "[PASS]" else print_endline "[FAIL]"
  in
  test "foo bar" "foo  bar" true;
  [%expect {| [PASS] |}];
  test " foobar" "foobar" true;
  [%expect {| [PASS] |}];
  test "foobar" "foobar " true;
  [%expect {| [PASS] |}];
  test "foobar" "foobar\t" true;
  [%expect {| [PASS] |}];
  test "foobar" "foobar\n" true;
  [%expect {| [PASS] |}];
  test "foobar" "foo bar" false;
  [%expect {| [PASS] |}];
  test "foo bar" "foo Bar" false;
  [%expect {| [PASS] |}];
  test
    "Error (warning 26): unused variable a."
    "(warning 26 [unused-var]): unused variable a."
    true;
  [%expect {| [PASS] |}];
  test "foo: bar" "bar" false;
  [%expect {| [PASS] |}]
;;
