module Fiber = Ocaml_lsp_fiber
open Async
open Test.Import

let print_diagnostics
  ?(prep = fun _ -> Fiber.return ())
  ?(print_range : bool = false)
  ?(path : string = "foo.ml")
  ?cwd
  ?extra_env
  (source : string)
  =
  Lsp_helpers.open_document_with_diagnostics_callback
    ~prep
    ~path
    ?cwd
    ?extra_env
    ~source
    ~diagnostics_callback:(fun diagnostics ->
      print_endline
        (String.concat
           ~sep:", "
           (List.map diagnostics.diagnostics ~f:(fun (d : Diagnostic.t) ->
              let range_message =
                if print_range
                then "\n" ^ Ocaml_lsp_server.Testing.Range.to_string d.range ^ "\n"
                else ""
              in
              match d.message with
              | `String m -> m ^ range_message
              | `MarkupContent { value; _ } -> value ^ range_message))))
    ()
;;

let change_config client params = Client.notification client (ChangeConfiguration params)

(* Create a synthetic workspace directory containing [dune-workspace] and [jenga.conf]
   markers, run [f] with its absolute path, then remove it.

   This lets us run the LSP server with a cwd whose filesystem state we control, so that
   merlin's project-config discovery (which walks up the filesystem looking for
   [dune-*]/[.merlin] files and [jenga.conf]) behaves deterministically regardless of how
   or where the test is invoked (jenga, dune with or without isolation, etc.). *)
let with_synthetic_jenga_dune_workspace f =
  Expect_test_helpers_async.with_temp_dir (fun tmp ->
    let open Deferred.Let_syntax in
    let touch name = Writer.save (Filename.concat tmp name) ~contents:"" in
    let%bind () = touch "dune-workspace" in
    let%bind () = touch "jenga.conf" in
    f tmp)
;;

let%expect_test "receiving diagnostics" =
  let source =
    {ocaml|
let x = Foo.oh_no
let y = garbage
;;
|ocaml}
  in
  let%map () =
    with_synthetic_jenga_dune_workspace (fun workspace_root ->
      (* [BUILD_SYSTEM_DISCOVER_ROOT_STOP_DIR] prevents [Jenga_rules_integration] from
         walking past our synthetic workspace root if, for some reason, an ancestor of the
         temp dir also contains a [jenga.conf]. *)
      print_diagnostics
        ~cwd:workspace_root
        ~extra_env:[ "BUILD_SYSTEM_DISCOVER_ROOT_STOP_DIR=" ^ workspace_root ]
        ~path:"../this-directory-does-not-exist/foo.ml"
        source)
  in
  [%expect
    {| Could not find `.merlin` files to load project configuration. It appears you are in a Jenga/Dune workspace. To get full Merlin support for this file, build the relevant target using Jenga or Dune. This is usually the default target for the directory containing this file. |}]
;;

let%expect_test "doesn't add other diagnostics if syntax errors" =
  let source =
    {ocaml|
  let x = "" in

  let () = 1
  |ocaml}
  in
  let%map () = print_diagnostics source in
  [%expect {| Expecting `in' to continue let-binding at line 4, character 11 |}]
;;

let%expect_test "shorten diagnostics" =
  let source =
    {ocaml|
    let x: unit = fun () ->




      ()

    let () = match true with

    | false -> ()
  |ocaml}
  in
  let req enable =
    print_diagnostics
      ~prep:(fun client ->
        change_config
          client
          (DidChangeConfigurationParams.create
             ~settings:
               (`Assoc [ "shortenMerlinDiagnostics", `Assoc [ "enable", `Bool enable ] ])))
      ~print_range:true
      source
  in
  let%bind.Deferred () = req true in
  let%bind.Deferred () = req false in
  [%expect
    {|
    This expression should not be a function, the expected type is
    unit
    ((1, 18), (2, 0))
    , Warning 8: this pattern-matching is not exhaustive.
      Here is an example of a case that is not matched: true
    ((8, 13), (9, 0))

    This expression should not be a function, the expected type is
    unit
    ((1, 18), (6, 8))
    , Warning 8: this pattern-matching is not exhaustive.
      Here is an example of a case that is not matched: true
    ((8, 13), (10, 17))
    |}];
  return ()
;;

let prep_which_errors ~syntax ~typing client =
  change_config
    client
    (DidChangeConfigurationParams.create
       ~settings:
         (`Assoc
           [ ( "whichDiagnostics"
             , `Assoc
                 [ "merlin_syntax", `Bool syntax
                 ; "merlin_typing", `Bool typing
                 ; "dune", `Bool true
                 ] )
           ]))
;;

let has_syntax_and_typing_errors =
  {ocaml|
    let _ = 2 * [] in
    let   = 2 in
    2;
    |ocaml}
;;

let has_typing_errors =
  {ocaml|
    let _ = 2 * [] in
    let x = 2 in
    x;
    |ocaml}
;;

let%expect_test "syntax enabled, typing enabled" =
  let req source =
    print_diagnostics
      ~prep:(prep_which_errors ~syntax:true ~typing:true)
      ~print_range:true
      source
  in
  let%bind.Deferred () = req has_syntax_and_typing_errors in
  [%expect
    {|
    Syntax error
    ((2, 10), (2, 11))
    |}];
  let%bind.Deferred () = req has_typing_errors in
  [%expect
    {|
    The constructor [] has type 'a list but an expression was expected of type
      int
    ((1, 16), (1, 18))
    |}];
  return ()
;;

let%expect_test "syntax enabled, typing disabled" =
  let req source =
    print_diagnostics
      ~prep:(prep_which_errors ~syntax:true ~typing:false)
      ~print_range:true
      source
  in
  let%bind.Deferred () = req has_syntax_and_typing_errors in
  [%expect
    {|
    Syntax error
    ((2, 10), (2, 11))
    |}];
  let%bind.Deferred () = req has_typing_errors in
  [%expect {| |}];
  return ()
;;

let%expect_test "syntax disabled, typing enabled" =
  let req source =
    print_diagnostics
      ~prep:(prep_which_errors ~syntax:false ~typing:true)
      ~print_range:true
      source
  in
  let%bind.Deferred () = req has_syntax_and_typing_errors in
  [%expect {| |}];
  let%bind.Deferred () = req has_typing_errors in
  [%expect
    {|
    The constructor [] has type 'a list but an expression was expected of type
      int
    ((1, 16), (1, 18))
    |}];
  return ()
;;

let%expect_test "syntax disabled, typing disabled" =
  let req source =
    print_diagnostics
      ~prep:(prep_which_errors ~syntax:false ~typing:false)
      ~print_range:true
      source
  in
  let%bind.Deferred () = req has_syntax_and_typing_errors in
  [%expect {| |}];
  let%bind.Deferred () = req has_typing_errors in
  [%expect {| |}];
  return ()
;;

(* Lrgrep errors *)

let%expect_test "lrgrep: let with extra semicolon" =
  let source =
    {ocaml|
let x = 5;
let y = 6
let z = 7
  |ocaml}
  in
  let%map () = print_diagnostics source in
  [%expect {| Syntax error: might be due to the semicolon line 2, character 9 |}]
;;

let%expect_test "lrgrep: forgot `in' in let binding" =
  let source =
    {ocaml|
let f a b =
  if a = 0 then b
  else (
    let x =
      match b with
      | None -> a
      | Some b -> b * a
    x * a
  )
  |ocaml}
  in
  let%map () = print_diagnostics source in
  [%expect {| Expecting `in' to continue let-binding at line 9, character 8 |}]
;;

let%expect_test "lrgrep: constructor with multiple arguments" =
  let source =
    {ocaml|
type t = A of int * int

let f x =
  match x with
  | A a b -> a + b
  |ocaml}
  in
  let%map () = print_diagnostics source in
  [%expect
    {| The constructor A expects 2 argument(s), but is applied here to 1 argument(s), Unbound value b |}]
;;

let%expect_test "lrgrep: no pipe before pattern" =
  let source =
    {ocaml|
let f x =
  match x with
  true -> 1
  false -> 0
  |ocaml}
  in
  let%map () = print_diagnostics source in
  [%expect {| Need to put a pipe before pattern in a match statement |}]
;;

let%expect_test "lrgrep: capital type name" =
  let source =
    {ocaml|
type T = x
  |ocaml}
  in
  let%map () = print_diagnostics source in
  [%expect {| Type names must start with a lower-case letter |}]
;;
