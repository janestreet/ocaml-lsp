module Fiber = Ocaml_lsp_fiber
open Async
open Test.Import

let change_config client params = Client.notification client (ChangeConfiguration params)

let codelens client textDocument =
  Client.request
    client
    (TextDocumentCodeLens
       { textDocument; workDoneToken = None; partialResultToken = None })
;;

let%expect_test "disable codelens" =
  let source =
    {ocaml|
let string = "Hello"
|ocaml}
  in
  let req client =
    let text_document = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let* () =
      change_config
        client
        (DidChangeConfigurationParams.create
           ~settings:(`Assoc [ "codelens", `Assoc [ "enable", `Bool false ] ]))
    in
    let* resp_codelens_disabled = codelens client text_document in
    (match resp_codelens_disabled with
     | Some lenses ->
       print_endline ("CodeLens found: " ^ string_of_int (List.length lenses))
     | None -> print_endline "CodeLens response is null");
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect {| CodeLens found: 0 |}]
;;

let%expect_test "enable hover extended" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = Hover_extended.hover client position in
    let () = Hover_extended.print_hover resp in
    let* () =
      change_config
        client
        (DidChangeConfigurationParams.create
           ~settings:(`Assoc [ "extendedHover", `Assoc [ "enable", `Bool true ] ]))
    in
    (* The first hover request has verbosity = 0 *)
    let* _ = Hover_extended.hover client position in
    (* The second hover request has verbosity = 1 *)
    let* resp = Hover_extended.hover client position in
    let () = Hover_extended.print_hover resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "start": { "line": 3, "character": 4 },
        "end": { "line": 3, "character": 13 }
      }
    }
    {
      "contents": {
        "kind": "plaintext",
        "value": "int option\n***\nKind: value non_float mod forkable unyielding many stateless immutable\n***\nMode: @ global portable uncontended read_write stateless unique many forkable unyielding static"
      },
      "range": {
        "start": { "line": 3, "character": 4 },
        "end": { "line": 3, "character": 13 }
      }
    }
    |}]
;;
