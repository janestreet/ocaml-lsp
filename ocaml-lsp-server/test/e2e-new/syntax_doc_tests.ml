module Fiber = Ocaml_lsp_fiber
open! Test.Import
open Async

let change_config client params = Client.notification client (ChangeConfiguration params)
let uri = DocumentUri.of_path "test.ml"
let create_postion line character = Position.create ~line ~character

let activate_syntax_doc =
  DidChangeConfigurationParams.create
    ~settings:(`Assoc [ "syntaxDocumentation", `Assoc [ "enable", `Bool true ] ])
;;

let deactivate_syntax_doc =
  DidChangeConfigurationParams.create
    ~settings:(`Assoc [ "syntaxDocumentation", `Assoc [ "enable", `Bool false ] ])
;;

let print_hover hover =
  match hover with
  | None -> print_endline "no hover response"
  | Some hover ->
    hover |> Hover.yojson_of_t |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let hover_req client position =
  Client.request
    client
    (TextDocumentHover
       { HoverParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri
       ; workDoneToken = None
       })
;;

let run_test text req =
  let handler =
    Client.Handler.make
      ~on_notification:(fun client _notification ~event_index:_ ->
        Client.state client;
        Fiber.return ((), None))
      ()
  in
  Test.run ~handler (fun client ->
    let run_client () =
      let capabilities =
        ClientCapabilities.create
          ~textDocument:
            (TextDocumentClientCapabilities.create
               ~hover:(HoverClientCapabilities.create ~contentFormat:[ Markdown ] ())
               ())
          ()
      in
      Client.start client (InitializeParams.create ~capabilities ())
    in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let textDocument =
        TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text
      in
      let* () =
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* () = req client in
      let* () = Client.request client Shutdown in
      Client.stop client
    in
    Fiber.fork_and_join_unit run_client run)
;;

let%expect_test "syntax doc should display" =
  let source =
    {ocaml|
let foo (type a : float64) (x : a) = 42
|ocaml}
  in
  let position = create_postion 1 19 in
  let req client =
    let* () = change_config client activate_syntax_doc in
    let* resp = hover_req client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  let (_ : string) = [%expect.output] in
  let%map () = run_test source req in
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```ocaml\nfloat64 mod external_\n```\n***\n`syntax` Kind abbreviation: The layout of types represented by a 64-bit machine float. See [Manual](https://oxcaml.org/documentation/unboxed-types/intro/)"
      },
      "range": {
        "start": { "line": 1, "character": 18 },
        "end": { "line": 1, "character": 25 }
      }
    }
    |}]
;;

let%expect_test "kind hover should display" =
  let source =
    {ocaml|
type t : immutable_data
|ocaml}
  in
  let position = create_postion 1 11 in
  let req client =
    let* () = change_config client activate_syntax_doc in
    let* resp = hover_req client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  let (_ : string) = [%expect.output] in
  let%map () = run_test source req in
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```ocaml\nvalue non_float mod forkable unyielding many stateless immutable\n```\n***\n`syntax` Kind abbreviation: The kind of types that contain no mutable parts and no functions. See [Manual](https://oxcaml.org/documentation/kinds/syntax/)"
      },
      "range": {
        "start": { "line": 1, "character": 9 },
        "end": { "line": 1, "character": 23 }
      }
    }
    |}]
;;

let%expect_test "kind hover shouldn't display" =
  (* The cursor is on "portable", which isn't an alias, so only the syntax hover should
     display. *)
  let source =
    {ocaml|
type t : value mod portable
|ocaml}
  in
  let position = create_postion 1 24 in
  let req client =
    let* () = change_config client activate_syntax_doc in
    let* resp = hover_req client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  let (_ : string) = [%expect.output] in
  let%map () = run_test source req in
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "`syntax` Mod-bound: Values of types of this kind can cross to `portable` from weaker modes. See [Manual](https://oxcaml.org/documentation/kinds/intro/)"
      },
      "range": {
        "start": { "line": 1, "character": 24 },
        "end": { "line": 1, "character": 24 }
      }
    }
    |}]
;;

let%expect_test "syntax doc should not display" =
  let source =
    {ocaml|
type color = Red|Blue
|ocaml}
  in
  let position = create_postion 1 9 in
  let req client =
    let* () = change_config client deactivate_syntax_doc in
    let* resp = hover_req client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  let%map () = run_test source req in
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```ocaml\ntype color = Red | Blue\n```"
      },
      "range": {
        "start": { "line": 1, "character": 0 },
        "end": { "line": 1, "character": 21 }
      }
    }
    |}]
;;

let%expect_test "syntax doc should print" =
  let source =
    {ocaml|
type t = ..
|ocaml}
  in
  let position = create_postion 1 5 in
  let req client =
    let* () = change_config client activate_syntax_doc in
    let* resp = hover_req client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  let%map () = run_test source req in
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```ocaml\ntype t = ..\n```\n***\n`syntax` Extensible Variant Type: Can be extended with new variant constructors using `+=`. See [Manual](https://ocaml.org/manual/5.2/extensiblevariants.html)"
      },
      "range": {
        "start": { "line": 1, "character": 0 },
        "end": { "line": 1, "character": 11 }
      }
    }
    |}]
;;

let%expect_test "should receive no hover response" =
  let source =
    {ocaml|
  let a = 1
  |ocaml}
  in
  let position = create_postion 1 5 in
  let req client =
    let* () = change_config client activate_syntax_doc in
    let* resp = hover_req client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  let%map () = run_test source req in
  [%expect {| no hover response |}]
;;
