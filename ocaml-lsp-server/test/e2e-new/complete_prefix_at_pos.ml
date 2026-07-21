open Async
open Test.Import

let print_complete_prefix_at_pos
  (source : string)
  (position : Position.t)
  (prefix : string)
  =
  let makeRequest (textDocument : TextDocumentIdentifier.t) =
    Lsp.Client_request.UnknownRequest
      { meth = Lsp.Client_request.Custom_request_names.complete_prefix_at_pos
      ; params =
          Some
            (`Assoc
              [ "textDocument", TextDocumentIdentifier.yojson_of_t textDocument
              ; "position", Position.yojson_of_t position
              ; "prefix", `String prefix
              ])
      }
  in
  Lsp_helpers.iter_lsp_response ~makeRequest ~source (fun x ->
    print_endline (Yojson.Safe.pretty_to_string x))
;;

let%expect_test "Serialization works as expected" =
  let serialize =
    Ocaml_lsp_server.Custom_request.Complete_prefix_at_pos.For_testing.serialize
  in
  let print j = Yojson.Safe.pretty_to_string j |> print_endline in
  let completions =
    [ CompletionItem.t_of_yojson
        (`Assoc [ "detail", `String "'a option -> bool"; "label", `String "is_none" ])
    ]
  in
  let labels = [ "~key", "'_weak1"; "~data", "'_weak1" ] in
  print (serialize (completions, None));
  [%expect
    {|
    {
      "entries": [ { "detail": "'a option -> bool", "label": "is_none" } ],
      "context": null
    }
    |}];
  print (serialize (completions, Some []));
  [%expect
    {|
    {
      "entries": [ { "detail": "'a option -> bool", "label": "is_none" } ],
      "context": [ "application", { "labels": [] } ]
    }
    |}];
  print (serialize (completions, Some labels));
  [%expect
    {|
    {
      "entries": [ { "detail": "'a option -> bool", "label": "is_none" } ],
      "context": [
        "application",
        {
          "labels": [
            { "name": "~key", "type": "'_weak1" },
            { "name": "~data", "type": "'_weak1" }
          ]
        }
      ]
    }
    |}];
  return ()
;;

let%expect_test "Can complete with prefix in buffer at position" =
  let source =
    {ocaml|
let go =
  print_endline
  @@ if Option.is_ (Some "blah")
  then "yes" else "no"
;;
|ocaml}
  in
  let position = Position.create ~line:4 ~character:18 in
  let%map () = print_complete_prefix_at_pos source position "Option.is_" in
  [%expect
    {|
    {
      "entries": [
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_none",
          "sortText": "0000",
          "textEdit": {
            "newText": "is_none",
            "range": {
              "start": { "line": 4, "character": 18 },
              "end": { "line": 4, "character": 18 }
            }
          }
        },
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_some",
          "sortText": "0001",
          "textEdit": {
            "newText": "is_some",
            "range": {
              "start": { "line": 4, "character": 18 },
              "end": { "line": 4, "character": 18 }
            }
          }
        }
      ],
      "context": null
    }
    |}]
;;

let%expect_test "Can complete with prefix not in buffer at position" =
  let source =
    {ocaml|
let go =
  print_endline
  @@ if  (Some "blah")
  then "yes" else "no"
;;
|ocaml}
  in
  let position = Position.create ~line:4 ~character:8 in
  let%map () = print_complete_prefix_at_pos source position "Option.is_" in
  [%expect
    {|
    {
      "entries": [
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_none",
          "sortText": "0000",
          "textEdit": {
            "newText": "is_none",
            "range": {
              "start": { "line": 4, "character": 8 },
              "end": { "line": 4, "character": 8 }
            }
          }
        },
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_some",
          "sortText": "0001",
          "textEdit": {
            "newText": "is_some",
            "range": {
              "start": { "line": 4, "character": 8 },
              "end": { "line": 4, "character": 8 }
            }
          }
        }
      ],
      "context": null
    }
    |}]
;;

let%expect_test "Can complete with different prefix in buffer at position" =
  let source =
    {ocaml|
let go =
  print_endline
  @@ if Or_error.is_some (Some "blah")
  then "yes" else "no"
;;
|ocaml}
  in
  let position = Position.create ~line:4 ~character:8 in
  let%map () = print_complete_prefix_at_pos source position "Option.is_" in
  [%expect
    {|
    {
      "entries": [
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_none",
          "sortText": "0000",
          "textEdit": {
            "newText": "is_none",
            "range": {
              "start": { "line": 4, "character": 8 },
              "end": { "line": 4, "character": 8 }
            }
          }
        },
        {
          "detail": "'a option -> bool",
          "kind": 12,
          "label": "is_some",
          "sortText": "0001",
          "textEdit": {
            "newText": "is_some",
            "range": {
              "start": { "line": 4, "character": 8 },
              "end": { "line": 4, "character": 8 }
            }
          }
        }
      ],
      "context": null
    }
    |}]
;;

let%expect_test "Completion can return function application context" =
  let source =
    {ocaml|
open Core
let (map : int String.Map.t) = String.Map.singleton "foo" 1
String.Map.add map ~key
;;
|ocaml}
  in
  let position = Position.create ~line:3 ~character:23 in
  let%map () = print_complete_prefix_at_pos source position "~key" in
  [%expect
    {|
    {
      "entries": [],
      "context": [
        "application", { "labels": [ { "name": "~key", "type": "'a" } ] }
      ]
    }
    |}]
;;
