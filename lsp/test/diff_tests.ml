module List = Base.List

include struct
  open Lsp.Types
  module Text_document = Lsp.Text_document
  module TextEdit = TextEdit
  module TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
  module TextDocumentItem = TextDocumentItem
  module DidOpenTextDocumentParams = DidOpenTextDocumentParams
end

let printfn a = Printf.ksprintf print_endline a

let test ~from ~to_ =
  let edits = Lsp.Diff.edit ~from ~to_ in
  let json = `List (List.map ~f:TextEdit.yojson_of_t edits) in
  Yojson.Safe.pretty_to_string ~std:false json |> print_endline;
  let textDocument =
    let uri = Lsp.Uri.of_path "/tmp/test" in
    TextDocumentItem.create ~languageId:"test" ~text:from ~uri ~version:1
  in
  let text_document =
    Text_document.make
      ~position_encoding:`UTF8
      (DidOpenTextDocumentParams.create ~textDocument)
  in
  let to_' =
    Text_document.apply_text_document_edits text_document edits |> Text_document.text
  in
  if not @@ String.equal to_ to_'
  then printfn "[FAILURE]\nresult:   %S\nexpected: %S" to_' to_
;;

let%expect_test "empty strings" =
  test ~from:"" ~to_:"";
  [%expect {| [] |}]
;;

let%expect_test "from empty" =
  test ~from:"" ~to_:"foobar";
  [%expect
    {|
    [
      {
        "newText": "foobar",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 0, "character": 0 }
        }
      }
    ]
    |}];
  test ~from:"\n" ~to_:"foobar";
  [%expect
    {|
    [
      {
        "newText": "foobar",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 1, "character": 0 }
        }
      }
    ]
    |}]
;;

let%expect_test "from empty - with newline" =
  test ~from:"" ~to_:"foobar\n";
  [%expect
    {|
    [
      {
        "newText": "foobar\n",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 0, "character": 0 }
        }
      }
    ]
    |}]
;;

let%expect_test "to empty" =
  test ~from:"foobar" ~to_:"";
  [%expect
    {|
    [
      {
        "newText": "",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 1, "character": 0 }
        }
      }
    ]
    |}]
;;

let%expect_test "no change" =
  test ~from:"foobar" ~to_:"foobar";
  [%expect {| [] |}]
;;

let%expect_test "multiline" =
  test ~from:"foo" ~to_:"baz\nbar\nxx\n";
  [%expect
    {|
    [
      {
        "newText": "baz\nbar\nxx\n",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 1, "character": 0 }
        }
      }
    ]
    |}]
;;

let%expect_test "change a character" =
  test ~from:"xxx y xx" ~to_:"xxx z xx";
  [%expect
    {|
    [
      {
        "newText": "xxx z xx",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 1, "character": 0 }
        }
      }
    ]
    |}]
;;

let%expect_test "delete empty line" =
  test ~from:"xxx\n\nyyy\n" ~to_:"xxx\nyyy\n";
  [%expect
    {|
    [
      {
        "newText": "",
        "range": {
          "start": { "line": 1, "character": 0 },
          "end": { "line": 2, "character": 0 }
        }
      }
    ]
    |}]
;;

let%expect_test "regerssion test 1" =
  Printexc.record_backtrace false;
  test
    ~from:
      {|a
y
z
u
|}
    ~to_:
      {|x
y
z
|};
  [%expect
    {|
    [
      {
        "newText": "x\n",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 1, "character": 0 }
        }
      },
      {
        "newText": "",
        "range": {
          "start": { "line": 3, "character": 0 },
          "end": { "line": 4, "character": 0 }
        }
      }
    ]
    |}]
;;

let%expect_test "regerssion test 2" =
  test ~from:"1\nz\n2" ~to_:"2\n1\n";
  [%expect
    {|
    [
      {
        "newText": "2\n",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 0, "character": 0 }
        }
      },
      {
        "newText": "",
        "range": {
          "start": { "line": 1, "character": 0 },
          "end": { "line": 3, "character": 0 }
        }
      }
    ]
    |}]
;;

let%expect_test "regression test 3" =
  (* the diff given here is wierd *)
  test ~from:"\n\n\nXXX\n" ~to_:"\n\nXXX";
  [%expect
    {|
    [
      {
        "newText": "",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 1, "character": 0 }
        }
      },
      {
        "newText": "XXX",
        "range": {
          "start": { "line": 3, "character": 0 },
          "end": { "line": 4, "character": 0 }
        }
      }
    ]
    |}]
;;

let%expect_test "regression test 4" =
  (* the diff given here is wierd *)
  test ~from:"a\n\nb\n\nc\n" ~to_:"a\nb\nc\n";
  [%expect
    {|
    [
      {
        "newText": "",
        "range": {
          "start": { "line": 1, "character": 0 },
          "end": { "line": 2, "character": 0 }
        }
      },
      {
        "newText": "",
        "range": {
          "start": { "line": 3, "character": 0 },
          "end": { "line": 4, "character": 0 }
        }
      }
    ]
    |}]
;;

let%expect_test "regression test 5" =
  test ~from:"\nXXX\n\n" ~to_:"XXX\n";
  [%expect
    {|
    [
      {
        "newText": "",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 1, "character": 0 }
        }
      },
      {
        "newText": "",
        "range": {
          "start": { "line": 2, "character": 0 },
          "end": { "line": 3, "character": 0 }
        }
      }
    ]
    |}]
;;
