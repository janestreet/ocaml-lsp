module Fiber = Ocaml_lsp_fiber
open Async
open Test.Import

let iter_completions
  ?prep
  ?path
  ?(triggerCharacter = "")
  ?(triggerKind = CompletionTriggerKind.Invoked)
  ~position
  =
  let makeRequest textDocument =
    let context = CompletionContext.create ~triggerCharacter ~triggerKind () in
    Lsp.Client_request.TextDocumentCompletion
      (CompletionParams.create ~textDocument ~position ~context ())
  in
  Lsp_helpers.iter_lsp_response ?prep ?path ~makeRequest
;;

let print_completions
  ?(prep = fun _ -> Fiber.return ())
  ?(path = "foo.ml")
  ?(limit = 10)
  ?(pre_print = fun x -> x)
  ?filter_by_prefix
  source
  position
  =
  iter_completions ~prep ~path ~source ~position (function
    | None -> print_endline "No completion Items"
    | Some completions ->
      let items =
        match completions with
        | `CompletionList comp -> comp.items
        | `List comp -> comp
      in
      let items =
        match filter_by_prefix with
        | Some prefix ->
          List.filter items ~f:(fun (item : CompletionItem.t) ->
            String.for_all (String.lowercase prefix) ~f:(fun c ->
              String.contains (String.lowercase item.label) c))
          |> List.sort ~compare:(fun (a : CompletionItem.t) (b : CompletionItem.t) ->
            Int.compare (String.length a.label) (String.length b.label))
        | None -> items
      in
      items
      |> pre_print
      |> (function
       | [] -> print_endline "No completions"
       | items ->
         print_endline "Completions:";
         let originalLength = List.length items in
         Core.List.take items (min limit originalLength)
         |> Core.List.iter ~f:(fun item ->
           item
           |> CompletionItem.yojson_of_t
           |> Yojson.Safe.pretty_to_string ~std:false
           |> print_endline);
         if originalLength > limit then print_endline "............."))
;;

let%expect_test "can start completion at arbitrary position (before the dot)" =
  let source = {ocaml|Strin.func|ocaml} in
  let position = Position.create ~line:0 ~character:5 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "",
      "kind": 9,
      "label": "String",
      "sortText": "0000",
      "textEdit": {
        "newText": "String",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 0, "character": 5 }
        }
      }
    }
    {
      "detail": "",
      "kind": 9,
      "label": "StringLabels",
      "sortText": "0001",
      "textEdit": {
        "newText": "StringLabels",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 0, "character": 5 }
        }
      }
    }
    |}]
;;

let%expect_test "Fuzzy completion enabled (VSCode behavior)" =
  let source = {ocaml|List.mp|ocaml} in
  let position = Position.create ~line:0 ~character:7 in
  let%bind () = print_completions source position in
  [%expect {| No completions |}];
  let prep_vscode_fuzzy_completion client =
    let settings = `Assoc [ "fuzzyCompletion", `Assoc [ "enable", `Bool true ] ] in
    Client.notification client (ChangeConfiguration { settings })
  in
  let%bind () =
    print_completions
      ~prep:prep_vscode_fuzzy_completion
      source
      position
      ~limit:2
      ~filter_by_prefix:"mp"
  in
  [%expect
    {|
    Completions:
    {
      "detail": "('a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "map",
      "sortText": "0044",
      "textEdit": {
        "newText": "map",
        "range": {
          "start": { "line": 0, "character": 5 },
          "end": { "line": 0, "character": 7 }
        }
      }
    }
    {
      "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
      "kind": 12,
      "label": "map2",
      "sortText": "0045",
      "textEdit": {
        "newText": "map2",
        "range": {
          "start": { "line": 0, "character": 5 },
          "end": { "line": 0, "character": 7 }
        }
      }
    }
    .............
    |}];
  let%bind () =
    print_completions
      ~prep:prep_vscode_fuzzy_completion
      {ocaml|Lst|ocaml}
      position
      ~limit:2
      ~filter_by_prefix:"Lst"
  in
  [%expect
    {|
    Completions:
    {
      "detail": "",
      "kind": 9,
      "label": "List",
      "sortText": "0257",
      "textEdit": {
        "newText": "List",
        "range": {
          "start": { "line": 0, "character": 4 },
          "end": { "line": 0, "character": 7 }
        }
      }
    }
    {
      "detail": "type ('a : value_or_null) list = [] | (::) of 'a * 'a list",
      "kind": 25,
      "label": "list",
      "sortText": "0344",
      "textEdit": {
        "newText": "list",
        "range": {
          "start": { "line": 0, "character": 4 },
          "end": { "line": 0, "character": 7 }
        }
      }
    }
    .............
    |}];
  let%bind () =
    print_completions
      ~prep:prep_vscode_fuzzy_completion
      {ocaml|lst|ocaml}
      position
      ~limit:2
      ~filter_by_prefix:"lst"
  in
  [%expect
    {|
    Completions:
    {
      "detail": "type ('a : value_or_null) list = [] | (::) of 'a * 'a list",
      "kind": 25,
      "label": "list",
      "sortText": "0273",
      "textEdit": {
        "newText": "list",
        "range": {
          "start": { "line": 0, "character": 4 },
          "end": { "line": 0, "character": 7 }
        }
      }
    }
    {
      "detail": "",
      "kind": 9,
      "label": "List",
      "sortText": "0317",
      "textEdit": {
        "newText": "List",
        "range": {
          "start": { "line": 0, "character": 4 },
          "end": { "line": 0, "character": 7 }
        }
      }
    }
    .............
    |}];
  Deferred.unit
;;

let%expect_test "can start completion at arbitrary position" =
  let source = {ocaml|StringLabels|ocaml} in
  let position = Position.create ~line:0 ~character:6 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "",
      "kind": 9,
      "label": "String",
      "sortText": "0000",
      "textEdit": {
        "newText": "String",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 0, "character": 6 }
        }
      }
    }
    {
      "detail": "",
      "kind": 9,
      "label": "StringLabels",
      "sortText": "0001",
      "textEdit": {
        "newText": "StringLabels",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 0, "character": 6 }
        }
      }
    }
    |}]
;;

let%expect_test "can start completion at arbitrary position 2" =
  let source = {ocaml|StringLabels|ocaml} in
  let position = Position.create ~line:0 ~character:7 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "",
      "kind": 9,
      "label": "StringLabels",
      "sortText": "0000",
      "textEdit": {
        "newText": "StringLabels",
        "range": {
          "start": { "line": 0, "character": 0 },
          "end": { "line": 0, "character": 7 }
        }
      }
    }
    |}]
;;

let%expect_test "can start completion after operator without space" =
  let source = {ocaml|[1;2]|>List.ma|ocaml} in
  let position = Position.create ~line:0 ~character:14 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "('a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "map",
      "sortText": "0000",
      "textEdit": {
        "newText": "map",
        "range": {
          "start": { "line": 0, "character": 12 },
          "end": { "line": 0, "character": 14 }
        }
      }
    }
    {
      "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
      "kind": 12,
      "label": "map2",
      "sortText": "0001",
      "textEdit": {
        "newText": "map2",
        "range": {
          "start": { "line": 0, "character": 12 },
          "end": { "line": 0, "character": 14 }
        }
      }
    }
    {
      "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "mapi",
      "sortText": "0002",
      "textEdit": {
        "newText": "mapi",
        "range": {
          "start": { "line": 0, "character": 12 },
          "end": { "line": 0, "character": 14 }
        }
      }
    }
    |}]
;;

let%expect_test "can start completion after operator with space" =
  let source = {ocaml|[1;2] |> List.ma|ocaml} in
  let position = Position.create ~line:0 ~character:16 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "('a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "map",
      "sortText": "0000",
      "textEdit": {
        "newText": "map",
        "range": {
          "start": { "line": 0, "character": 14 },
          "end": { "line": 0, "character": 16 }
        }
      }
    }
    {
      "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
      "kind": 12,
      "label": "map2",
      "sortText": "0001",
      "textEdit": {
        "newText": "map2",
        "range": {
          "start": { "line": 0, "character": 14 },
          "end": { "line": 0, "character": 16 }
        }
      }
    }
    {
      "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "mapi",
      "sortText": "0002",
      "textEdit": {
        "newText": "mapi",
        "range": {
          "start": { "line": 0, "character": 14 },
          "end": { "line": 0, "character": 16 }
        }
      }
    }
    |}]
;;

let%expect_test "can start completion in dot chain with tab" =
  let source = {ocaml|[1;2] |> List.	ma|ocaml} in
  let position = Position.create ~line:0 ~character:17 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "('a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "map",
      "sortText": "0000",
      "textEdit": {
        "newText": "map",
        "range": {
          "start": { "line": 0, "character": 15 },
          "end": { "line": 0, "character": 17 }
        }
      }
    }
    {
      "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
      "kind": 12,
      "label": "map2",
      "sortText": "0001",
      "textEdit": {
        "newText": "map2",
        "range": {
          "start": { "line": 0, "character": 15 },
          "end": { "line": 0, "character": 17 }
        }
      }
    }
    {
      "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "mapi",
      "sortText": "0002",
      "textEdit": {
        "newText": "mapi",
        "range": {
          "start": { "line": 0, "character": 15 },
          "end": { "line": 0, "character": 17 }
        }
      }
    }
    |}]
;;

let%expect_test "can start completion in dot chain with newline" =
  let source =
    {ocaml|[1;2] |> List.
ma|ocaml}
  in
  let position = Position.create ~line:1 ~character:2 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "('a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "map",
      "sortText": "0000",
      "textEdit": {
        "newText": "map",
        "range": {
          "start": { "line": 1, "character": 0 },
          "end": { "line": 1, "character": 2 }
        }
      }
    }
    {
      "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
      "kind": 12,
      "label": "map2",
      "sortText": "0001",
      "textEdit": {
        "newText": "map2",
        "range": {
          "start": { "line": 1, "character": 0 },
          "end": { "line": 1, "character": 2 }
        }
      }
    }
    {
      "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "mapi",
      "sortText": "0002",
      "textEdit": {
        "newText": "mapi",
        "range": {
          "start": { "line": 1, "character": 0 },
          "end": { "line": 1, "character": 2 }
        }
      }
    }
    |}]
;;

let%expect_test "can start completion in dot chain with space" =
  let source = {ocaml|[1;2] |> List. ma|ocaml} in
  let position = Position.create ~line:0 ~character:17 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "('a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "map",
      "sortText": "0000",
      "textEdit": {
        "newText": "map",
        "range": {
          "start": { "line": 0, "character": 15 },
          "end": { "line": 0, "character": 17 }
        }
      }
    }
    {
      "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
      "kind": 12,
      "label": "map2",
      "sortText": "0001",
      "textEdit": {
        "newText": "map2",
        "range": {
          "start": { "line": 0, "character": 15 },
          "end": { "line": 0, "character": 17 }
        }
      }
    }
    {
      "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "mapi",
      "sortText": "0002",
      "textEdit": {
        "newText": "mapi",
        "range": {
          "start": { "line": 0, "character": 15 },
          "end": { "line": 0, "character": 17 }
        }
      }
    }
    |}]
;;

let%expect_test "can start completion after dereference" =
  let source =
    {ocaml|let apple=ref 10 in
!ap|ocaml}
  in
  let position = Position.create ~line:1 ~character:3 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "int ref",
      "kind": 12,
      "label": "apple",
      "sortText": "0000",
      "textEdit": {
        "newText": "apple",
        "range": {
          "start": { "line": 1, "character": 1 },
          "end": { "line": 1, "character": 3 }
        }
      }
    }
    |}]
;;

let%expect_test "can complete symbol passed as a named argument" =
  let source =
    {ocaml|let g ~f = f 0 in
g ~f:ig|ocaml}
  in
  let position = Position.create ~line:1 ~character:7 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "'a -> unit",
      "kind": 12,
      "label": "ignore",
      "sortText": "0000",
      "textEdit": {
        "newText": "ignore",
        "range": {
          "start": { "line": 1, "character": 5 },
          "end": { "line": 1, "character": 7 }
        }
      }
    }
    {
      "detail": "'a @ local once contended -> unit",
      "kind": 12,
      "label": "ignore_contended",
      "sortText": "0001",
      "textEdit": {
        "newText": "ignore_contended",
        "range": {
          "start": { "line": 1, "character": 5 },
          "end": { "line": 1, "character": 7 }
        }
      }
    }
    |}]
;;

let%expect_test "can complete symbol passed as a named argument - 2" =
  let source =
    {ocaml|module M = struct let igfoo _x = () end
let g ~f = f 0 in
g ~f:M.ig|ocaml}
  in
  let position = Position.create ~line:2 ~character:9 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "'a -> unit",
      "kind": 12,
      "label": "igfoo",
      "sortText": "0000",
      "textEdit": {
        "newText": "igfoo",
        "range": {
          "start": { "line": 2, "character": 7 },
          "end": { "line": 2, "character": 9 }
        }
      }
    }
    |}]
;;

let%expect_test "can complete symbol passed as an optional argument" =
  let source =
    {ocaml|
let g ?f = f in
g ?f:ig
    |ocaml}
  in
  let position = Position.create ~line:2 ~character:7 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "'a -> unit",
      "kind": 12,
      "label": "ignore",
      "sortText": "0000",
      "textEdit": {
        "newText": "ignore",
        "range": {
          "start": { "line": 2, "character": 5 },
          "end": { "line": 2, "character": 7 }
        }
      }
    }
    {
      "detail": "'a @ local once contended -> unit",
      "kind": 12,
      "label": "ignore_contended",
      "sortText": "0001",
      "textEdit": {
        "newText": "ignore_contended",
        "range": {
          "start": { "line": 2, "character": 5 },
          "end": { "line": 2, "character": 7 }
        }
      }
    }
    |}]
;;

let%expect_test "can complete symbol passed as an optional argument - 2" =
  let source =
    {ocaml|module M = struct let igfoo _x = () end
let g ?f = f in
g ?f:M.ig|ocaml}
  in
  let position = Position.create ~line:2 ~character:9 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "'a -> unit",
      "kind": 12,
      "label": "igfoo",
      "sortText": "0000",
      "textEdit": {
        "newText": "igfoo",
        "range": {
          "start": { "line": 2, "character": 7 },
          "end": { "line": 2, "character": 9 }
        }
      }
    }
    |}]
;;

let%expect_test "completes identifier after completion-triggering character" =
  let source =
    {ocaml|
module Test = struct
  let somenum = 42
  let somestring = "hello"
end

let x = Test.
    |ocaml}
  in
  let position = Position.create ~line:6 ~character:13 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "int",
      "kind": 12,
      "label": "somenum",
      "sortText": "0000",
      "textEdit": {
        "newText": "somenum",
        "range": {
          "start": { "line": 6, "character": 13 },
          "end": { "line": 6, "character": 13 }
        }
      }
    }
    {
      "detail": "string",
      "kind": 12,
      "label": "somestring",
      "sortText": "0001",
      "textEdit": {
        "newText": "somestring",
        "range": {
          "start": { "line": 6, "character": 13 },
          "end": { "line": 6, "character": 13 }
        }
      }
    }
    |}]
;;

let%expect_test "completes infix operators" =
  let source =
    {ocaml|
let (>>|) = (+)
let y = 1 >
|ocaml}
  in
  let position = Position.create ~line:2 ~character:11 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "int -> int -> int",
      "kind": 12,
      "label": ">>|",
      "sortText": "0000",
      "textEdit": {
        "newText": ">>|",
        "range": {
          "start": { "line": 2, "character": 10 },
          "end": { "line": 2, "character": 11 }
        }
      }
    }
    {
      "detail": "'a -> 'a -> bool",
      "kind": 12,
      "label": ">",
      "sortText": "0001",
      "textEdit": {
        "newText": ">",
        "range": {
          "start": { "line": 2, "character": 10 },
          "end": { "line": 2, "character": 11 }
        }
      }
    }
    {
      "detail": "'a -> 'a -> bool",
      "kind": 12,
      "label": ">=",
      "sortText": "0002",
      "textEdit": {
        "newText": ">=",
        "range": {
          "start": { "line": 2, "character": 10 },
          "end": { "line": 2, "character": 11 }
        }
      }
    }
    |}]
;;

let%expect_test "completes without prefix" =
  let source =
    {ocaml|
let somenum = 42
let somestring = "hello"

let plus_42 (x:int) (y:int) =
  somenum +
|ocaml}
  in
  let position = Position.create ~line:5 ~character:12 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "int -> int -> int",
      "kind": 12,
      "label": "+",
      "sortText": "0000",
      "textEdit": {
        "newText": "+",
        "range": {
          "start": { "line": 5, "character": 11 },
          "end": { "line": 5, "character": 12 }
        }
      }
    }
    {
      "detail": "float -> float -> float",
      "kind": 12,
      "label": "+.",
      "sortText": "0001",
      "textEdit": {
        "newText": "+.",
        "range": {
          "start": { "line": 5, "character": 11 },
          "end": { "line": 5, "character": 12 }
        }
      }
    }
    |}]
;;

let%expect_test "completes labels" =
  let source = {ocaml|let f = ListLabels.map ~|ocaml} in
  let position = Position.create ~line:0 ~character:24 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "int -> int",
      "kind": 12,
      "label": "~+",
      "sortText": "0000",
      "textEdit": {
        "newText": "~+",
        "range": {
          "start": { "line": 0, "character": 23 },
          "end": { "line": 0, "character": 24 }
        }
      }
    }
    {
      "detail": "float -> float",
      "kind": 12,
      "label": "~+.",
      "sortText": "0001",
      "textEdit": {
        "newText": "~+.",
        "range": {
          "start": { "line": 0, "character": 23 },
          "end": { "line": 0, "character": 24 }
        }
      }
    }
    {
      "detail": "int -> int",
      "kind": 12,
      "label": "~-",
      "sortText": "0002",
      "textEdit": {
        "newText": "~-",
        "range": {
          "start": { "line": 0, "character": 23 },
          "end": { "line": 0, "character": 24 }
        }
      }
    }
    {
      "detail": "float -> float",
      "kind": 12,
      "label": "~-.",
      "sortText": "0003",
      "textEdit": {
        "newText": "~-.",
        "range": {
          "start": { "line": 0, "character": 23 },
          "end": { "line": 0, "character": 24 }
        }
      }
    }
    {
      "detail": "'a -> 'b",
      "kind": 5,
      "label": "~f",
      "sortText": "0004",
      "textEdit": {
        "newText": "~f",
        "range": {
          "start": { "line": 0, "character": 23 },
          "end": { "line": 0, "character": 24 }
        }
      }
    }
    |}]
;;

let%expect_test "works for polymorphic variants - function application context - 1" =
  let source =
    {ocaml|
let f (_a: [`String | `Int of int]) = ()

let u = f `Str
  |ocaml}
  in
  let position = Position.create ~line:3 ~character:14 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "`String",
      "kind": 20,
      "label": "`String",
      "sortText": "0000",
      "textEdit": {
        "newText": "`String",
        "range": {
          "start": { "line": 3, "character": 10 },
          "end": { "line": 3, "character": 14 }
        }
      }
    }
    |}]
;;

let%expect_test "works for polymorphic variants - function application context - 2" =
  let source =
    {ocaml|
let f (_a: [`String | `Int of int]) = ()

let u = f `In
  |ocaml}
  in
  let position = Position.create ~line:3 ~character:13 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "`Int of int",
      "kind": 20,
      "label": "`Int",
      "sortText": "0000",
      "textEdit": {
        "newText": "`Int",
        "range": {
          "start": { "line": 3, "character": 10 },
          "end": { "line": 3, "character": 13 }
        }
      }
    }
    |}]
;;

let%expect_test "works for polymorphic variants" =
  let source =
    {ocaml|
type t = [ `Int | `String ]

let x : t = `I
  |ocaml}
  in
  let position = Position.create ~line:3 ~character:15 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "`Int",
      "kind": 20,
      "label": "`Int",
      "sortText": "0000",
      "textEdit": {
        "newText": "`Int",
        "range": {
          "start": { "line": 3, "character": 13 },
          "end": { "line": 3, "character": 15 }
        }
      }
    }
    |}]
;;

let%expect_test "completion for holes" =
  let source = {ocaml|let u : int = _|ocaml} in
  let position = Position.create ~line:0 ~character:15 in
  let filter =
    List.filter ~f:(fun (item : CompletionItem.t) ->
      not (String.is_prefix item.label ~prefix:"__"))
  in
  let%map () = print_completions ~pre_print:filter source position in
  [%expect
    {|
    Completions:
    {
      "filterText": "_0",
      "kind": 1,
      "label": "0",
      "sortText": "0000",
      "textEdit": {
        "newText": "0",
        "range": {
          "start": { "line": 0, "character": 14 },
          "end": { "line": 0, "character": 15 }
        }
      }
    }
    |}]
;;

let%expect_test "completes identifier at top level" =
  let source =
    {ocaml|
let somenum = 42
let somestring = "hello"

let () =
  some
|ocaml}
  in
  let position = Position.create ~line:5 ~character:6 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "int",
      "kind": 12,
      "label": "somenum",
      "sortText": "0000",
      "textEdit": {
        "newText": "somenum",
        "range": {
          "start": { "line": 5, "character": 2 },
          "end": { "line": 5, "character": 6 }
        }
      }
    }
    {
      "detail": "string",
      "kind": 12,
      "label": "somestring",
      "sortText": "0001",
      "textEdit": {
        "newText": "somestring",
        "range": {
          "start": { "line": 5, "character": 2 },
          "end": { "line": 5, "character": 6 }
        }
      }
    }
    |}]
;;

let%expect_test "completes from a module" =
  let source = {ocaml|let f = List.m|ocaml} in
  let position = Position.create ~line:0 ~character:14 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "('a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "map",
      "sortText": "0000",
      "textEdit": {
        "newText": "map",
        "range": {
          "start": { "line": 0, "character": 13 },
          "end": { "line": 0, "character": 14 }
        }
      }
    }
    {
      "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
      "kind": 12,
      "label": "map2",
      "sortText": "0001",
      "textEdit": {
        "newText": "map2",
        "range": {
          "start": { "line": 0, "character": 13 },
          "end": { "line": 0, "character": 14 }
        }
      }
    }
    {
      "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
      "kind": 12,
      "label": "mapi",
      "sortText": "0002",
      "textEdit": {
        "newText": "mapi",
        "range": {
          "start": { "line": 0, "character": 13 },
          "end": { "line": 0, "character": 14 }
        }
      }
    }
    {
      "detail": "'a @ local -> 'a list @ local -> bool",
      "kind": 12,
      "label": "mem",
      "sortText": "0003",
      "textEdit": {
        "newText": "mem",
        "range": {
          "start": { "line": 0, "character": 13 },
          "end": { "line": 0, "character": 14 }
        }
      }
    }
    {
      "detail": "'a -> ('a * 'b) list -> bool",
      "kind": 12,
      "label": "mem_assoc",
      "sortText": "0004",
      "textEdit": {
        "newText": "mem_assoc",
        "range": {
          "start": { "line": 0, "character": 13 },
          "end": { "line": 0, "character": 14 }
        }
      }
    }
    {
      "detail": "'a -> ('a * 'b) list -> bool",
      "kind": 12,
      "label": "mem_assq",
      "sortText": "0005",
      "textEdit": {
        "newText": "mem_assq",
        "range": {
          "start": { "line": 0, "character": 13 },
          "end": { "line": 0, "character": 14 }
        }
      }
    }
    {
      "detail": "'a @ local -> 'a list @ local -> bool",
      "kind": 12,
      "label": "memq",
      "sortText": "0006",
      "textEdit": {
        "newText": "memq",
        "range": {
          "start": { "line": 0, "character": 13 },
          "end": { "line": 0, "character": 14 }
        }
      }
    }
    {
      "detail": "('a -> 'a -> int) -> 'a list -> 'a list -> 'a list",
      "kind": 12,
      "label": "merge",
      "sortText": "0007",
      "textEdit": {
        "newText": "merge",
        "range": {
          "start": { "line": 0, "character": 13 },
          "end": { "line": 0, "character": 14 }
        }
      }
    }
    |}]
;;

let%expect_test "completes a module name" =
  let source = {ocaml|let f = L|ocaml} in
  let position = Position.create ~line:0 ~character:9 in
  let%map () =
    print_completions ~pre_print:(fun l -> Core.List.take l 5) source position
  in
  [%expect
    {|
    Completions:
    {
      "detail": "",
      "kind": 9,
      "label": "LargeFile",
      "sortText": "0000",
      "textEdit": {
        "newText": "LargeFile",
        "range": {
          "start": { "line": 0, "character": 8 },
          "end": { "line": 0, "character": 9 }
        }
      }
    }
    {
      "detail": "",
      "kind": 9,
      "label": "Lazy",
      "sortText": "0001",
      "textEdit": {
        "newText": "Lazy",
        "range": {
          "start": { "line": 0, "character": 8 },
          "end": { "line": 0, "character": 9 }
        }
      }
    }
    {
      "detail": "",
      "kind": 9,
      "label": "Lexing",
      "sortText": "0002",
      "textEdit": {
        "newText": "Lexing",
        "range": {
          "start": { "line": 0, "character": 8 },
          "end": { "line": 0, "character": 9 }
        }
      }
    }
    {
      "detail": "",
      "kind": 9,
      "label": "List",
      "sortText": "0003",
      "textEdit": {
        "newText": "List",
        "range": {
          "start": { "line": 0, "character": 8 },
          "end": { "line": 0, "character": 9 }
        }
      }
    }
    {
      "detail": "",
      "kind": 9,
      "label": "ListLabels",
      "sortText": "0004",
      "textEdit": {
        "newText": "ListLabels",
        "range": {
          "start": { "line": 0, "character": 8 },
          "end": { "line": 0, "character": 9 }
        }
      }
    }
    |}]
;;

let%expect_test "completion doesn't autocomplete record fields" =
  let source =
    {ocaml|
    type r = {
      x: int;
      y: string
    }

    let _ =
  |ocaml}
  in
  let position = Position.create ~line:5 ~character:8 in
  let%map () =
    print_completions
      ~pre_print:
        (List.filter ~f:(fun (compl : CompletionItem.t) ->
           compl.label = "x" || compl.label = "y"))
      source
      position
  in
  (* We expect 0 completions *)
  [%expect {| No completions |}]
;;
