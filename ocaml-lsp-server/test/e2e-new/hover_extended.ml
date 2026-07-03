module Fiber = Ocaml_lsp_fiber
open Async
open Test.Import

let print_hover hover =
  match hover with
  | None -> print_endline "no hover response"
  | Some hover ->
    hover |> Hover.yojson_of_t |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let hover client position =
  Client.request
    client
    (TextDocumentHover
       { HoverParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri
       ; workDoneToken = None
       })
;;

let print_hover_extended resp =
  resp |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let hover_extended client position verbosity =
  let params =
    let required =
      [ ( "textDocument"
        , TextDocumentIdentifier.yojson_of_t
            (TextDocumentIdentifier.create ~uri:Helpers.uri) )
      ; "position", Position.yojson_of_t position
      ]
    in
    let params =
      match verbosity with
      | None -> required
      | Some v -> ("verbosity", `Int v) :: required
    in
    Some (Jsonrpc.Structured.t_of_yojson (`Assoc params))
  in
  Client.request
    client
    (UnknownRequest
       { meth = Lsp.Client_request.Custom_request_names.hover_extended; params })
;;

let%expect_test "hover reference" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  (* This test shows the default hover behavior, and we enable extendedHover by default.
     Testing the disabling of extendedHover requires threading through extra logic and
     isn't worth the effort (setting OCAMLLSP_HOVER_IS_EXTENDED=false just uses the
     default value, which is [true]). *)
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
        "value": "int option\n***\nKind: immutable_data\n***\nMode: @ portable stateless unique static"
      },
      "range": {
        "start": { "line": 3, "character": 4 },
        "end": { "line": 3, "character": 13 }
      }
    }
    |}]
;;

let%expect_test "hover returns type inferred under cursor in a formatted way" =
  let source =
    {ocaml|
let f a b c d e f g h i = 1 + a + b + c + d + e + f + g + h + i
|ocaml}
  in
  let position = Position.create ~line:1 ~character:4 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "contents": {
        "kind": "plaintext",
        "value": "int -> int -> int -> int -> int -> int -> int -> int -> int -> int\n***\nAllocation: heap"
      },
      "range": {
        "start": { "line": 1, "character": 4 },
        "end": { "line": 1, "character": 5 }
      }
    }
    |}]
;;

let%expect_test "hover extended" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  let%map.Deferred () =
    Helpers.test ~extra_env:[ "OCAMLLSP_HOVER_IS_EXTENDED=true" ] source req
  in
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
        "value": "int option\n***\nKind: immutable_data\n***\nMode: @ portable stateless unique static"
      },
      "range": {
        "start": { "line": 3, "character": 4 },
        "end": { "line": 3, "character": 13 }
      }
    }
    |}]
;;

let%expect_test "default verbosity" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "start": { "line": 3, "character": 4 },
        "end": { "line": 3, "character": 13 }
      }
    }
    |}]
;;

let%expect_test "explicit verbosity 0" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position (Some 0) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "start": { "line": 3, "character": 4 },
        "end": { "line": 3, "character": 13 }
      }
    }
    |}]
;;

let%expect_test "explicit verbosity 1" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position (Some 1) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 1,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": true,
      "contents": {
        "kind": "plaintext",
        "value": "int option\n***\nKind: immutable_data\n***\nMode: @ portable stateless unique static"
      },
      "range": {
        "start": { "line": 3, "character": 4 },
        "end": { "line": 3, "character": 13 }
      }
    }
    |}]
;;

let%expect_test "explicit verbosity 2" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position (Some 2) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 2,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": true,
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

let%expect_test "implicity verbosity increases" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "start": { "line": 3, "character": 4 },
        "end": { "line": 3, "character": 13 }
      }
    }
    {
      "verbosity": 1,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": true,
      "contents": {
        "kind": "plaintext",
        "value": "int option\n***\nKind: immutable_data\n***\nMode: @ portable stateless unique static"
      },
      "range": {
        "start": { "line": 3, "character": 4 },
        "end": { "line": 3, "character": 13 }
      }
    }
    {
      "verbosity": 2,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": true,
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

let%expect_test "hover extended returns type inferred under cursor in a formatted way" =
  let source =
    {ocaml|
let f a b c d e f g h i = 1 + a + b + c + d + e + f + g + h + i
|ocaml}
  in
  let position = Position.create ~line:1 ~character:4 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "int -> int -> int -> int -> int -> int -> int -> int -> int -> int\n***\nAllocation: heap"
      },
      "range": {
        "start": { "line": 1, "character": 4 },
        "end": { "line": 1, "character": 5 }
      }
    }
    |}]
;;

let%expect_test "heap allocation" =
  let source =
    {ocaml|
let f g x y =
  let z = x + y in
  Some (g z)
;;
|ocaml}
  in
  let position = Position.create ~line:3 ~character:3 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "'a -> 'a option\n***\nSome\n***\nAllocation: heap"
      },
      "range": {
        "start": { "line": 3, "character": 2 },
        "end": { "line": 3, "character": 6 }
      }
    }
    |}]
;;

let%expect_test "stack allocation" =
  let source =
    {ocaml|
let f g x y =
  let z = x + y in
  exclave_ Some (g z)
;;
|ocaml}
  in
  let position = Position.create ~line:3 ~character:12 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "'a -> 'a option\n***\nSome\n***\nAllocation: stack"
      },
      "range": {
        "start": { "line": 3, "character": 11 },
        "end": { "line": 3, "character": 15 }
      }
    }
    |}]
;;

let%expect_test "no relevant allocation to show" =
  let source =
    {ocaml|
let f g x y =
  let z = x + y in
  Some (g z)
;;
|ocaml}
  in
  let position = Position.create ~line:2 ~character:13 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "int -> int -> int\n***\nInteger addition.\n    Left-associative operator, see {!Ocaml_operators} for more information.\n***\nAllocation: no relevant allocation to show"
      },
      "range": {
        "start": { "line": 2, "character": 12 },
        "end": { "line": 2, "character": 13 }
      }
    }
    |}]
;;

let%expect_test "not an allocation (constructor without arguments)" =
  let source =
    {ocaml|
let f g x y =
  let z = x + y in
  None
;;
|ocaml}
  in
  let position = Position.create ~line:3 ~character:2 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "'a option\n***\nNone\n***\nAllocation: not an allocation (constructor without arguments)"
      },
      "range": {
        "start": { "line": 3, "character": 2 },
        "end": { "line": 3, "character": 6 }
      }
    }
    |}]
;;

let%expect_test "could be stack or heap" =
  let source =
    {ocaml|
let f g x y =
  let z = Some (g z) in
  y
;;
|ocaml}
  in
  let position = Position.create ~line:2 ~character:10 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "'a -> 'a option\n***\nSome\n***\nAllocation: could be stack or heap"
      },
      "range": {
        "start": { "line": 2, "character": 10 },
        "end": { "line": 2, "character": 14 }
      }
    }
    |}]
;;

let%expect_test "function on the stack" =
  let source =
    {ocaml|
let f g =
  exclave_ fun x -> g x
;;
|ocaml}
  in
  let position = Position.create ~line:2 ~character:11 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "'a -> 'b\n***\nAllocation: stack"
      },
      "range": {
        "start": { "line": 2, "character": 11 },
        "end": { "line": 2, "character": 23 }
      }
    }
    |}]
;;

let%expect_test "can hover a ppx" =
  let source =
    {ocaml|
    let%expect "testception" =
      print_endline "boo!";
      [%expect {| boo! |}]
    ;;
    |ocaml}
  in
  let position = Position.create ~line:3 ~character:12 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {xxx|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": false,
      "contents": {
        "value": "(* ppx expect expansion *)\n[%expect {| boo! |}]",
        "language": "ocaml"
      },
      "range": {
        "start": { "line": 3, "character": 6 },
        "end": { "line": 3, "character": 26 }
      }
    }
    |xxx}]
;;

let hover_verbosity_test ~source ~position ~verbosity =
  let req client =
    let* resp = hover_extended client position (Some verbosity) in
    let open Yojson.Safe.Util in
    let hover_contents = resp |> member "contents" |> member "value" |> to_string in
    let can_increase_verbosity = resp |> member "canIncreaseVerbosity" |> to_bool in
    print_endline ("canIncreaseVerbosity: " ^ Bool.to_string can_increase_verbosity);
    print_newline ();
    print_endline hover_contents;
    Fiber.return ()
  in
  Helpers.test source req
;;

let%expect_test "kind verbosity" =
  let source =
    {ocaml|
type foo : value mod portable
|ocaml}
  in
  let position = Position.create ~line:1 ~character:5 in
  let%bind.Deferred () = hover_verbosity_test ~source ~position ~verbosity:0 in
  [%expect
    {|
    canIncreaseVerbosity: true

    type foo : value mod portable
    |}];
  let%bind.Deferred () = hover_verbosity_test ~source ~position ~verbosity:1 in
  [%expect
    {|
    canIncreaseVerbosity: true

    type foo : value mod portable
    ***
    Kind: value mod portable
    |}];
  let%bind.Deferred () = hover_verbosity_test ~source ~position ~verbosity:2 in
  [%expect
    {|
    canIncreaseVerbosity: false

    type foo : value mod portable
    ***
    Kind:
    value separable non_null
      mod portable
          local
          unforkable
          yielding
          once
          stateful
          unique
          read_write
          uncontended
          static
          internal
    |}];
  let%map.Deferred () = hover_verbosity_test ~source ~position ~verbosity:3 in
  [%expect
    {|
    canIncreaseVerbosity: false

    type foo : value mod portable
    ***
    Kind:
    value separable non_null
      mod portable
          local
          unforkable
          yielding
          once
          stateful
          unique
          read_write
          uncontended
          static
          internal
    |}]
;;

let%expect_test "kind abbreviation verbosity" =
  let source =
    {ocaml|
type foo : immediate
|ocaml}
  in
  let position = Position.create ~line:1 ~character:5 in
  let%bind.Deferred () = hover_verbosity_test ~source ~position ~verbosity:0 in
  [%expect
    {|
    canIncreaseVerbosity: true

    type foo : immediate
    |}];
  let%bind.Deferred () = hover_verbosity_test ~source ~position ~verbosity:1 in
  [%expect
    {|
    canIncreaseVerbosity: true

    type foo : immediate
    ***
    Kind: immediate
    |}];
  let%bind.Deferred () = hover_verbosity_test ~source ~position ~verbosity:2 in
  [%expect
    {|
    canIncreaseVerbosity: true

    type foo : immediate
    ***
    Kind: value non_pointer mod global many stateless immutable external_
    |}];
  let%map.Deferred () = hover_verbosity_test ~source ~position ~verbosity:3 in
  [%expect
    {|
    canIncreaseVerbosity: false

    type foo : immediate
    ***
    Kind:
    value non_pointer non_null
      mod global
          many
          stateless
          immutable
          forkable
          unyielding
          aliased
          portable
          contended
          external_
          static
    |}]
;;

let%expect_test "mode verbosity" =
  let source =
    {ocaml|
let counter = ref 0
let (f @ portable) () =
  let _ = counter in
  ()
|ocaml}
  in
  let position = Position.create ~line:3 ~character:13 in
  let%bind.Deferred () = hover_verbosity_test ~source ~position ~verbosity:0 in
  [%expect
    {|
    canIncreaseVerbosity: true

    int ref
    |}];
  let%bind.Deferred () = hover_verbosity_test ~source ~position ~verbosity:1 in
  [%expect
    {|
    canIncreaseVerbosity: true

    int ref

    type ('a : value_or_null) ref = { mutable contents : 'a; }
    ***
    Kind: mutable_data
    ***
    Mode: @ portable contended stateless
    |}];
  let%map.Deferred () = hover_verbosity_test ~source ~position ~verbosity:2 in
  [%expect
    {|
    canIncreaseVerbosity: true

    int ref

    type ('a : value_or_null) ref = { mutable contents : 'a; }
    ***
    Kind: value non_float mod forkable unyielding many stateless
    ***
    Mode: @ global portable contended read_write stateless aliased many forkable unyielding dynamic
    |}]
;;
