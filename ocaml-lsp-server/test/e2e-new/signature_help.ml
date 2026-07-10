module Fiber = Ocaml_lsp_fiber
open Test.Import
open Async

let print_signature_help (help : SignatureHelp.t option) =
  match help with
  | Some help ->
    SignatureHelp.yojson_of_t help
    |> Yojson.Safe.pretty_to_string ~std:false
    |> print_endline
  | None -> print_endline "no signature help"
;;

let signature_help client position =
  Client.request
    client
    (SignatureHelp
       (SignatureHelpParams.create
          ~position
          ~textDocument:(TextDocumentIdentifier.create ~uri:Helpers.uri)
          ()))
;;

let%expect_test "signature help for a simple function" =
  let source =
    {ocaml|
let add x y = x + y

let _ = add 1
|ocaml}
  in
  let req client =
    let* resp = signature_help client (Position.create ~line:3 ~character:14) in
    print_signature_help resp;
    Fiber.return ()
  in
  let%map () = Helpers.test source req in
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "add : int -> int -> int",
          "parameters": [ { "label": [ 6, 9 ] }, { "label": [ 13, 16 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "signature help for a function with labeled arguments" =
  let source =
    {ocaml|
let greet ~name ~greeting = greeting ^ ", " ^ name

let _ = greet ~name:"Alice"
|ocaml}
  in
  let req client =
    let* resp = signature_help client (Position.create ~line:3 ~character:27) in
    print_signature_help resp;
    Fiber.return ()
  in
  let%map () = Helpers.test source req in
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "greet : name:string -> greeting:string -> string",
          "parameters": [ { "label": [ 8, 19 ] }, { "label": [ 23, 38 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "signature help for a function with optional arguments" =
  let source =
    {ocaml|
let make_message ?prefix ~content () =
  match prefix with
  | Some p -> p ^ ": " ^ content
  | None -> content

let _ = make_message ~content:"hello"
|ocaml}
  in
  let req client =
    let* resp = signature_help client (Position.create ~line:6 ~character:38) in
    print_signature_help resp;
    Fiber.return ()
  in
  let%map () = Helpers.test source req in
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "make_message : ?prefix:string -> content:string -> unit -> string",
          "parameters": [
            { "label": [ 15, 29 ] },
            { "label": [ 33, 47 ] },
            { "label": [ 51, 55 ] }
          ]
        }
      ]
    }
    |}]
;;

let%expect_test "signature help at the start of function application" =
  let source =
    {ocaml|
let multiply x y z = x * y * z

let _ = multiply
|ocaml}
  in
  let req client =
    let* resp = signature_help client (Position.create ~line:3 ~character:16) in
    print_signature_help resp;
    Fiber.return ()
  in
  let%map () = Helpers.test source req in
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "multiply : int -> int -> int -> int",
          "parameters": [
            { "label": [ 11, 14 ] },
            { "label": [ 18, 21 ] },
            { "label": [ 25, 28 ] }
          ]
        }
      ]
    }
    |}]
;;

let%expect_test "signature help with higher-order function" =
  let source =
    {ocaml|
let apply f x = f x

let _ = apply (fun x -> x + 1)
|ocaml}
  in
  let req client =
    let* resp = signature_help client (Position.create ~line:3 ~character:31) in
    print_signature_help resp;
    Fiber.return ()
  in
  let%map () = Helpers.test source req in
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "apply : (int -> int) -> int -> int",
          "parameters": [ { "label": [ 8, 20 ] }, { "label": [ 24, 27 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "no signature help outside function application" =
  let source =
    {ocaml|
let x = 42
|ocaml}
  in
  let req client =
    let* resp = signature_help client (Position.create ~line:1 ~character:10) in
    print_signature_help resp;
    Fiber.return ()
  in
  let%map () = Helpers.test source req in
  [%expect {| { "signatures": [] } |}]
;;

let%expect_test "signature help for stdlib function" =
  let source =
    {ocaml|
let _ = List.map (fun x -> x) []
|ocaml}
  in
  let req client =
    (* Position cursor after the first argument to see signature help *)
    let* resp = signature_help client (Position.create ~line:1 ~character:30) in
    print_signature_help resp;
    Fiber.return ()
  in
  let%map () = Helpers.test source req in
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "documentation": {
            "kind": "plaintext",
            "value": "[map f [a1; ...; an]] applies function [f] to [a1, ..., an],\n   and builds the list [[f a1; ...; f an]]\n   with the results returned by [f]."
          },
          "label": "List.map : ('a -> 'a) -> 'a list -> 'a list",
          "parameters": [ { "label": [ 11, 21 ] }, { "label": [ 25, 32 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "signature help with multiple arguments provided" =
  let source =
    {ocaml|
let f a b c d = a + b + c + d

let _ = f 1 2 3
|ocaml}
  in
  let req client =
    (* Cursor after the third argument - should show fourth parameter as active *)
    let* resp = signature_help client (Position.create ~line:3 ~character:15) in
    print_signature_help resp;
    Fiber.return ()
  in
  let%map () = Helpers.test source req in
  [%expect
    {|
    {
      "activeParameter": 2,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "f : int -> int -> int -> int -> int",
          "parameters": [
            { "label": [ 4, 7 ] },
            { "label": [ 11, 14 ] },
            { "label": [ 18, 21 ] },
            { "label": [ 25, 28 ] }
          ]
        }
      ]
    }
    |}]
;;

let%expect_test "signature help in nested function call" =
  let source =
    {ocaml|
let outer x = x + 1
let inner y = y * 2

let _ = outer (inner 5)
|ocaml}
  in
  let req client =
    (* Cursor inside the inner function call *)
    let* resp = signature_help client (Position.create ~line:4 ~character:21) in
    print_signature_help resp;
    Fiber.return ()
  in
  let%map () = Helpers.test source req in
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "inner : int -> int",
          "parameters": [ { "label": [ 8, 11 ] } ]
        }
      ]
    }
    |}]
;;
