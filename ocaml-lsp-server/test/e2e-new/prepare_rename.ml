(** This tests that on prepare rename requests, we only send back the range corresponding
    to text that will be changed. This distinguishes from the old (incorrect) behavior
    when TextDocumentPrepareRename used a `Buffer merlin-occurrences search. The old
    behavior would have returned the range of [Foo.Bar.baz] below, giving the false
    impression that the user could rename the modules at the same time. *)
open Test.Import

open Async

let prepare_rename ~position ~source =
  Lsp_helpers.iter_lsp_response
    ~makeRequest:(fun textDocument ->
      TextDocumentPrepareRename (PrepareRenameParams.create ~textDocument ~position ()))
    ~source
    (fun range ->
      match range with
      | None -> print_endline "No prepare rename response"
      | Some range ->
        Range.yojson_of_t range |> Yojson.Safe.pretty_to_string |> print_endline)
;;

let%expect_test "TextDocumentPrepareRename only returns region that will be changed" =
  let source =
    {ocaml|
module Foo = struct
  module Bar = struct
    type t = | A | B
    let baz = 1
  end
end
let x = Foo.Bar.baz
let y = Foo.Bar.A
module X = Foo.Bar
  |ocaml}
  in
  let%bind () =
    prepare_rename ~position:(Position.create ~line:7 ~character:17) ~source
  in
  (* Can rename a normal identifier *)
  [%expect
    {|
    {
      "start": { "line": 7, "character": 16 },
      "end": { "line": 7, "character": 19 }
    }
    |}];
  let%bind () =
    prepare_rename ~position:(Position.create ~line:7 ~character:13) ~source
  in
  (* Renaming a module that's part of an identifier (like [Bar] in [Foo.Bar.baz]) doesn't
     work, so we should return no response here *)
  [%expect {| No prepare rename response |}];
  let%bind () = prepare_rename ~position:(Position.create ~line:7 ~character:9) ~source in
  (* Renaming a module that's part of an identifier (like [Foo] in [Foo.Bar.baz]) doesn't
     work, so we should return no response here *)
  [%expect {| No prepare rename response |}];
  let%bind () =
    prepare_rename ~position:(Position.create ~line:8 ~character:17) ~source
  in
  (* Can rename a variant tag *)
  [%expect
    {|
    {
      "start": { "line": 8, "character": 16 },
      "end": { "line": 8, "character": 17 }
    }
    |}];
  let%bind () =
    prepare_rename ~position:(Position.create ~line:9 ~character:16) ~source
  in
  (* Can rename a module that's the actual value rather than part of a name *)
  [%expect
    {|
    {
      "start": { "line": 9, "character": 15 },
      "end": { "line": 9, "character": 18 }
    }
    |}];
  Deferred.unit
;;
