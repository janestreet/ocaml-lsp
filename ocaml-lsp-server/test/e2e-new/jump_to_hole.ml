open Async
open Test.Import

let print_jump_to_hole (source : string) (position : Position.t) (direction : string) =
  let makeRequest (textDocument : TextDocumentIdentifier.t) =
    let params =
      Some
        (`Assoc
          [ "uri", Uri.yojson_of_t textDocument.uri
          ; "position", Position.yojson_of_t position
          ; "direction", `String direction
          ])
    in
    Lsp.Client_request.UnknownRequest
      { meth = Lsp.Client_request.Custom_request_names.typed_holes_jump; params }
  in
  Lsp_helpers.iter_lsp_response ~makeRequest ~source (fun x ->
    print_endline (Yojson.Safe.to_string x))
;;

let%expect_test "can jump to next hole" =
  let source =
    {ocaml|
let rec zip_shortest (xs : 'a list) (ys : 'b list) : ('a * 'b) list =
  match (xs, ys) with
  | ([], []) | ([], _::_) | (_::_, []) -> _
  | (x::xs, y::ys) -> ((_, _) :: (zip_shortest _ _))
;;
|ocaml}
  in
  let position = Position.create ~line:4 ~character:40 in
  let%map () = print_jump_to_hole source position "next" in
  [%expect {| {"start":{"line":4,"character":47},"end":{"line":4,"character":48}} |}]
;;

let%expect_test "can jump to prev hole" =
  let source =
    {ocaml|
let rec zip_shortest (xs : 'a list) (ys : 'b list) : ('a * 'b) list =
  match (xs, ys) with
  | ([], []) | ([], _::_) | (_::_, []) -> _
  | (x::xs, y::ys) -> ((_, _) :: (zip_shortest _ _))
;;
|ocaml}
  in
  let position = Position.create ~line:4 ~character:40 in
  let%map () = print_jump_to_hole source position "prev" in
  [%expect {| {"start":{"line":4,"character":27},"end":{"line":4,"character":28}} |}]
;;

let%expect_test "jump to next hole can wrap" =
  let source =
    {ocaml|
let rec zip_shortest (xs : 'a list) (ys : 'b list) : ('a * 'b) list =
  match (xs, ys) with
  | ([], []) | ([], _::_) | (_::_, []) -> _
  | (x::xs, y::ys) -> ((_, _) :: (zip_shortest _ _))
;;
|ocaml}
  in
  let position = Position.create ~line:5 ~character:1 in
  let%map () = print_jump_to_hole source position "next" in
  [%expect {| {"start":{"line":3,"character":42},"end":{"line":3,"character":43}} |}]
;;

let%expect_test "jump to prev hole can wrap" =
  let source =
    {ocaml|
let rec zip_shortest (xs : 'a list) (ys : 'b list) : ('a * 'b) list =
  match (xs, ys) with
  | ([], []) | ([], _::_) | (_::_, []) -> _
  | (x::xs, y::ys) -> ((_, _) :: (zip_shortest _ _))
;;
|ocaml}
  in
  let position = Position.create ~line:2 ~character:20 in
  let%map () = print_jump_to_hole source position "prev" in
  [%expect {| {"start":{"line":4,"character":49},"end":{"line":4,"character":50}} |}]
;;

let%expect_test "can jump to next hole when on char before current hole" =
  let source =
    {ocaml|
let rec zip_shortest (xs : 'a list) (ys : 'b list) : ('a * 'b) list =
  match (xs, ys) with
  | ([], []) | ([], _::_) | (_::_, []) -> _
  | (x::xs, y::ys) -> ((_, _) :: (zip_shortest _ _))
;;
|ocaml}
  in
  let position = Position.create ~line:4 ~character:27 in
  let%map () = print_jump_to_hole source position "next" in
  [%expect {| {"start":{"line":4,"character":47},"end":{"line":4,"character":48}} |}]
;;

let%expect_test "can jump to next hole when on char after current hole" =
  let source =
    {ocaml|
let rec zip_shortest (xs : 'a list) (ys : 'b list) : ('a * 'b) list =
  match (xs, ys) with
  | ([], []) | ([], _::_) | (_::_, []) -> _
  | (x::xs, y::ys) -> ((_, _) :: (zip_shortest _ _))
;;
|ocaml}
  in
  let position = Position.create ~line:4 ~character:28 in
  let%map () = print_jump_to_hole source position "next" in
  [%expect {| {"start":{"line":4,"character":47},"end":{"line":4,"character":48}} |}]
;;

let%expect_test "can jump to prev hole when on char before current hole" =
  let source =
    {ocaml|
let rec zip_shortest (xs : 'a list) (ys : 'b list) : ('a * 'b) list =
  match (xs, ys) with
  | ([], []) | ([], _::_) | (_::_, []) -> _
  | (x::xs, y::ys) -> ((_, _) :: (zip_shortest _ _))
;;
|ocaml}
  in
  let position = Position.create ~line:4 ~character:27 in
  let%map () = print_jump_to_hole source position "prev" in
  [%expect {| {"start":{"line":4,"character":24},"end":{"line":4,"character":25}} |}]
;;

let%expect_test "can jump to prev hole when on char after current hole" =
  let source =
    {ocaml|
let rec zip_shortest (xs : 'a list) (ys : 'b list) : ('a * 'b) list =
  match (xs, ys) with
  | ([], []) | ([], _::_) | (_::_, []) -> _
  | (x::xs, y::ys) -> ((_, _) :: (zip_shortest _ _))
;;
|ocaml}
  in
  let position = Position.create ~line:4 ~character:28 in
  let%map () = print_jump_to_hole source position "prev" in
  [%expect {| {"start":{"line":4,"character":24},"end":{"line":4,"character":25}} |}]
;;
