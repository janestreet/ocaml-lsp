open Async

let wrap_test = Code_actions.code_action_test ~title:"Wrap-type-in-module"

let%expect_test "preserving whitespace" =
  let%bind () = wrap_test "t$y$pe foo = bar" in
  [%expect
    {|
    module Foo = struct
      type t = bar
    end
    |}];
  let%bind () = wrap_test "t$y$pe foo= bar" in
  [%expect
    {|
    module Foo = struct
      type t= bar
    end
    |}];
  let%bind () = wrap_test "t$y$pe foo\n=\nbar" in
  [%expect
    {|
    module Foo = struct
      type t
      =
      bar
    end
    |}];
  let%bind () =
    wrap_test
      (
    String.concat
      "\n"
      [ "typ$e$ ('a, 'b) a = { bar : 'a";
        "                  ; baz : 'b";
        "                  }" ]
      [@ocamlformat "disable"])
  in
  [%expect
    {|
    module A = struct
      type ('a, 'b) t = { bar : 'a
                        ; baz : 'b
                        }
    end
    |}];
  (* non-space character can come before the type name *)
  let%bind () =
    wrap_test
    (String.concat
      "\n"
      [ "ty$p$e";
        "abc = { a: int;";
        "b: int; c: int } [@@deriving sexp]" ]) [@ocamlformat "disable"]
  in
  [%expect
    {|
    module Abc = struct
      type
      t = { a: int;
      b: int; c: int } [@@deriving sexp]
    end
    |}];
  (* entire module is indented by correct amount *)
  let%bind () =
    wrap_test
      (String.concat
         "\n"
         [ "module Outer = struct"
         ; "  module Inner = struct"
         ; "    type record ="
         ; "      { foo : int"
         ; "      ; bar : in$t$"
         ; "      }"
         ; "  end"
         ; "end"
         ])
  in
  [%expect
    {|
    module Outer = struct
      module Inner = struct
        module Record = struct
          type t =
            { foo : int
            ; bar : int
            }
        end
      end
    end
    |}];
  return ()
;;

let%expect_test "type parameters" =
  let%bind () = wrap_test "t$y$pe ('a, 'b, _) foo = bar" in
  [%expect
    {|
    module Foo = struct
      type ('a, 'b, _) t = bar
    end
    |}];
  return ()
;;

let%expect_test "definition chain" =
  let%bind () =
    wrap_test
      {xxx|module Module = struct
  module Foo = struct
    type ('a, 'b) t =
      { a : 'a
      ; b : 'b
      }
  end
end

ty$p$e ('a, 'b) foo = ('a, 'b) Module.Foo.t =
  { a : 'a
  ; b : 'b
  }
|xxx}
  in
  [%expect
    {|
    module Module = struct
      module Foo = struct
        type ('a, 'b) t =
          { a : 'a
          ; b : 'b
          }
      end
    end

    module Foo = struct
      type ('a, 'b) t = ('a, 'b) Module.Foo.t =
        { a : 'a
        ; b : 'b
        }
    end
    |}];
  return ()
;;

let%expect_test "can trigger action on any part of type declaration" =
  let%bind () = wrap_test {|type abc = { a: int; b: int; c: int } [@@derivin$g$ sexp]|} in
  [%expect
    {|
    module Abc = struct
      type t = { a: int; b: int; c: int } [@@deriving sexp]
    end
    |}];
  let%bind () = wrap_test {|type abc = { a: int; b: int;$ $c: int } [@@deriving sexp]|} in
  [%expect
    {|
    module Abc = struct
      type t = { a: int; b: int; c: int } [@@deriving sexp]
    end
    |}];
  let%bind () = wrap_test {|type a$b$c = { a: int; b: int; c: int } [@@deriving sexp]|} in
  [%expect
    {|
    module Abc = struct
      type t = { a: int; b: int; c: int } [@@deriving sexp]
    end
    |}];
  let%bind () = wrap_test {|typ$e$ abc = { a: int; b: int; c: int } [@@deriving sexp]|} in
  [%expect
    {|
    module Abc = struct
      type t = { a: int; b: int; c: int } [@@deriving sexp]
    end
    |}];
  return ()
;;

let%expect_test "type with name t is ignored" =
  let%map () = wrap_test {|type $t$ = int|} in
  [%expect {| |}]
;;

let%expect_test "produce sig in mli" =
  let%bind () =
    wrap_test
      ?path:(Some "needs-refactoring.mli")
      {|type abc = { a: int; b: int; c: int } [@@derivin$g$ sexp]|}
  in
  [%expect
    {|
    module Abc : sig
      type t = { a: int; b: int; c: int } [@@deriving sexp]
    end
    |}];
  return ()
;;
