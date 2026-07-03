let%expect_test "deduplicate_remote_by_directory" =
  let open Core in
  let module Lsp_types = Lsp.Types in
  let loc uri_path ~line ~character =
    let uri = Lsp_types.DocumentUri.of_path uri_path in
    let pos = { Lsp_types.Position.line; character } in
    { Lsp_types.Location.uri; range = { start = pos; end_ = pos } }
  in
  let test ~local ~remote =
    let result =
      Ocaml_lsp_server.References_req.For_testing.deduplicate_remote_by_directory
        ~local
        ~remote
    in
    List.iter result ~f:(fun (loc : Lsp_types.Location.t) ->
      let path = Lsp_types.DocumentUri.to_path loc.uri in
      printf "%s:%d\n" path loc.range.start.line)
  in
  (* Remote refs from same directory as local refs are dropped *)
  test
    ~local:[ loc "/home/user/src/a.ml" ~line:1 ~character:0 ]
    ~remote:
      [ loc "/home/user/src/b.ml" ~line:5 ~character:0
      ; loc "/home/user/other/c.ml" ~line:10 ~character:0
      ];
  [%expect {| /home/user/other/c.ml:10 |}];
  (* Remote ref in same file is also dropped (same directory) *)
  test
    ~local:[ loc "/home/user/src/a.ml" ~line:1 ~character:0 ]
    ~remote:[ loc "/home/user/src/a.ml" ~line:5 ~character:0 ];
  [%expect {| |}];
  (* No local refs: all remote refs kept *)
  test
    ~local:[]
    ~remote:
      [ loc "/home/user/src/a.ml" ~line:1 ~character:0
      ; loc "/home/user/lib/b.ml" ~line:2 ~character:0
      ];
  [%expect
    {|
    /home/user/src/a.ml:1
    /home/user/lib/b.ml:2
    |}];
  (* No remote refs: nothing returned *)
  test ~local:[ loc "/home/user/src/a.ml" ~line:1 ~character:0 ] ~remote:[];
  [%expect {| |}];
  (* Multiple local directories filter out corresponding remote directories *)
  test
    ~local:
      [ loc "/home/user/src/a.ml" ~line:1 ~character:0
      ; loc "/home/user/lib/b.ml" ~line:2 ~character:0
      ]
    ~remote:
      [ loc "/home/user/src/x.ml" ~line:5 ~character:0
      ; loc "/home/user/lib/y.ml" ~line:6 ~character:0
      ; loc "/home/user/test/z.ml" ~line:7 ~character:0
      ];
  [%expect {| /home/user/test/z.ml:7 |}]
;;
