open! Core

[@@@alert "-private_lsp_library"]

module Jsonrpc = Lsp_json_rpc_types.Jsonrpc

let roundtrip_packet json_str =
  let json = Yojson.Safe.from_string json_str in
  let packet = Jsonrpc.Packet.t_of_yojson json in
  let json' = Jsonrpc.Packet.yojson_of_t packet in
  let json_str' = Yojson.Safe.to_string json' in
  printf "Input:  %s\n" json_str;
  printf "Output: %s\n" json_str';
  (* Verify semantic equality by parsing both and comparing *)
  let j1 = Yojson.Safe.from_string json_str in
  let j2 = Yojson.Safe.from_string json_str' in
  (* Check that all fields from input are present in output *)
  (match j1, j2 with
   | `Assoc fields1, `Assoc fields2 ->
     List.iter fields1 ~f:(fun (key, _) ->
       if not (List.Assoc.mem fields2 key ~equal:String.equal)
       then printf "MISSING field in output: %s\n" key)
   | _ -> ());
  printf "\n"
;;

let%expect_test "request roundtrip" =
  roundtrip_packet
    {|{"jsonrpc":"2.0","id":1,"method":"textDocument/documentHighlight","params":{"textDocument":{"uri":"file:///test.ml"},"position":{"line":0,"character":5}}}|};
  [%expect
    {|
    Input:  {"jsonrpc":"2.0","id":1,"method":"textDocument/documentHighlight","params":{"textDocument":{"uri":"file:///test.ml"},"position":{"line":0,"character":5}}}
    Output: {"id":1,"params":{"textDocument":{"uri":"file:///test.ml"},"position":{"line":0,"character":5}},"method":"textDocument/documentHighlight","jsonrpc":"2.0"}
    |}]
;;

let%expect_test "notification roundtrip" =
  roundtrip_packet
    {|{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///test.ml","languageId":"ocaml","version":1,"text":"let x = 1"}}}|};
  [%expect
    {|
    Input:  {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///test.ml","languageId":"ocaml","version":1,"text":"let x = 1"}}}
    Output: {"params":{"textDocument":{"uri":"file:///test.ml","languageId":"ocaml","version":1,"text":"let x = 1"}},"method":"textDocument/didOpen","jsonrpc":"2.0"}
    |}]
;;

let%expect_test "response roundtrip" =
  roundtrip_packet
    {|{"jsonrpc":"2.0","id":1,"result":{"capabilities":{"textDocumentSync":1}}}|};
  [%expect
    {|
    Input:  {"jsonrpc":"2.0","id":1,"result":{"capabilities":{"textDocumentSync":1}}}
    Output: {"id":1,"jsonrpc":"2.0","result":{"capabilities":{"textDocumentSync":1}}}
    |}]
;;

let%expect_test "error response roundtrip" =
  roundtrip_packet
    {|{"jsonrpc":"2.0","id":5,"error":{"code":-32603,"message":"uncaught exception","data":null}}|};
  [%expect
    {|
    Input:  {"jsonrpc":"2.0","id":5,"error":{"code":-32603,"message":"uncaught exception","data":null}}
    Output: {"id":5,"jsonrpc":"2.0","error":{"data":null,"code":-32603,"message":"uncaught exception"}}
    |}]
;;

let%expect_test "completion request roundtrip" =
  roundtrip_packet
    {|{"jsonrpc":"2.0","id":16,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///test.ml"},"position":{"line":5,"character":10},"context":{"triggerKind":1}}}|};
  [%expect
    {|
    Input:  {"jsonrpc":"2.0","id":16,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///test.ml"},"position":{"line":5,"character":10},"context":{"triggerKind":1}}}
    Output: {"id":16,"params":{"textDocument":{"uri":"file:///test.ml"},"position":{"line":5,"character":10},"context":{"triggerKind":1}},"method":"textDocument/completion","jsonrpc":"2.0"}
    |}]
;;

let%expect_test "cancel notification roundtrip" =
  roundtrip_packet {|{"jsonrpc":"2.0","method":"$/cancelRequest","params":{"id":9}}|};
  [%expect
    {|
    Input:  {"jsonrpc":"2.0","method":"$/cancelRequest","params":{"id":9}}
    Output: {"params":{"id":9},"method":"$/cancelRequest","jsonrpc":"2.0"}
    |}]
;;

let%expect_test "initialize request roundtrip" =
  roundtrip_packet
    {|{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"processId":12345,"capabilities":{"textDocument":{"completion":{"completionItem":{"resolveSupport":{"properties":["documentation"]}}}}},"rootUri":"file:///workspace"}}|};
  [%expect
    {|
    Input:  {"jsonrpc":"2.0","id":0,"method":"initialize","params":{"processId":12345,"capabilities":{"textDocument":{"completion":{"completionItem":{"resolveSupport":{"properties":["documentation"]}}}}},"rootUri":"file:///workspace"}}
    Output: {"id":0,"params":{"processId":12345,"capabilities":{"textDocument":{"completion":{"completionItem":{"resolveSupport":{"properties":["documentation"]}}}}},"rootUri":"file:///workspace"},"method":"initialize","jsonrpc":"2.0"}
    |}]
;;

(** Test LSP message framing (Content-Length header + body) *)

let test_framing json_str =
  let json = Yojson.Safe.from_string json_str in
  let packet = Jsonrpc.Packet.t_of_yojson json in
  let output_json = Jsonrpc.Packet.yojson_of_t packet in
  let body = Yojson.Safe.to_string output_json in
  let header = Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body) in
  let framed = header ^ body in
  (* Verify the framed message has correct format *)
  match String.substr_index framed ~pattern:"\r\n\r\n" with
  | Some idx ->
    let header_part = String.prefix framed idx in
    let body_part = String.drop_prefix framed (idx + 4) in
    printf "Header: %s\n" header_part;
    printf "Body length: %d\n" (String.length body_part);
    printf "Declared length: %d\n" (String.length body);
    printf "Match: %b\n" (String.length body_part = String.length body)
  | None -> printf "ERROR: no header separator found\n"
;;

let%expect_test "framing format" =
  test_framing {|{"jsonrpc":"2.0","id":1,"method":"textDocument/hover","params":{}}|};
  [%expect
    {|
    Header: Content-Length: 66
    Body length: 66
    Declared length: 66
    Match: true
    |}]
;;
