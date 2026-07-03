open! Core

[@@@alert "-private_lsp_library"]

module Jsonrpc = Lsp_json_rpc_types.Jsonrpc

let%expect_test "t_of_yojson works with optional request_time field present in requests" =
  let json_with_request_time =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "id", `Int 1
      ; "method", `String "initialize"
      ; "params", `Assoc [ "rootUri", `Null ]
      ; "request_time", `String "2024-01-23T12:00:00.000000Z"
      ]
  in
  let packet = Jsonrpc.Packet.t_of_yojson json_with_request_time in
  (match packet with
   | Jsonrpc.Packet.Request { id; method_; params; request_time; event_index = _ } ->
     let params_str =
       Option.map params ~f:(fun p ->
         Yojson.Safe.to_string (Jsonrpc.Structured.yojson_of_t p))
     in
     (* NB: [Core.Time_ns.sexp_of_t] is deprecated in the OSS [core], so let's avoid using
        it:
        https://ocaml.org/p/core/v0.17.1/doc/core/Core/Time_ns/index.html#val-sexp_of_t *)
     let request_time : string option =
       Option.map request_time ~f:Time_ns.to_string_utc
     in
     print_s
       [%message
         "Parsed request"
           (id : [ `Int of int | `String of string ])
           (method_ : string)
           (params_str : string option)
           (request_time : string option)]
   | _ -> print_s [%message "Expected a request"]);
  [%expect
    {|
    ("Parsed request" (id (Int 1)) (method_ initialize)
     (params_str ("{\"rootUri\":null}"))
     (request_time ("2024-01-23 12:00:00.000000000Z")))
    |}]
;;

let%expect_test "t_of_yojson works without optional request_time field in requests" =
  let json_with_request_time =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "id", `Int 1
      ; "method", `String "initialize"
      ; "params", `Assoc [ "rootUri", `Null ]
      ]
  in
  let packet = Jsonrpc.Packet.t_of_yojson json_with_request_time in
  (match packet with
   | Jsonrpc.Packet.Request { id; method_; params; request_time; event_index = _ } ->
     let params_str =
       Option.map params ~f:(fun p ->
         Yojson.Safe.to_string (Jsonrpc.Structured.yojson_of_t p))
     in
     (* NB: [Core.Time_ns.sexp_of_t] is deprecated in the OSS [core], so let's avoid using
        it:
        https://ocaml.org/p/core/v0.17.1/doc/core/Core/Time_ns/index.html#val-sexp_of_t *)
     let request_time : string option =
       Option.map request_time ~f:Time_ns.to_string_utc
     in
     print_s
       [%message
         "Parsed request"
           (id : [ `Int of int | `String of string ])
           (method_ : string)
           (params_str : string option)
           (request_time : string option)]
   | _ -> print_s [%message "Expected a request"]);
  [%expect
    {|
    ("Parsed request" (id (Int 1)) (method_ initialize)
     (params_str ("{\"rootUri\":null}")) (request_time ()))
    |}]
;;

let%expect_test "t_of_yojson works with unexpected request_time field in notifications" =
  let json_with_request_time =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "method", `String "initialized"
      ; "params", `Assoc []
      ; "request_time", `String "2024-01-23T12:00:00.000000Z"
      ]
  in
  let packet = Jsonrpc.Packet.t_of_yojson json_with_request_time in
  (match packet with
   | Jsonrpc.Packet.Notification { method_; params; event_index = _ } ->
     let params_str =
       Option.map params ~f:(fun p ->
         Yojson.Safe.to_string (Jsonrpc.Structured.yojson_of_t p))
     in
     print_s
       [%message "Parsed notification" (method_ : string) (params_str : string option)]
   | _ -> print_s [%message "Expected a notification"]);
  [%expect {| ("Parsed notification" (method_ initialized) (params_str ({}))) |}]
;;

let%expect_test "t_of_yojson works with only expected fields in notifications" =
  let json_with_request_time =
    `Assoc
      [ "jsonrpc", `String "2.0"; "method", `String "initialized"; "params", `Assoc [] ]
  in
  let packet = Jsonrpc.Packet.t_of_yojson json_with_request_time in
  (match packet with
   | Jsonrpc.Packet.Notification { method_; params; event_index = _ } ->
     let params_str =
       Option.map params ~f:(fun p ->
         Yojson.Safe.to_string (Jsonrpc.Structured.yojson_of_t p))
     in
     print_s
       [%message "Parsed notification" (method_ : string) (params_str : string option)]
   | _ -> print_s [%message "Expected a notification"]);
  [%expect {| ("Parsed notification" (method_ initialized) (params_str ({}))) |}]
;;

let%expect_test "t_of_yojson reads event_index on requests and notifications" =
  let request_json =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "id", `Int 7
      ; "method", `String "initialize"
      ; "event_index", `Int 42
      ]
  in
  let packet = Jsonrpc.Packet.t_of_yojson request_json in
  (match packet with
   | Jsonrpc.Packet.Request { event_index; _ } ->
     print_s [%message "Request event_index" (event_index : int option)]
   | _ -> print_s [%message "Expected a request"]);
  [%expect {| ("Request event_index" (event_index (42))) |}];
  let notification_json =
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "method", `String "initialized"
      ; "params", `Assoc []
      ; "event_index", `Int 99
      ]
  in
  let packet = Jsonrpc.Packet.t_of_yojson notification_json in
  (match packet with
   | Jsonrpc.Packet.Notification { event_index; _ } ->
     print_s [%message "Notification event_index" (event_index : int option)]
   | _ -> print_s [%message "Expected a notification"]);
  [%expect {| ("Notification event_index" (event_index (99))) |}]
;;

let%expect_test "yojson_of_t round-trips event_index for requests and notifications" =
  let request =
    { Jsonrpc.Request.id = `Int 1
    ; method_ = "initialize"
    ; params = None
    ; request_time = None
    ; event_index = Some 5
    }
  in
  let json = Jsonrpc.Request.yojson_of_t request in
  print_endline (Yojson.Safe.to_string json);
  [%expect {| {"id":1,"event_index":5,"method":"initialize","jsonrpc":"2.0"} |}];
  let notification =
    { Jsonrpc.Notification.method_ = "initialized"; params = None; event_index = Some 12 }
  in
  let json = Jsonrpc.Notification.yojson_of_t notification in
  print_endline (Yojson.Safe.to_string json);
  [%expect {| {"event_index":12,"method":"initialized","jsonrpc":"2.0"} |}]
;;
