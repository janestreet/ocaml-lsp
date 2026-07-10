open! Import
open! Fiber.O

let capability = "handleCompletePrefixAtPos", `Bool true
let meth = Lsp.Client_request.Custom_request_names.complete_prefix_at_pos
let get_doc_id = Util.get_doc_id
let get_pos = Util.get_pos

module Request_params = struct
  open Json.Conv

  type t =
    { text_document : TextDocumentIdentifier.t [@key "textDocument"]
    ; position : Position.t
    ; prefix : string
    }
  [@@deriving yojson]

  let params_schema =
    `Assoc
      [ "textDocument", `String "<TextDocumentIdentifier>"
      ; "position", `String "<Position>"
      ; "prefix", `String "<string>"
      ]
  ;;

  let of_jsonrpc_params params =
    try Some (t_of_yojson (Jsonrpc.Structured.yojson_of_t params)) with
    | _ -> None
  ;;

  let of_jsonrpc_params_exn params =
    let params_spec = Util.{ params_schema; of_jsonrpc_params } in
    Util.of_jsonrpc_params_exn params_spec params
  ;;
end

let serialize (completions, labels) =
  `Assoc
    [ "entries", Json.Conv.yojson_of_list CompletionItem.yojson_of_t completions
    ; ( "context"
        (* The complex, seemingly redundant structure here is intended to mirror the
           output of 'merlin complete-prefix'. One day there may be forms of context other
           than just function application. *)
      , Option.value_map labels ~default:`Null ~f:(fun labels ->
          `List
            [ `String "application"
            ; `Assoc
                [ ( "labels"
                  , Json.Conv.yojson_of_list
                      (fun (n, t) -> `Assoc [ "name", `String n; "type", `String t ])
                      labels )
                ]
            ]) )
    ]
;;

let on_request ~log_info ~(params : Jsonrpc.Structured.t option) (state : State.t) =
  Fiber.of_thunk (fun () ->
    let Request_params.{ text_document = { uri }; position; prefix } =
      Request_params.of_jsonrpc_params_exn params
    in
    let doc = Document_store.get state.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return (`Assoc [ "entries", `Null; "context", `Null ])
    | `Merlin merlin ->
      Compl.Complete_by_prefix.complete_nosquash
        ~log_info
        state
        merlin
        ~prefix
        ~suffix:""
        position
        ~resolve:false
        ~deprecated:false
      >>| serialize)
;;

module For_testing = struct
  let serialize = serialize
end
