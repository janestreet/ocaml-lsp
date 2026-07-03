open Import

let priority = Priorities.merlin_call_compatible
let capability = "handleMerlinCallCompatible", `Bool true
let meth = Lsp.Client_request.Custom_request_names.merlin_call_compatible
let get_doc_id = Util.get_doc_id

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; result_as_sexp : bool
    ; command : string
    ; args : string list
    }

  let create ~text_document ~result_as_sexp ~command ~args =
    { text_document; result_as_sexp; command; args }
  ;;

  let stringish_of_yojson
    =
    (* The function is relatively optimistic and attempts to treat literal data as strings
       of characters. *)
    function
    | `String s -> Some s
    | `Bool b -> Some (string_of_bool b)
    | `Float f -> Some (string_of_float f)
    | `Int i -> Some (string_of_int i)
    | `Intlit i -> Some i
    | _ -> None
  ;;

  let args_of_yojson_list args =
    let open Option.O in
    let+ args =
      List.fold_left
        ~f:(fun acc x ->
          let* acc = acc in
          let+ x = stringish_of_yojson x in
          x :: acc)
        ~init:(Some [])
        args
    in
    List.rev args
  ;;

  let args_of_yojson_assoc args =
    let open Option.O in
    let+ args =
      List.fold_left
        ~f:(fun acc (key, value) ->
          let key = "-" ^ key in
          let* acc = acc in
          let+ x = stringish_of_yojson value in
          x :: key :: acc)
        ~init:(Some [])
        args
    in
    List.rev args
  ;;

  let args_of_yojson json =
    let open Yojson.Safe.Util in
    match to_option (member "args") json with
    | Some (`List args) -> args |> args_of_yojson_list |> Option.value ~default:[]
    | Some (`Assoc args) -> args |> args_of_yojson_assoc |> Option.value ~default:[]
    | _ -> []
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let result_as_sexp = json |> member "resultAsSexp" |> to_bool in
    let command = json |> member "command" |> to_string in
    let args = args_of_yojson json in
    let text_document = TextDocumentIdentifier.t_of_yojson json in
    { text_document; result_as_sexp; command; args }
  ;;

  let yojson_of_t { text_document; result_as_sexp; command; args } =
    match TextDocumentIdentifier.yojson_of_t text_document with
    | `Assoc assoc ->
      let result_as_sexp = "resultAsSexp", `Bool result_as_sexp in
      let command = "command", `String command in
      let args = "args", `List (List.map ~f:(fun x -> `String x) args) in
      `Assoc (result_as_sexp :: command :: args :: assoc)
    | _ -> (* unreachable *) assert false
  ;;
end

(* This type and its yojson conversions are defined in [Lsp.Client_request] so that the
   remote-lsp can import them. If you need to change them, do it there, not here! *)
type t = Lsp.Client_request.Call_compatible_result.t

let t_of_yojson = Lsp.Client_request.Call_compatible_result.t_of_yojson
let yojson_of_t = Lsp.Client_request.Call_compatible_result.yojson_of_t

let with_pipeline ~log_info state uri specs raw_args cmd_args f =
  let doc = Document_store.get state.State.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return `Null
  | `Merlin merlin ->
    let open Fiber.O in
    let* config = Document.Merlin.mconfig merlin in
    let specs = List.map ~f:snd specs in
    let config, args =
      Mconfig.parse_arguments
        ~wd:(Sys.getcwd ())
        ~warning:ignore
        specs
        raw_args
        config
        cmd_args
    in
    Document.Merlin.with_configurable_pipeline_exn
      ~log_info
      ~config
      ~priority
      merlin
      (f args)
;;

let raise_invalid_params ?data ~message () =
  let open Jsonrpc.Response.Error in
  raise @@ make ?data ~code:Code.InvalidParams ~message ()
;;

let perform_query action params pipeline =
  let action () = action pipeline params in
  let class_, output =
    match action () with
    | result -> "return", result
    | exception Failure message -> "failure", `String message
    | exception exn ->
      let message = Printexc.to_string exn in
      "exception", `String message
  in
  `Assoc [ "class", `String class_; "value", output ]
;;

let serialize_results ~result_as_sexp json =
  let result =
    if result_as_sexp
    then Merlin_utils.(json |> Sexp.of_json |> Sexp.to_string)
    else json |> Yojson.Basic.to_string
  in
  let (result : t) = { result_as_sexp; result } in
  yojson_of_t result
;;

let handle_using_merlin
  ~(log_info : Log_info.t)
  state
  { Request_params.result_as_sexp; command; args; text_document }
  =
  let log_info = Log_info.update_action ~suffix:("-" ^ command) log_info in
  match Merlin_commands.New_commands.(find_command command all_commands) with
  | Merlin_commands.New_commands.Command (_name, _doc, specs, params, action) ->
    let open Fiber.O in
    let uri = text_document.uri in
    let+ json =
      with_pipeline ~log_info state uri specs args params @@ perform_query action
    in
    serialize_results ~result_as_sexp json
  | exception Not_found ->
    let data = `Assoc [ "command", `String command ] in
    raise_invalid_params ~data ~message:"Unexpected command name" ()
;;

(* There are many JSON variants, namely [Json.t], which is the most general, and
   [Merlin_utils.Std.Json.t], which is a subset of [Json.t]. The return type of custom
   requests has to be the most general, but we know that what we get back from remote-lsp
   is actually the subset [Merlin_utils.Std.Json.t]. To reuse merlin's sexp machinery, we
   convert from the former to the latter. *)
let rec to_merlin_json (json : Json.t) : Merlin_utils.Std.Json.t =
  match json with
  | (`Null | `Bool _ | `Int _ | `Float _ | `String _) as json -> json
  | `Assoc l -> `Assoc (List.map l ~f:(fun (key, json) -> key, to_merlin_json json))
  | `List l -> `List (List.map l ~f:to_merlin_json)
  | `Intlit _ ->
    failwith [%string "key `Intlit not allowed in merlin json: %{Json.to_string json}"]
  | `Tuple _ ->
    failwith [%string "key `Tuple not allowed in merlin json: %{Json.to_string json}"]
  | `Variant _ ->
    failwith [%string "key `Variant not allowed in merlin json: %{Json.to_string json}"]
;;

(* Because the [UnknownRequest] type is very generic and the remote LSP has different
   workspace paths than the local LSP, the [call_compatible_query] ends up having to reach
   into the json to modify paths. In particular, it looks for:

   1) a ["uri"] field in the top-level [`Assoc] of [params_json], which it translates to a
      uri that should be valid in its own workspace,
   2) an ouput json that's compatible with [Result.t_of_yojson] so that it can unwrap it
      and pull out the [result] field, and
   3) any ["file"] fields in the output json, which it translates back from its workspace
      into local paths.

   See [app/code-search/remote-lsp/lib/lsp_client.ml] for more details. *)
let handle_with_remote_lsp ~log_info ~result_as_sexp ~params_json server state uri =
  let open Fiber.O in
  let failure =
    `Assoc [ "class", `String "failure"; "value", `String "remote-lsp fallback failed" ]
  in
  let* json =
    match Remote_lsp.call_compatible_query ~log_info state uri ~params:params_json with
    | Some json -> json >>| Option.value ~default:failure
    | None -> Fiber.return failure
  in
  let json = to_merlin_json json in
  (* This message appears on the bottom line in vim and emacs, and as a popup in the
     bottom right in vscode. *)
  let+ () =
    task_if_running state.detached ~f:(fun () ->
      Server.notification
        server
        (ShowMessage
           (ShowMessageParams.create
              ~message:"remote-lsp: Merlin results may be out of date"
              ~type_:Info)))
  in
  serialize_results ~result_as_sexp json
;;

let on_request ~(log_info : Log_info.t) ~params:params_json server state =
  let open Fiber.O in
  let params = (Option.value ~default:(`Assoc []) params_json :> Json.t) in
  let params = Request_params.t_of_yojson params in
  let uri = params.text_document.uri in
  let* merlin_can_answer_queries =
    Merlin_status.merlin_can_answer_queries ~log_info state uri
  in
  (* For now, we only fall back to merlin on [locate] (go to definition/declaration) and
     [locate-type] (go to type declaration) queries, which are sent by emacs via
     ocaml-eglot. *)
  let use_remote_lsp_fallback =
    State.should_fall_back state
    && List.mem [ "locate"; "locate-type" ] params.command ~equal:String.equal
  in
  (* If we are in a test, merlin will not be configured, but will still be able answer
     queries as it won't have to cross file boundaries. Querying remote-lsp is thus a)
     unnecessary and b) likely to fail as the test LSP was probably not first passed a
     file with a valid feature-id (e.g. "foo.ml"). *)
  match merlin_can_answer_queries || Core.am_running_test with
  | true -> handle_using_merlin ~log_info state params
  | false ->
    (match use_remote_lsp_fallback with
     | false -> handle_using_merlin ~log_info state params
     | true ->
       let result_as_sexp = params.result_as_sexp in
       handle_with_remote_lsp ~log_info ~result_as_sexp ~params_json server state uri)
;;
