open Import
open Fiber.O
module Fiber_extensions = Ocaml_lsp_fiber_shims

let priority = Priorities.code_action

module Code_action_error = struct
  type t =
    | Initial
    | Need_merlin_extend of string
    | Exn of Exn_with_backtrace.t

  let empty = Initial

  let combine x y =
    match x, y with
    | Initial, _ -> y (* [Initial] cedes to any *)
    | _, Initial -> x
    | Exn _, _ -> x (* [Exn] takes over any *)
    | _, Exn _ -> y
    | Need_merlin_extend _, Need_merlin_extend _ -> y
  ;;
end

let compute_ocaml_code_actions ~log_info (params : CodeActionParams.t) state doc =
  let action_is_enabled =
    match params.context.only with
    | None -> fun _ -> true
    | Some set ->
      fun (action : Code_action.t) -> List.mem set action.kind ~equal:Poly.equal
  in
  let enabled_actions =
    List.filter
      ~f:action_is_enabled
      [ Action_destruct_line.t state
      ; Action_destruct.t state
      ; Action_update_signature.t state
      ; Action_combine_cases.t
      ; Action_inferred_intf.t state
      ; Action_type_annotate.t
      ; Action_remove_type_annotation.t
      ; Action_refactor_open.unqualify
      ; Action_refactor_open.qualify
      ; Action_add_rec.t
      ; Action_mark_remove_unused.mark
      ; Action_mark_remove_unused.remove
      ; Action_inline.t
      ; Action_extract.local
      ; Action_extract.function_
      ; Action_wrap_type_in_module.t
      ]
  in
  let batchable, non_batchable =
    List.partition_map
      ~f:(fun ca ->
        match ca.run with
        | `Batchable f -> First f
        | `Non_batchable f -> Second f)
      enabled_actions
  in
  let* batch_results =
    if List.is_empty batchable
    then Fiber.return []
    else
      Document.Merlin.with_pipeline_exn
        ~log_info
        ~priority
        (Document.merlin_exn doc)
        (fun pipeline ->
           List.filter_map batchable ~f:(fun ca ->
             try ca pipeline doc params with
             | Merlin_extend.Extend_main.Handshake.Error _ -> None))
  in
  let code_action ca =
    let+ res =
      Fiber_extensions.map_reduce_errors_with_monoid
        (module Code_action_error)
        ~on_error:(fun exn ->
          match exn.exn with
          | Merlin_extend.Extend_main.Handshake.Error error ->
            Fiber.return (Code_action_error.Need_merlin_extend error)
          | _ -> Fiber.return (Code_action_error.Exn exn))
        (fun () -> ca ~log_info doc params)
    in
    match res with
    | Ok res -> res
    | Error Initial -> assert false
    | Error (Need_merlin_extend _) -> None
    | Error (Exn exn) -> Exn_with_backtrace.reraise exn
  in
  let* non_batch_results =
    Fiber.parallel_map non_batchable ~f:code_action |> Fiber.map ~f:List.filter_opt
  in
  let+ construct_results =
    Action_construct.get_construct_actions ~log_info state doc params
  in
  batch_results @ non_batch_results @ construct_results
;;

let compute ~log_info server (params : CodeActionParams.t) =
  let state : State.t = Server.state server in
  let uri = params.textDocument.uri in
  let doc =
    let store = state.store in
    Document_store.get_opt store uri
  in
  let actions = function
    | [] -> None
    | xs -> Some (List.map ~f:(fun a -> `CodeAction a) xs)
  in
  match doc with
  | None -> Fiber.return (Reply.now None, state)
  | Some doc ->
    (match Document.syntax doc with
     | Ocamllex | Menhir | Cram | Dune -> Fiber.return (Reply.now None, state)
     | Ocaml | Reason ->
       let reply () =
         let+ code_action_results =
           compute_ocaml_code_actions ~log_info params state doc
         in
         actions code_action_results
       in
       let later f =
         Fiber.return
           ( Reply.later (fun k ->
               let* resp = f () in
               k resp)
           , state )
       in
       later reply)
;;
