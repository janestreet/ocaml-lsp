open! Import
open Fiber.O

let priority = Priorities.color

let compute_colors ~log_info document : Lsp.Types.ColorInformation.t list Fiber.t =
  Document.Merlin.with_pipeline_exn ~log_info ~priority document (fun pipeline ->
    let parse_tree : Mreader.parsetree = Mpipeline.reader_parsetree pipeline in
    match parse_tree with
    | `Interface _ -> []
    | `Implementation structure -> Css_lsp.compute_colors structure)
;;

let compute ~log_info server (params : Lsp.Types.DocumentColorParams.t) =
  let state : State.t = Server.state server in
  let feature_enabled =
    Option.map state.configuration.data.ppx_css_colors ~f:(fun c -> c.enable)
  in
  let empty = Reply.now [], state in
  match feature_enabled with
  | None | Some false -> Fiber.return empty
  | Some true ->
    let uri = params.textDocument.uri in
    let doc : Document.t option =
      let store = state.store in
      Document_store.get_opt store uri
    in
    (match doc with
     | None -> Fiber.return empty
     | Some doc ->
       (match Document.syntax doc with
        | Reason | Ocamllex | Menhir | Cram | Dune -> Fiber.return empty
        | Ocaml ->
          (match Document.kind doc with
           | `Other -> Fiber.return empty
           | `Merlin doc ->
             Fiber.return
               ( Reply.later (fun send ->
                   let* colors = compute_colors ~log_info doc in
                   send colors)
               , state ))))
;;
