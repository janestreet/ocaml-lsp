open Import
open Fiber.O

let priority = Priorities.merlin_can_answer_queries

let log_config_failures ~log_info (config : Mconfig.t) =
  List.iter config.merlin.failures ~f:(fun failure ->
    Ocaml_lsp_logging.log_event
      ~message:failure
      ~category:"merlin config failure"
      log_info)
;;

let no_config_failures ~log_info (config : Mconfig.t) =
  log_config_failures ~log_info config;
  List.is_empty config.merlin.failures
;;

let merlin_can_answer_queries ~log_info (state : State.t) uri =
  let doc = Document_store.get state.store uri in
  let+ config =
    Document.Merlin.with_pipeline_exn
      ~log_info
      ~priority
      (Document.merlin_exn doc)
      (fun pipeline -> Mpipeline.final_config pipeline)
  in
  no_config_failures ~log_info config
;;

let merlin_pipeline_can_answer_queries ~log_info pipeline =
  no_config_failures ~log_info (Mpipeline.final_config pipeline)
;;
