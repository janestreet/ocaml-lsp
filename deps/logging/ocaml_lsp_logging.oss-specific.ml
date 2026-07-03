module Fiber = Ocaml_lsp_fiber
module Event_info = Ocaml_lsp_logging_impl.Event_info
module Log_info = Ocaml_lsp_logging_impl.Log_info
module Observations = Ocaml_lsp_logging_impl.Observations
module Session_info = Ocaml_lsp_logging_impl.Session_info
module Structured_logging = Ocaml_lsp_logging_impl.Structured_logging

type t = Ocaml_lsp_logging_impl.t

let init = Ocaml_lsp_logging_impl.init
let close = Ocaml_lsp_logging_impl.close
let with_logging = Ocaml_lsp_logging_impl.with_logging
let with_fiber_logging = Ocaml_lsp_logging_impl.with_fiber_logging
let log_merlin_timing = Ocaml_lsp_logging_impl.log_merlin_timing
let log_event = Ocaml_lsp_logging_impl.log_event
let log_queue_stats = Ocaml_lsp_logging_impl.log_queue_stats
let log_reference_counts = Ocaml_lsp_logging_impl.log_reference_counts
let async_log_global_add_tags = Ocaml_lsp_logging_impl.async_log_global_add_tags
let log_message = Ocaml_lsp_logging_impl.log_message
let log_debug = Ocaml_lsp_logging_impl.log_debug
let log_error = Ocaml_lsp_logging_impl.log_error
