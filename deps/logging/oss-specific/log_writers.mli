(** Convenience logging operations, re-exported at the [Ocaml_lsp_logging] top level. *)

module Fiber = Ocaml_lsp_fiber

(** Times the execution of synchronous function f and logs the result with default
    category [lsp timing]. *)
val with_logging
  :  ?message:string
  -> ?category:string
  -> f:(unit -> 'a)
  -> Log_info.t
  -> 'a

(** Times the execution of asynchronous Fiber f and logs the result with default category
    [lsp timing]. *)
val with_fiber_logging
  :  ?message:string
  -> ?category:string
  -> f:(unit -> 'a Fiber.t)
  -> Log_info.t
  -> 'a Fiber.t

(** Logs merlin timing observations with category "merlin timing". The logged values are
    in milliseconds. *)
val log_merlin_timing
  :  ?message:string
  -> enqueue_time:Core.Time_ns.t
  -> exec_start_time:Core.Time_ns.t
  -> exec_stop_time:Core.Time_ns.t
  -> timing_breakdown:(string * float) list
  -> cache_information:Merlin_utils.Std.json
  -> Log_info.t
  -> unit

(** Logs a generic event. *)
val log_event
  :  ?message:string
  -> ?category:string
  -> ?info:Core.Sexp.t
  -> Log_info.t
  -> unit

(** Logs priority queue stats to Structured logging. *)
val log_queue_stats
  :  action:string
  -> realtime_size:int
  -> background_size:int
  -> total_size:int
  -> Session.t option
  -> unit

(** Logs reference counts with category "references". *)
val log_reference_counts
  :  ?message:string
  -> local_count:int
  -> remote_count:int
  -> merged_count:int
  -> Log_info.t
  -> unit

(** Add tags to [Async.Log.Global]. No-op in OSS builds. *)
val async_log_global_add_tags : tags:(string * string) list -> unit

(** Log primitive message, used when session and event info are not available *)
val log_message : ?info:Core.Sexp.t -> string -> unit

(** Log primitive debug message, used when session and event info are not available *)
val log_debug : ?info:Core.Sexp.t -> string -> unit

(** Log primitive error, used when session and event info are not available *)
val log_error : ?info:Core.Sexp.t -> string -> unit
