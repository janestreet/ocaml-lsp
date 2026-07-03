(* TODO: have these emit useful logs instead of stubbing them out completely. *)

module Fiber = Ocaml_lsp_fiber

let with_logging ?message:_ ?category:_ ~f _ = f ()
let with_fiber_logging ?message:_ ?category:_ ~f _ = f ()

let log_merlin_timing
  ?message:_
  ~enqueue_time:_
  ~exec_start_time:_
  ~exec_stop_time:_
  ~timing_breakdown:_
  ~cache_information:_
  _
  =
  ()
;;

let log_event ?message:_ ?category:_ ?info:_ _ = ()
let log_queue_stats ~action:_ ~realtime_size:_ ~background_size:_ ~total_size:_ _ = ()
let log_reference_counts ?message:_ ~local_count:_ ~remote_count:_ ~merged_count:_ _ = ()
let async_log_global_add_tags ~tags:_ = ()
let log_message ?info:_ _ = ()
let log_debug ?info:_ _ = ()
let log_error ?info:_ _ = ()
