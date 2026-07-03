(** Sub-step timing breakdown from [Mpipeline.timing_information]. All times are in
    milliseconds. *)
module Pipeline_timing : sig
  type t =
    { error : float (** Error fetching time. *)
    ; pp : float (** PP phase time. *)
    ; ppx : float (** PPX phase time. *)
    ; reader : float (** Reader phase time. *)
    ; typer : float (** Typer phase time. *)
    ; total : float (** Sum of all sub-step times. *)
    }

  (** Extracts pipeline sub-step timings from a [Mpipeline.timing_information]-style assoc
      list. Missing components default to [Float.nan]. *)
  val create : (string * float) list -> t
end

(** Timing observations for LSP request handlers. *)
module Lsp : sig
  type t =
    { request_wall_time : Core.Time_ns.Span.t
    (** Time between receiving the LSP request and having the response ready. Measured
        using a [request_time] from [ocaml-lsp-wrapper] when possible. *)
    ; handler_time : Core.Time_ns.Span.t
    (** Time between the start and the end of request handler execution (not including
        queuing). *)
    ; request_queue_time : Core.Time_ns.Span.t
    (** Time waiting in the LSP priority queue before the handler starts. *)
    ; input_delay : Core.Time_ns.Span.t option
    (** Time the request spent sitting in ocaml-lsp's stdin. [None] when the
        [ocaml-lsp-wrapper] doesn't provide a [request_time]. *)
    }

  (** Computes timing observations from raw timestamps. *)
  val create
    :  request_time:Core.Time_ns.t option
    -> enqueue_time:Core.Time_ns.t
    -> exec_start_time:Core.Time_ns.t
    -> exec_stop_time:Core.Time_ns.t
    -> t
end

(** Timing observations for Merlin pipeline requests. *)
module Merlin : sig
  type t =
    { wall_time : Core.Time_ns.Span.t
    (** Time from merlin enqueue to pipeline completion. *)
    ; pipeline_time : Core.Time_ns.Span.t
    (** Time the merlin pipeline was executing (not including thread queue wait). *)
    ; queue_time : Core.Time_ns.Span.t
    (** Time waiting for the merlin thread to start executing the pipeline. *)
    ; pipeline_timing : Pipeline_timing.t
    (** Sub-step breakdown of the pipeline (pp, ppx, reader, typer, error). *)
    ; typer_cache_hit : bool
    ; reader_cache_hit : bool
    ; ppx_cache_hit : bool
    }

  (** Computes merlin timing observations from raw timestamps and pipeline timing
      breakdown. *)
  val create
    :  enqueue_time:Core.Time_ns.t
    -> exec_start_time:Core.Time_ns.t
    -> exec_stop_time:Core.Time_ns.t
    -> timing_breakdown:(string * float) list
    -> cache_information:Merlin_utils.Std.json
    -> t

  (** Pipeline execution time minus total pipeline sub-step time — i.e. time spent outside
      the known pipeline phases. *)
  val query_time : t -> Core.Time_ns.Span.t
end
