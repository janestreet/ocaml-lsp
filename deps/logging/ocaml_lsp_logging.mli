module Fiber = Ocaml_lsp_fiber

module Event_info : sig
  (** Per-event fields that are logged *)
  type t = Ocaml_lsp_logging_impl.Event_info.t =
    { index : int
    (** Global index tracked in the lsp-server state; lets us associate multiple log
        entries to the same lsp request. *)
    ; action : string (** Name of the request/notification/etc. the lsp is handling. *)
    ; feature : string option
    (** Feature-id if it can be extracted and isn't sensitive. *)
    ; file : string option
    (** Path to the primary file from the repo-root if the feature is known. *)
    ; other_files : string list option
    (** Paths to other involved files from the repo-root if the feature is known. *)
    ; lines : int option (** Number of lines in the primary file. *)
    ; hash : int option (** A [String.hash] of the contents of the primary file. *)
    ; pos_line : int option (** Line of the cursor position in the request. *)
    ; pos_char : int option (** Character of the cursor position in the request. *)
    ; enqueue_time : Core.Time_ns.t (** Time when the request was enqueued. *)
    ; request_time : Core.Time_ns.t option
    (** Time when the request arrived. It is calculated by the wrapper, so might not be
        availble if ocaml-lsp is ran w/out one. *)
    }

  val create
    :  index:int
    -> action:string
    -> ?log_paths_and_features:bool
         (** When [false] (the default), feature/file/other_files are suppressed
             regardless of what [primary_uri]/[other_uris] say, to avoid emitting paths
             outside of a Jane Street repository. *)
    -> ?primary_uri:Lsp.Uri.t
    -> ?other_uris:Lsp.Uri.t list
    -> ?text:string
    -> ?position:Lsp.Types.Position.t
    -> ?request_time:Core.Time_ns.t
    -> unit
    -> t

  (** Returns a new [t] with [suffix] appended to the action name. *)
  val update_action : suffix:string -> t -> t
end

module Observations : sig
  (** Sub-step timing breakdown from [Mpipeline.timing_information]. All times are in
      milliseconds. *)
  module Pipeline_timing : sig
    type t = Ocaml_lsp_logging_impl.Observations.Pipeline_timing.t =
      { error : float (** Error fetching time. *)
      ; pp : float (** PP phase time. *)
      ; ppx : float (** PPX phase time. *)
      ; reader : float (** Reader phase time. *)
      ; typer : float (** Typer phase time. *)
      ; total : float (** Sum of all sub-step times. *)
      }

    (** Extracts pipeline sub-step timings from a [Mpipeline.timing_information]-style
        assoc list. Missing components default to [Float.nan]. *)
    val create : (string * float) list -> t
  end

  (** Timing observations for LSP request handlers. *)
  module Lsp : sig
    type t = Ocaml_lsp_logging_impl.Observations.Lsp.t =
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
    type t = Ocaml_lsp_logging_impl.Observations.Merlin.t =
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

    (** Pipeline execution time minus total pipeline sub-step time — i.e. time spent
        outside the known pipeline phases. *)
    val query_time : t -> Core.Time_ns.Span.t
  end
end

module Session_info : sig
  (** Session-level metadata included in every log row. This is common between all users
      of [ocaml-lsp-logging] *)
  type t = Ocaml_lsp_logging_impl.Session_info.t =
    { username : string
    ; deployment_stage : Stage.t
    ; log_paths_and_features : bool
    ; editor : string
    ; editor_version : string
    ; host : string
    ; ocaml_version : string
    ; build_version_jane : string option
    ; worker_name : string option
    (** Logical name of the ocaml-lsp worker process behind this session (as passed via
        [ocaml-lsp -worker-name]). Populated by [ocaml-lsp-wrapper] when it spawns workers
        so that each worker's logs can be distinguished. *)
    }

  val sexp_of_t : t -> Core.Sexp.t

  val create
    :  deployment_stage:Stage.t
    -> editor:string
    -> editor_version:string
    -> worker_name:string option
    -> t
end

module Structured_logging : sig
  (** Writer for structured logging. *)
  type t = Ocaml_lsp_logging_impl.Structured_logging.t

  val create : unit -> t Async.Deferred.t
  val close : t -> unit Core.Or_error.t Async.Deferred.t

  val log_lsp_timing
    :  t
    -> Session_info.t
    -> category:string
    -> message:string
    -> Event_info.t
    -> Observations.Lsp.t
    -> unit

  val log_merlin_timing
    :  t
    -> Session_info.t
    -> category:string
    -> message:string
    -> Event_info.t
    -> Observations.Merlin.t
    -> unit

  val log_queue_stats
    :  t
    -> Session_info.t
    -> action:string
    -> realtime_size:int
    -> background_size:int
    -> total_size:int
    -> unit
end

type t = Ocaml_lsp_logging_impl.t

module Log_info : sig
  (** Per-request logging context. Called [log_info] at use sites. *)
  type t = Ocaml_lsp_logging_impl.Log_info.t =
    { session : Ocaml_lsp_logging_impl.t option
    ; event : Event_info.t
    }

  val create
    :  Ocaml_lsp_logging_impl.t option
    -> event_index:int
    -> action:string
    -> ?primary_uri:Lsp.Uri.t
    -> ?other_uris:Lsp.Uri.t list
    -> ?text:string
    -> ?position:Lsp.Types.Position.t
    -> ?request_time:Core.Time_ns.t
    -> unit
    -> t

  (** Returns a new [t] with [suffix] appended to the action name. *)
  val update_action : suffix:string -> t -> t
end

val init
  :  deployment_stage:Stage.t
  -> editor:string
  -> editor_version:string
  -> worker_name:string option
  -> t Async.Deferred.t

val close : t -> unit Core.Or_error.t Async.Deferred.t

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

(** Logs priority queue stats to structured logging. *)
val log_queue_stats
  :  action:string
  -> realtime_size:int
  -> background_size:int
  -> total_size:int
  -> t option
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
