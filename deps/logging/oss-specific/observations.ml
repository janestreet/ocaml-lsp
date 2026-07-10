open! Core

module Pipeline_timing = struct
  type t =
    { error : float
    ; pp : float
    ; ppx : float
    ; reader : float
    ; typer : float
    ; total : float
    }

  let create timing_breakdown =
    let extract component =
      List.Assoc.find timing_breakdown ~equal:String.equal component
      |> Option.value ~default:Float.nan
    in
    { error = extract "error"
    ; pp = extract "pp"
    ; ppx = extract "ppx"
    ; reader = extract "reader"
    ; typer = extract "typer"
    ; total = List.sum (module Float) timing_breakdown ~f:snd
    }
  ;;
end

module Lsp = struct
  type t =
    { request_wall_time : Time_ns.Span.t
    ; handler_time : Time_ns.Span.t
    ; request_queue_time : Time_ns.Span.t
    ; input_delay : Time_ns.Span.t option
    }

  let create ~request_time ~enqueue_time ~exec_start_time ~exec_stop_time =
    let arrival_time = Option.value request_time ~default:enqueue_time in
    { request_wall_time = Time_ns.diff exec_stop_time arrival_time
    ; handler_time = Time_ns.diff exec_stop_time exec_start_time
    ; request_queue_time = Time_ns.diff exec_start_time enqueue_time
    ; input_delay = Option.map request_time ~f:(fun t -> Time_ns.diff exec_start_time t)
    }
  ;;
end

module Merlin = struct
  type t =
    { wall_time : Time_ns.Span.t
    ; pipeline_time : Time_ns.Span.t
    ; queue_time : Time_ns.Span.t
    ; pipeline_timing : Pipeline_timing.t
    ; typer_cache_hit : bool
    ; reader_cache_hit : bool
    ; ppx_cache_hit : bool
    }

  let create
    ~enqueue_time
    ~exec_start_time
    ~exec_stop_time
    ~timing_breakdown
    ~cache_information:_
    =
    (* TODO: if upstream Merlin provides cache information, extract and log it. *)
    { wall_time = Time_ns.diff exec_stop_time enqueue_time
    ; pipeline_time = Time_ns.diff exec_stop_time exec_start_time
    ; queue_time = Time_ns.diff exec_start_time enqueue_time
    ; pipeline_timing = Pipeline_timing.create timing_breakdown
    ; typer_cache_hit = false
    ; reader_cache_hit = false
    ; ppx_cache_hit = false
    }
  ;;

  let query_time t =
    Time_ns.Span.( - ) t.pipeline_time (Time_ns.Span.of_sec t.pipeline_timing.total)
  ;;
end
