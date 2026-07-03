(** Writer for structured logging. *)
type t

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
