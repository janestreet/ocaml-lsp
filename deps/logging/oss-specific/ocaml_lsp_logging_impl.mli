module Event_info = Event_info
module Log_info = Log_info
module Observations = Observations
module Session_info = Session_info
module Structured_logging = Structured_logging

type t = Session.t

val init
  :  deployment_stage:Stage.t
  -> editor:string
  -> editor_version:string
  -> worker_name:string option
  -> t Async.Deferred.t

val close : t -> unit Core.Or_error.t Async.Deferred.t

include module type of Log_writers
