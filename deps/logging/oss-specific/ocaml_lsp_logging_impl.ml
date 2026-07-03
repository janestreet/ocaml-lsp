module Event_info = Event_info
module Log_info = Log_info
module Observations = Observations
module Session_info = Session_info
module Structured_logging = Structured_logging

type t = Session.t [@@deriving sexp_of]

let init = Session.init
let close = Session.close

include Log_writers
