open! Core

type t =
  { session : Session.t option
  ; event : Event_info.t
  }

let create
  (session : Session.t option)
  ~event_index
  ~action
  ?primary_uri
  ?other_uris
  ?text
  ?position
  ?request_time
  ()
  =
  { session
  ; event =
      Event_info.create
        ~index:event_index
        ~action
        ~log_paths_and_features:false
        ?primary_uri
        ?other_uris
        ?text
        ?position
        ?request_time
        ()
  }
;;

let update_action ~suffix t = { t with event = Event_info.update_action ~suffix t.event }
