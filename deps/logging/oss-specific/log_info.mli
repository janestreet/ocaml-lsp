(** Per-request logging context. Called [log_info] at use sites. *)
type t =
  { session : Session.t option
  ; event : Event_info.t
  }

val create
  :  Session.t option
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
