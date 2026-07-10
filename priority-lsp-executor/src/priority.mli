type t =
  | Background
  (** Tasks that should be executed when no Realtime tasks are left. These should
      correspond to requests that happen in the background, such as a cache warmup. *)
  | Realtime
  (** Tasks that should be executed as soon as possible. These should correspond to
      requests due to an explicit user action, such as a go-to-definition request. *)
