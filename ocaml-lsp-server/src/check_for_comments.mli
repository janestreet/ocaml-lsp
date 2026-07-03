open! Import

(** Returns [true] if [position] occurs inside a comment in the document *)
val position_in_comment
  :  log_info:Log_info.t
  -> position:Position.t
  -> merlin:Document.Merlin.t
  -> priority:Priority.t
  -> bool Fiber.t
