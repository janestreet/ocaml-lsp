open Import

val compute
  :  log_info:Log_info.t
  -> State.t
  -> InlayHintParams.t
  -> InlayHint.t list option Fiber.t
