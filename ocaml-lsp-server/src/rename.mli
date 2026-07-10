open Import

val prepare_rename
  :  log_info:Log_info.t
  -> State.t
  -> PrepareRenameParams.t
  -> Range.t option Fiber.t

val rename
  :  log_info:Log_info.t
  -> State.t
  -> RenameParams.t
  -> WorkspaceEdit.t option Fiber.t
