open! Import

(** Implements [textDocument/documentColor]. *)
val compute
  :  log_info:Log_info.t
  -> State.t Server.t
  -> Lsp.Types.DocumentColorParams.t
  -> (Lsp.Types.ColorInformation.t list Reply.t * State.t) Fiber.t
