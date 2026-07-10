open Import

val legend : SemanticTokensLegend.t

val on_request_full
  :  log_info:Log_info.t
  -> State.t
  -> SemanticTokensParams.t
  -> SemanticTokens.t option Fiber.t

val on_request_full_delta
  :  log_info:Log_info.t
  -> State.t
  -> SemanticTokensDeltaParams.t
  -> [ `SemanticTokens of SemanticTokens.t
     | `SemanticTokensDelta of SemanticTokensDelta.t
     ]
       option
       Fiber.t

module Debug : sig
  val meth_request_full : string
  val get_doc_id : params:Jsonrpc.Structured.t option -> TextDocumentIdentifier.t option

  val on_request_full
    :  log_info:Log_info.t
    -> params:Jsonrpc.Structured.t option
    -> State.t
    -> Json.t Fiber.t
end
