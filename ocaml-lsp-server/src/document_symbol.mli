open Import

val run
  :  log_info:Log_info.t
  -> State.t
  -> Document.t
  -> Uri.t
  -> [> `DocumentSymbol of DocumentSymbol.t list
     | `SymbolInformation of SymbolInformation.t list
     ]
       option
       Fiber.t
