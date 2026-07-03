type error =
  { msg : string
  ; loc : Location.t
  }

val parse_file : path:string -> Lexing.lexbuf -> (unit, error) result
