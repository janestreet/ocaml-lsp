type error =
  { msg : string
  ; loc : Location.t
  }

let parse_file ~path:_ _ =
  let loc =
    { Location.loc_start = Lexing.dummy_pos
    ; loc_end = Lexing.dummy_pos
    ; loc_ghost = true
    }
  in
  Error { msg = "lrgrep currently not supported in OSS build"; loc }
;;
