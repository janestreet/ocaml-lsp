let get () =
  match Sys.getenv_opt "OCAMLLSP_TEST" with
  | Some "true" -> "NO_VERSION_UTIL"
  | _ -> "dev"
;;
