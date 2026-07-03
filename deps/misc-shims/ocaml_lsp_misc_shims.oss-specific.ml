open Core

let split_n_boxed l n = Core.List.split_n l n
let elide_backtrace v = Base.Backtrace.elide := v
let ppx_string_config_for_string = Ppx_string.config_for_string
let set_once_set_if_none t ~here value = Core.Set_once.set_if_none t here value

let flag_optional_with_default_doc name arg_type sexp_of_default ~default ~doc =
  Command.Param.flag_optional_with_default_doc name arg_type sexp_of_default ~default ~doc
;;
