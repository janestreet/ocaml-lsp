let _TEST () : bool option =
  let%map.Core.Option v = Sys.getenv_opt "OCAMLLSP_TEST" in
  match v with
  | "true" -> true
  | "false" -> false
  | unexpected_val ->
    Format.eprintf
      "invalid value %S for OCAMLLSP_TEST ignored. Only true or false are allowed@."
      unexpected_val;
    false
;;

(* Overrides the standard library directory merlin uses to type documents. The main use
   case is the OSS build's test suite: there, merlin-lib comes from the oxcaml repo but
   the opam switch's stdlib is built by the vanilla upstream compiler, whose cmi format
   OxCaml merlin cannot read. [make runtestoss] builds an OxCaml stdlib and points the
   server at it via this variable. *)
let _STDLIB () : string option = Sys.getenv_opt "OCAMLLSP_STDLIB"

let _IS_HOVER_EXTENDED () : bool option =
  let%bind.Core.Option v = Sys.getenv_opt "OCAMLLSP_HOVER_IS_EXTENDED" in
  match v with
  | "true" | "1" -> Some true
  | _ -> Some false
;;
