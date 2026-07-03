module Fiber = Ocaml_lsp_fiber
module List = ListLabels

include struct
  module Code_error = Ocaml_lsp_stdune.Code_error
  module Exn_with_backtrace = Ocaml_lsp_stdune.Exn_with_backtrace
  module Sexp = Base.Sexp
end

include struct
  open Jsonrpc
  module Id = Id
  module Response = Response
  module Request = Request
  module Notification = Notification
  module Packet = Packet
end

module Json = struct
  type t = Ppx_yojson_conv_lib.Yojson.Safe.t

  let to_pretty_string (t : t) = Yojson.Safe.pretty_to_string ~std:false t
  let error = Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
  let pp ppf (t : t) = Yojson.Safe.pretty_print ppf t

  let rec of_sexp (t : Sexp.t) : t =
    match t with
    | Atom s -> `String s
    | List xs -> `List (List.map ~f:of_sexp xs)
  ;;
end

module Log = struct
  let level : (string option -> bool) ref = ref (fun _ -> false)
  let out = ref Format.err_formatter

  type message =
    { message : string
    ; payload : (string * Json.t) list
    }

  let msg message payload = { message; payload }

  let log ?section k =
    if !level section
    then (
      let message = k () in
      (match section with
       | None -> Format.fprintf !out "%s@." message.message
       | Some section -> Format.fprintf !out "[%s] %s@." section message.message);
      (match message.payload with
       | [] -> ()
       | fields -> Format.fprintf !out "%a@." Json.pp (`Assoc fields));
      Format.pp_print_flush !out ())
  ;;
end

let sprintf = Printf.sprintf

let () =
  (Printexc.register_printer [@ocaml.alert "-unsafe_multidomain"]) (function
    | Jsonrpc.Response.Error.E t ->
      let json = Jsonrpc.Response.Error.yojson_of_t t in
      Some ("jsonrpc response error " ^ Json.to_pretty_string (json :> Json.t))
    | _ -> None)
;;
