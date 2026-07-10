module Fiber = Ocaml_lsp_fiber
module List = Stdlib.ListLabels
module Code_error = Ocaml_lsp_stdune.Code_error
module Fdecl = Ocaml_lsp_stdune.Fdecl
module Header = Lsp.Header
module Io = Lsp.Io
module Time_ns = Core.Time_ns

module Json = struct
  include Lsp.Json

  let pp ppf (t : t) = Yojson.Safe.pretty_print ppf t
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

module Types = Lsp.Types
module Client_request = Lsp.Client_request
module Server_request = Lsp.Server_request
module Server_notification = Lsp.Server_notification
module Client_notification = Lsp.Client_notification

module Jrpc_id = struct
  include Jsonrpc.Id

  let to_dyn = function
    | `String s -> Dyn.String s
    | `Int i -> Dyn.Int i
  ;;
end
