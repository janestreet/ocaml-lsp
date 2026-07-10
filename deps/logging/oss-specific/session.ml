open! Core

type t = { info : Session_info.t } [@@deriving sexp_of]

let init ~deployment_stage ~editor ~editor_version ~worker_name =
  let info = Session_info.create ~deployment_stage ~editor ~editor_version ~worker_name in
  Async.return { info }
;;

let close _ = Async.return (Ok ())
