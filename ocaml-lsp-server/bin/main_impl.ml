open Core
open Async

module Channel = struct
  type t = Lsp.Cli.Channel.t =
    | Stdio
    | Pipe of string
    | Socket of int
end

let command ~is_deployed_binary =
  Command.async
    ~summary:"OCaml LSP"
    (let%map_open.Command () = return ()
     and channel =
       choose_one_non_optional
         [ flag
             "stdio"
             (no_arg_required Channel.Stdio)
             ~doc:"Communicate over stdio (default)"
         ; flag
             "port"
             (required int |> map_flag ~f:(fun port -> Channel.Socket port))
             ~doc:"PORT Communicate over an internet socket (127.0.0.1:PORT)"
         ; flag
             "unix"
             (required string |> map_flag ~f:(fun unix -> Channel.Pipe unix))
             ~doc:"UNIX Communicate over a unix domain socket"
         ]
         ~if_nothing_chosen:(Default_to Stdio)
     and client_pid =
       flag
         "client-pid"
         (optional int)
         ~doc:
           "PID Process id of client. When passed, the server will exit when this client \
            dies."
     and client_existence_check_interval =
       Ocaml_lsp_misc_shims.flag_optional_with_default_doc
         "check-client-existence-every"
         Time_ns.Span.arg_type
         [%sexp_of: Time_ns.Span.t]
         ~default:(Time_ns.Span.of_int_sec 10)
         ~doc:
           "SPAN How frequently to check if client exists. Only relevant with \
            [-client-pid]."
     and dot_merlin =
       flag
         "dot-merlin"
         (optional Filename_unix.arg_type)
         ~doc:"FILE Path to .merlin file. Used for Jane Script and OCaml_plugin."
     and deployment_stage =
       flag
         "deployment-stage"
         (optional
            (Command.Arg_type.enumerated_sexpable
               ~case_sensitive:false
               ~list_values_in_help:true
               (module Ocaml_lsp_server.Stage.Prod_or_dev)))
         ~doc:"DEPLOYMENT_STAGE whether this ocaml-lsp is deployed to DEV or PROD"
     and worker_name =
       flag
         "worker-name"
         (optional string)
         ~doc:
           "NAME Logical name of this ocaml-lsp worker (typically set by \
            [ocaml-lsp-wrapper]). Included in logs and structured logging datasets. Do \
            not set if running [ocaml-lsp] directly"
     in
     fun () ->
       Option.iter client_pid ~f:(fun pid ->
         Clock_ns.every' client_existence_check_interval (fun () ->
           match Signal_unix.can_send_to (Pid.of_int pid) with
           | true -> return ()
           | false -> exit 0));
       let stage =
         match is_deployed_binary, deployment_stage with
         | true, None -> Ocaml_lsp_server.Stage.Deployed Prod
         | _, Some ds -> Ocaml_lsp_server.Stage.Deployed ds
         | false, None -> Ocaml_lsp_server.Stage.Localhost
       in
       Ocaml_lsp_server.run channel ~dot_merlin ~stage ~worker_name)
;;
