(** Run ocaml-lsp-server binary in a deployed configuration *)
let () = Command_unix.run (Main_impl.command ~is_deployed_binary:true)
