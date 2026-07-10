let get_test_ocaml_lsp_bin () =
  let build_path_prefix_map =
    match Sys.getenv_opt "BUILD_PATH_PREFIX_MAP" with
    | Some build_path_prefix_map -> build_path_prefix_map
    | None -> failwith "BUILD_PATH_PREFIX_MAP is not set"
  in
  let sandbox_root =
    build_path_prefix_map
    |> String.split_on_char ':'
    |> List.find_map (fun item ->
      let prefix = "/workspace_root=" in
      Core.String.chop_prefix item ~prefix)
    |> Core.Option.value_exn
         ~message:
           "BUILD_PATH_PREFIX_MAP is missing, empty or has no /workspace_root entry"
  in
  let path = Filename.concat sandbox_root "ocaml-lsp-server/bin/main.exe" in
  if not (Sys.file_exists path) then failwith "ocaml-lsp binary does not exist";
  path
;;

module Expect_test_helpers = struct
  let require_does_raise ~here f = Expect_test_helpers_core.require_does_raise here f
end
