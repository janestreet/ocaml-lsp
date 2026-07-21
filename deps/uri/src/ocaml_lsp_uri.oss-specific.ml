open! Core

module Feature_id = struct
  type t = string

  let of_string_opt s =
    match String.equal s "" with
    | true -> None
    | false -> Some s
  ;;

  let to_string t = t
end

type t = Lsp.Types.DocumentUri.t

(* TODO: devise a sensible OSS definition for "workspace". *)
let workspace_and_relative_path uri =
  let path = Lsp.Types.DocumentUri.drop_query uri |> Lsp.Types.DocumentUri.to_path in
  let relpath = String.chop_prefix_if_exists path ~prefix:"/" in
  let relpath = if String.is_empty relpath then "." else relpath in
  Some (File_path.Absolute.of_string "/", File_path.Relative.of_string relpath)
;;

let feature_id _ = None
