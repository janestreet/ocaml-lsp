module Feature_id : sig
  type t

  val of_string_opt : string -> t option
  val to_string : t -> string
end

type t = Lsp.Types.DocumentUri.t

val workspace_and_relative_path
  :  t
  -> (File_path.Absolute.t * File_path.Relative.t) option

val feature_id : t -> Feature_id.t option
