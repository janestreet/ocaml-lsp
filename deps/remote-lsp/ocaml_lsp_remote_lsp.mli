module Fiber = Ocaml_lsp_fiber
module Feature_id = Ocaml_lsp_uri.Feature_id

module Metadata_for_query : sig
  type t =
    { feature_id : Feature_id.t
    ; workspace : File_path.Absolute.t
    ; file_contents : string
    ; file_path : File_path.t
    }
end

val go_to_query
  :  log_info:Ocaml_lsp_logging.Log_info.t
  -> stage:Stage.t
  -> disable_remote_lsp:(unit -> unit)
  -> metadata:Metadata_for_query.t
  -> Lsp.Types.Position.t
  -> Lsp.Types.Go_to_target.t
  -> Lsp.Types.Location.t option Fiber.t

val hover_query
  :  log_info:Ocaml_lsp_logging.Log_info.t
  -> stage:Stage.t
  -> disable_remote_lsp:(unit -> unit)
  -> metadata:Metadata_for_query.t
  -> Lsp.Types.Position.t
  -> Lsp.Types.Hover.t option Fiber.t

val references_query
  :  log_info:Ocaml_lsp_logging.Log_info.t
  -> stage:Stage.t
  -> disable_remote_lsp:(unit -> unit)
  -> metadata:Metadata_for_query.t
  -> Lsp.Types.Position.t
  -> Lsp.Types.ReferenceContext.t
  -> Lsp.Types.Location.t list option Fiber.t

val call_compatible_query
  :  log_info:Ocaml_lsp_logging.Log_info.t
  -> stage:Stage.t
  -> disable_remote_lsp:(unit -> unit)
  -> metadata:Metadata_for_query.t
  -> params:Lsp_json_rpc_types.Jsonrpc.Structured.t option
  -> Lsp.Json.t option Fiber.t
