open Import

type t =
  { wheel : Lev_fiber.Timer.Wheel.t
  ; data : Config_data.t
  ; stage : Stage.t
  }

val default : Stage.t -> t Fiber.t
val wheel : t -> Lev_fiber.Timer.Wheel.t
val update : t -> DidChangeConfigurationParams.t -> t Fiber.t
val remote_lsp_fallback_enabled : t -> bool
val which_diagnostics : t -> Config_data.WhichDiagnostics.t
val shorten_merlin_diagnostics : t -> bool
val dune_build_on_open : t -> bool
