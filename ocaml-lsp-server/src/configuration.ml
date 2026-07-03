open! Import
open Fiber.O

type t =
  { wheel : Lev_fiber.Timer.Wheel.t
  ; data : Config_data.t
  ; stage : Stage.t
  }

let wheel t = t.wheel

let default stage =
  let+ wheel =
    let delay =
      match Env_vars._TEST () with
      | None -> 0.25
      | Some _ -> 0.0
    in
    Lev_fiber.Timer.Wheel.create ~delay
  in
  let data = Config_data.default in
  { wheel; data; stage }
;;

let update t { DidChangeConfigurationParams.settings } =
  let* wheel =
    match
      match settings with
      | `Assoc xs ->
        (match List.Assoc.find ~equal:String.equal xs "diagnostics_delay" with
         | Some (`Float f) -> Some f
         | Some (`Int i) -> Some (float_of_int i)
         | None -> None
         | _ ->
           Jsonrpc.Response.Error.raise
             (Jsonrpc.Response.Error.make
                ~code:InvalidRequest
                ~message:"invalid value for diagnostics_delay"
                ()))
      | _ -> None
    with
    | None -> Fiber.return t.wheel
    | Some delay ->
      if Float.equal delay (Lev_fiber.Timer.Wheel.delay t.wheel)
      then Fiber.return t.wheel
      else
        let* () = Lev_fiber.Timer.Wheel.set_delay t.wheel ~delay in
        Fiber.return t.wheel
  in
  let data =
    let new_data = Config_data.t_of_yojson settings in
    let merge x y = Option.merge x y ~f:(fun _ y -> y) in
    let which_diagnostics =
      match settings with
      | `Assoc json ->
        (match List.Assoc.find ~equal:String.equal json "merlinDiagnostics" with
         | Some (`Assoc [ ("enable", `Bool b) ]) ->
           merge
             (Some
                { Config_data.WhichDiagnostics.merlin_syntax = b
                ; merlin_typing = b
                ; dune = false
                })
             new_data.which_diagnostics
         | None | Some _ -> new_data.which_diagnostics)
      | _ -> new_data.which_diagnostics
    in
    { Config_data.codelens = merge t.data.codelens new_data.codelens
    ; extended_hover = merge t.data.extended_hover new_data.extended_hover
    ; which_diagnostics = merge t.data.which_diagnostics which_diagnostics
    ; inlay_hints = merge t.data.inlay_hints new_data.inlay_hints
    ; syntax_documentation =
        merge t.data.syntax_documentation new_data.syntax_documentation
    ; shorten_merlin_diagnostics =
        merge t.data.shorten_merlin_diagnostics new_data.shorten_merlin_diagnostics
    ; ppx_css_colors = merge t.data.ppx_css_colors new_data.ppx_css_colors
    ; remote_lsp_fallback = merge t.data.remote_lsp_fallback new_data.remote_lsp_fallback
    ; dune_build_on_open = merge t.data.dune_build_on_open new_data.dune_build_on_open
    ; fuzzy_completion = merge t.data.fuzzy_completion new_data.fuzzy_completion
    ; filter_double_underscore =
        merge t.data.filter_double_underscore new_data.filter_double_underscore
    ; document_symbol = merge t.data.document_symbol new_data.document_symbol
    }
  in
  Fiber.return { t with wheel; data }
;;

let remote_lsp_fallback_enabled t =
  match t.data.remote_lsp_fallback with
  | Some { enable } -> enable
  | None -> false
;;

let which_diagnostics t =
  match t.data.which_diagnostics with
  | Some choices -> choices
  | None -> { merlin_syntax = false; merlin_typing = false; dune = false }
;;

let shorten_merlin_diagnostics t =
  match t.data.shorten_merlin_diagnostics with
  | Some { enable = true } | None -> true
  | Some { enable = false } -> false
;;

let dune_build_on_open t =
  match t.data.dune_build_on_open with
  | Some { enable } -> enable
  | None -> false
;;
