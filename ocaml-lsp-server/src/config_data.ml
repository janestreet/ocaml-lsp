open Import

module InlayHints = struct
  type t =
    { hint_pattern_variables : bool [@default false] [@key "hintPatternVariables"]
    ; hint_let_bindings : bool [@default false] [@key "hintLetBindings"]
    ; hint_function_params : bool [@default false] [@key "hintFunctionParams"]
    ; hint_let_syntax_ppx : bool [@default false] [@key "hintLetSyntaxPpx"]
    }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Lens = struct
  type t = { enable : bool [@default false] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module ExtendedHover = struct
  type t = { enable : bool [@default true] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module SyntaxDocumentation = struct
  type t = { enable : bool [@default false] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module WhichDiagnostics = struct
  open Base (* to bring [Bool.equal] into scope *)

  (* Typing errors are not reported if there are syntax errors. If a user has dune errors
     and merlin errors turned on, they will *usually* see reasonable behavior, if merlin
     finds the same errors as the build and the lsp successfully de-duplicates. But both
     of those steps can sometimes fail. *)
  type t =
    { merlin_syntax : bool [@default false]
    ; merlin_typing : bool [@default false]
    ; dune : bool [@default false]
    }
  [@@deriving yojson, equal] [@@yojson.allow_extra_fields]
end

module ShortenMerlinDiagnostics = struct
  type t = { enable : bool [@default false] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module PpxCssColors = struct
  type t = { enable : bool [@default true] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

(** Whether we should fall back to remote-lsp when there is no local build running. This
    must be turned off on remote-lsp lsp servers!

    We therefore default this setting to false. We expect that editors will set it to true
    in their default configs. *)
module RemoteLspFallback = struct
  type t = { enable : bool [@default false] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module DuneBuildOnOpen = struct
  type t = { enable : bool [@default false] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module FuzzyCompletion = struct
  type t = { enable : bool [@default false] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module FilterDoubleUnderscore = struct
  type t = { enable : bool [@default true] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module DocumentSymbol = struct
  type t =
    { include_local_bindings : bool [@default false] [@key "includeLocalBindings"] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

(* NB: if you change one of the string names of the fields here, make sure to grep the
   codebase for occurences of the old name, as it may be used in yojson fragments, and the
   typechecker won't catch the mismatch between the old name and the new name. *)
type t =
  { codelens : Lens.t Json.Nullable_option.t [@default None] [@yojson_drop_default ( = )]
  ; extended_hover : ExtendedHover.t Json.Nullable_option.t
       [@key "extendedHover"] [@default None] [@yojson_drop_default ( = )]
  ; inlay_hints : InlayHints.t Json.Nullable_option.t
       [@key "inlayHints"] [@default None] [@yojson_drop_default ( = )]
  ; syntax_documentation : SyntaxDocumentation.t Json.Nullable_option.t
       [@key "syntaxDocumentation"] [@default None] [@yojson_drop_default ( = )]
  ; which_diagnostics : WhichDiagnostics.t Json.Nullable_option.t
       [@key "whichDiagnostics"] [@default None] [@yojson_drop_default ( = )]
  ; shorten_merlin_diagnostics : ShortenMerlinDiagnostics.t Json.Nullable_option.t
       [@key "shortenMerlinDiagnostics"] [@default None] [@yojson_drop_default ( = )]
  ; ppx_css_colors : PpxCssColors.t Json.Nullable_option.t
       [@key "ppxCssColors"] [@default None] [@yojson_drop_default ( = )]
  ; remote_lsp_fallback : RemoteLspFallback.t Json.Nullable_option.t
       [@key "remoteLspFallback"] [@default None] [@yojson_drop_default ( = )]
  ; dune_build_on_open : DuneBuildOnOpen.t Json.Nullable_option.t
       [@key "duneBuildOnOpen"] [@default None] [@yojson_drop_default ( = )]
  ; (* When [true] ocaml-lsp ignore text after the last '.' and let the LSP client to
       fuzzy match. This improves UX for VSCode users. *)
    fuzzy_completion : FuzzyCompletion.t Json.Nullable_option.t
       [@key "fuzzyCompletion"] [@default None] [@yojson_drop_default ( = )]
  ; filter_double_underscore : FilterDoubleUnderscore.t Json.Nullable_option.t
       [@key "filterDoubleUnderscore"] [@default None] [@yojson_drop_default ( = )]
  ; document_symbol : DocumentSymbol.t Json.Nullable_option.t
       [@key "documentSymbol"] [@default None] [@yojson_drop_default ( = )]
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

let default =
  { codelens = Some { enable = false }
  ; extended_hover = Some { enable = true }
  ; inlay_hints =
      Some
        { hint_pattern_variables = false
        ; hint_let_bindings = false
        ; hint_function_params = false
        ; hint_let_syntax_ppx = false
        }
  ; syntax_documentation = Some { enable = true }
  ; which_diagnostics =
      Some { merlin_syntax = false; merlin_typing = false; dune = false }
  ; shorten_merlin_diagnostics = Some { enable = false }
  ; ppx_css_colors = Some { enable = true }
  ; remote_lsp_fallback = Some { enable = false }
  ; dune_build_on_open = Some { enable = false }
  ; fuzzy_completion = Some { enable = false }
  ; filter_double_underscore = Some { enable = true }
  ; document_symbol = Some { include_local_bindings = false }
  }
;;
