val compute_colors
  :  Ocaml_parsing.Parsetree.structure
  -> Lsp.Types.ColorInformation.t list

val complete
  :  source:Merlin_kernel.Msource.t
  -> pos:Lsp.Types.Position.t
  -> Ocaml_parsing.Parsetree.structure
  -> Lsp.Types.CompletionItem.t list
