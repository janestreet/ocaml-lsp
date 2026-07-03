(lang dune 3.11)
(name ocaml-lsp)

(package
 (name ocaml-lsp)
 (synopsis "OCaml Language Server Protocol implementation")
 (allow_empty)
 (depends
  (ocaml (= 5.4))
  (dune (>= 3.11))
  async
  base
  cmarkit
  (cppo :build)
  core
  core_unix
  csexp
  dune-private-libs
  (expect_test_helpers_async :with-test)
  (expect_test_helpers_core :with-test)
  file_path
  lrgrep
  merlin-lib
  odoc-parser
  ppx_bin_prot
  ppx_compare
  ppx_enumerate
  (ppx_expect :with-test)
  (ppx_jane :with-test)
  ppx_js_style
  ppx_let
  ppx_log
  ppx_sexp_conv
  ppx_sexp_message
  ppx_sexp_value
  ppx_shorthand
  ppx_string
  ppx_yojson_conv
  re
  re2
  spawn
  uutf
  yojson))
