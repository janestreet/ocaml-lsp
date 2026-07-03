BUILD_DIR ?= _build_oss
OCAML_COMPILER ?= ocaml-base-compiler.5.4.1
OXCAML_MERLIN_HASH ?= $(shell cat oxcaml_merlin_hash)
MERLIN_PIN ?= git+https://github.com/oxcaml/oxcaml.git\#$(OXCAML_MERLIN_HASH)
MERLIN_SUBPATH ?= external/merlin

export DUNE_BUILD_DIR := $(BUILD_DIR)

.PHONY: oss-deps buildoss runtestoss cleanoss deepcleanoss

cleanoss:
	rm -rf $(BUILD_DIR)

deepcleanoss: cleanoss
	rm -rf _opam

oss-deps:
	@if [ ! -d _opam ]; then \
		opam --cli=2.5 switch create . $(OCAML_COMPILER) --no-install --yes; \
	fi
	eval "$$(opam --cli=2.5 env)"; \
	opam --cli=2.5 pin add merlin-lib "$(MERLIN_PIN)" --subpath="$(MERLIN_SUBPATH)" --no-action --yes; \
	python3 -c 'from pathlib import Path; p = Path("_opam/.opam-switch/overlay/merlin-lib/opam"); p.write_text(p.read_text().replace("  [\"dune\" \"subst\"] {dev}\n", ""))'; \
	opam --cli=2.5 install merlin-lib --skip-updates --yes; \
	opam --cli=2.5 install ./ocaml-lsp.opam --deps-only --with-test --skip-updates --yes

buildoss: oss-deps
	eval "$$(opam --cli=2.5 env)"; \
	dune build --root .

runtestoss: oss-deps
	eval "$$(opam --cli=2.5 env)"; \
	dune runtest --display=short --root .
