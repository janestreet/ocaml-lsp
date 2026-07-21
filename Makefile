BUILD_DIR ?= _build_oss
OCAML_COMPILER ?= ocaml-base-compiler.5.4.1
OXCAML_MERLIN_HASH ?= $(shell cat oxcaml_merlin_hash)
OXCAML_REPO ?= https://github.com/oxcaml/oxcaml.git
MERLIN_PIN ?= git+$(OXCAML_REPO)\#$(OXCAML_MERLIN_HASH)
MERLIN_SUBPATH ?= external/merlin
# Everything OxCaml-compiler-related lives under $(OXCAML_DIR): the compiler is cloned
# and built in $(OXCAML_DIR)/oxcaml (removed once installed), installed with
# $(OXCAML_DIR)/_install as its --prefix, and stamped with $(OXCAML_DIR)/HASH.
OXCAML_DIR ?= _oxcaml
# Optionally set this to the installation prefix of a pre-built OxCaml compiler (a
# directory containing lib/ocaml). When set, [ox-stdlib] builds nothing and the tests use
# that installation's stdlib instead. It must match the revision merlin-lib is pinned to
# (see [oxcaml_merlin_hash]), or merlin will reject its cmis.
OXCAML_INSTALL ?=
AUTOCONF ?= autoconf27 # Note: this has to be autoconf >= 2.71

ifeq ($(OXCAML_INSTALL),)
OX_STDLIB = $(abspath $(OXCAML_DIR))/_install/lib/ocaml
else
OX_STDLIB = $(OXCAML_INSTALL)/lib/ocaml
endif

export DUNE_BUILD_DIR := $(BUILD_DIR)

.PHONY: oss-deps buildoss runtestoss ox-stdlib cleanoss deepcleanoss

cleanoss:
	rm -rf $(BUILD_DIR)

deepcleanoss: cleanoss
	rm -rf _opam $(OXCAML_DIR)

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

# The merlin-lib we pin comes from the oxcaml repo, so it can only read cmi files
# produced by the `oxcaml` compiler; it rejects the `.cmi`s produced by the `ocaml`
# compiler. Without an `oxcaml`-built stdlib, no identifier from the standard library
# resolves in the tests.
ox-stdlib: oss-deps
	@if [ -n "$(OXCAML_INSTALL)" ]; then \
		echo "Using pre-built OxCaml install at $(OXCAML_INSTALL)"; \
	elif [ -f $(OXCAML_DIR)/HASH ] \
	  && [ "$$(cat $(OXCAML_DIR)/HASH)" = "$(OXCAML_MERLIN_HASH)" ]; then \
		echo "OxCaml stdlib for $(OXCAML_MERLIN_HASH) already present in $(OXCAML_DIR)"; \
	else \
		set -e; \
		autoconf_version=$$($(AUTOCONF) --version | sed -n '1s/.* \([0-9][0-9.]*\)$$/\1/p'); \
		if [ "$$(printf '%s\n' 2.71 "$$autoconf_version" | sort -V | head -n 1)" != "2.71" ]; then \
			echo "error: autoconf >= 2.71 is required to build the OxCaml stdlib" \
			  "(found $$autoconf_version). On JS dev boxes, try: make ox-stdlib AUTOCONF=autoconf27"; \
			exit 1; \
		fi; \
		rm -rf $(OXCAML_DIR); \
		mkdir -p $(OXCAML_DIR)/oxcaml; \
		git -C $(OXCAML_DIR)/oxcaml init -q; \
		git -C $(OXCAML_DIR)/oxcaml remote add origin $(OXCAML_REPO); \
		git -C $(OXCAML_DIR)/oxcaml fetch --depth 1 origin $(OXCAML_MERLIN_HASH); \
		git -C $(OXCAML_DIR)/oxcaml checkout -q FETCH_HEAD; \
		eval "$$(opam --cli=2.5 env)"; \
		cd $(OXCAML_DIR)/oxcaml; \
		$(AUTOCONF); \
		./configure --enable-dev --prefix="$(abspath $(OXCAML_DIR))/_install"; \
		env -u DUNE_BUILD_DIR $(MAKE) install; \
		cd "$(abspath .)"; \
		echo "$(OXCAML_MERLIN_HASH)" > $(OXCAML_DIR)/HASH; \
		rm -rf $(OXCAML_DIR)/oxcaml; \
	fi

runtestoss: oss-deps ox-stdlib
	eval "$$(opam --cli=2.5 env)"; \
	OCAMLLSP_STDLIB="$(OX_STDLIB)" \
	dune runtest --display=short --root .
