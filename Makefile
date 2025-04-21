ELM_FILES = $(wildcard src/**/*.elm)
CODEGEN_FILES = $(wildcard codegen/*.elm)

.PHONY: build
build: generated/Glsl/Functions/NuPlot.elm src/optimized.js Makefile
	parcel build src/index.html --public-url .

src/optimized.js: $(ELM_FILES) Makefile
	yarn elm-optimize-level-2 src/elm/UI.elm --output $@

generated/Glsl/Functions/NuPlot.elm: codegen/functions.frag codegen/src/Generate.elm codegen/bindings/Gen/Glsl.elm Makefile
	rm -rf generated
	yarn elm-pages run codegen/src/Generate.elm
	elm-format --yes generated

codegen/bindings/Gen/Glsl.elm: codegen/elm.codegen.json Makefile
	rm -rf codegen/bindings
	yarn elm-codegen install
	mkdir -p codegen/bindings
	mv codegen/Gen codegen/bindings


.PHONY: clean
clean: Makefile
	rm -rf .cache dist elm-stuff
