ELM_FILES = $(wildcard src/**/*.elm)
CODEGEN_FILES = $(wildcard codegen/*.elm)

.PHONY: build
build: generated/Glsl/Functions/NuPlot.elm src/optimized.js Makefile
	parcel build src/index.html --public-url .

src/optimized.js: $(ELM_FILES) Makefile
	yarn elm-optimize-level-2 src/elm/UI.elm --output $@

generated/Glsl/Functions/NuPlot.elm: codegen/functions.frag codegen/Generate.elm codegen/Gen/Glsl.elm Makefile
	rm -rf generated
	yarn elm-codegen run --flags-from $< codegen/Generate.elm
	elm-format --yes $@

codegen/Gen/Glsl.elm: codegen/Generate.elm codegen/elm.codegen.json Makefile
	rm -rf codegen/Gen
	yarn elm-codegen install


.PHONY: clean
clean: Makefile
	rm -rf .cache dist elm-stuff
