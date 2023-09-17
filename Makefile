ELM_FILES = $(wildcard src/**/*.elm)
CODEGEN_FILES = $(wildcard codegen/src/*.elm) $(wildcard codegen/src/Glsl/*.elm) $(wildcard codegen/helpers/*.elm) $(wildcard codegen/helpers/*/*.elm)

.PHONY: build
build: generated/Glsl/Functions.elm src/optimized.js Makefile
	parcel build src/index.html --public-url .

src/optimized.js: $(ELM_FILES) Makefile
	yarn elm-optimize-level-2 src/elm/UI.elm --output $@

generated/Glsl/Functions.elm: codegen/functions.frag $(CODEGEN_FILES) codegen/bindings/Gen/Glsl.elm Makefile
	rm -rf generated
	yarn elm-codegen run --flags-from $< codegen/src/Generate.elm

codegen/bindings/Gen/Glsl.elm: $(CODEGEN_FILES) codegen/elm.codegen.json Makefile
	rm -rf codegen/bindings
	yarn elm-codegen install
	mkdir -p codegen/bindings
	mv codegen/Gen codegen/bindings/


.PHONY: clean
clean: Makefile
	rm -rf .cache dist elm-stuff
