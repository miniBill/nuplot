ELM_FILES = $(wildcard src/**/*.elm)
CODEGEN_FILES = $(wildcard codegen/*.elm) $(wildcard codegen/Glsl/*.elm) $(wildcard codegen/helpers/*.elm) $(wildcard codegen/helpers/*/*.elm)

.PHONY: build
build: generated/Glsl/Functions.elm src/optimized.js Makefile
	parcel build src/index.html --public-url .

src/optimized.js: $(ELM_FILES) Makefile
	elm-optimize-level-2 src/elm/UI.elm --output $@

generated/Glsl/Functions.elm: codegen/functions.frag $(CODEGEN_FILES) codegen/Gen/Glsl.elm Makefile
	rm -rf generated
	elm-codegen run --flags-from $<

codegen/Gen/Glsl.elm: $(CODEGEN_FILES) codegen/elm.codegen.json Makefile
	elm-codegen install


.PHONY: clean
clean: Makefile
	rm -rf .cache dist elm-stuff
