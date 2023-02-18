.PHONY: all
all: generated/Glsl.elm

generated/Glsl.elm: codegen/functions.frag codegen/Generate.elm codegen/Gen/Glsl/Helper.elm $(wildcard codegen/Glsl/*.elm) Makefile
	elm-codegen run --flags-from $<

codegen/Gen/Glsl/Helper.elm: codegen/helpers/Glsl/Helper.elm codegen/elm.codegen.json
	elm-codegen install
