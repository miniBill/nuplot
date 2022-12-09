.PHONY: all
all: generated/Glsl.elm

generated/Glsl.elm: codegen/functions.glsl codegen/Generate.elm codegen/helpers/Glsl/Helper.elm $(wildcard codegen/Glsl/*.elm) Makefile
	elm-codegen run --flags-from $<
