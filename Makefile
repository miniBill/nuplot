.PHONY: clean
clean:
	rm -rf .cache elm-stuff dist

.PHONY: run
run: clean
	parcel src/dev.html

.PHONY: forBenchmark
forBenchmark: clean
	elm-optimize-level-2 src/elm/UI.elm --output src/optimized.js
	parcel build --no-minify src/index.html --public-url .

.PHONY: build
build: clean
	elm-optimize-level-2 src/elm/UI.elm --output src/optimized.js
	parcel build src/index.html --public-url .
