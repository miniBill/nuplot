.PHONY: clean
clean:
	rm -rf .cache elm-stuff dist


.PHONY: build
build: clean
	parcel build src/index.html --public-url .
