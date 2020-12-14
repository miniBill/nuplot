.PHONY: all
all: image.ppm

tracer.cpp: head.h raw.glsl foot.h Makefile
	cat head.h > $@
	cat raw.glsl | sed 's/uniform//;s/(vec2/_(vec2/' | grep -v precision >> $@
	cat foot.h >> $@

image.ppm: tracer
	./tracer > $@
