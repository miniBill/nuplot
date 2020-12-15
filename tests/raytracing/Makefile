.PHONY: all
all: image.ppm sorted.log

tracer.cpp debug.log: head.h raw.glsl foot.h Makefile
	cat head.h > tracer.cpp
	cat raw.glsl | sed 's/uniform//;s/(vec2/_(vec2/;s/(vec3/_(vec3/' | grep -v precision >> tracer.cpp
	cat foot.h >> tracer.cpp

sorted.log: debug.log
	cat $^ | sort | uniq > $@

image.ppm: tracer
	./tracer > $@
