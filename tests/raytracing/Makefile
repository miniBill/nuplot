# These flags allow us to declare every function and not define them and strip them
CFLAGS=-Wall -fdata-sections -ffunction-sections -Wl,--gc-section
CXXFLAGS=$(CFLAGS)

.PHONY: all
all: out/debug.log # out/spir-v.asm

out/tracer.cpp: head.h raw.frag foot.h
	mkdir -p `dirname $@`
	cat head.h > $@
	cat raw.frag | sed 's/uniform//g;s/(vec2/_(vec2/g;s/out vec3/vec3\&/g;s/ywz/ywz()/g;s/yzw/yzw()/g;s/(vec3/_(vec3/' | grep -v precision >> $@
	cat foot.h >> $@

out/image.ppm: out/tracer
	./out/tracer > $@

 out/debug.log: out/image.ppm

out/spir-v.asm out/frag.spv: out/forspir.frag
	glslangValidator -G --auto-map-locations $^ -o out/frag.spv --spirv-dis > out/spir-v.asm

out/forspir.frag: version310.glsl raw.frag
	mkdir -p `dirname $@`
	cat raw.frag | sed 's/gl_FragColor/fragmentColor/;s/precision highp float;//' | cat version310.glsl - > $@

.PHONY: clean
clean:
	rm -rf out
