CFLAGS=-Wall

.PHONY: all
all: out/image.ppm out/sorted.log out/spir-v.asm

out/tracer.cpp: head.h raw.frag foot.h
	mkdir -p `dirname $@`
	cat head.h > $@
	cat raw.frag | sed 's/uniform//g;s/(vec2/_(vec2/g;s/out vec3/vec3\&/g;s/ywz/ywz()/g;s/yzw/yzw()/g;s/(vec3/_(vec3/' | grep -v precision >> $@
	cat foot.h >> $@

out/sorted.log: out/debug.log
	cat $^ | sort | uniq > $@

out/image.ppm out/debug.log: out/tracer
	./out/tracer > $@

out/spir-v.asm out/frag.spv: out/forspir.frag
	glslangValidator -G --auto-map-locations $^ -o out/frag.spv --spirv-dis > out/spir-v.asm

out/forspir.frag: version310.glsl raw.frag
	mkdir -p `dirname $@`
	cat raw.frag | sed 's/gl_FragColor/fragmentColor/;s/precision highp float;//' | cat version310.glsl - > $@

.PHONY: clean
clean:
	rm -rf out
