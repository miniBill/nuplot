precision highp float;

uniform float u_whiteLines;
uniform float u_completelyReal;
uniform vec2 u_zoomCenter;
uniform float u_viewportWidth;
uniform float u_canvasWidth;
uniform float u_canvasHeight;

#define MAX_DEPTH 31
//#define MAX_DEPTH 50

vec3 hl2rgb(float h, float l)
{
    vec3 rgb = clamp(abs(mod(h*6.0+vec3(0.0,4.0,2.0),6.0)-3.0)-1.0,0.0,1.0);
    return l + (rgb - 0.5) * (1.0 - abs(2.0 * l - 1.0));
}

vec2 dup(float x) {
    return vec2(x, x);
}

vec4 gnum(float f) {
    return vec4(f, 0, 0, 0);
}

int int64_shl(int i) {
    return i * 2;
}

int int64_shl_plus_one(int i) {
    return i * 2 + 1;
}

int int64_shr(int i) {
    return i / 2;
}

bool int64_is_even(int i) {
    return i / 2 * 2 == i;
}
/*
ivec2 int64_shl(ivec2 i) {
    i *= 2;
    if(i.x >= 1073741824) {
        i.x -= 1073741824;
        i.y++;
    }
    return i;
}

ivec2 int64_shl_plus_one(ivec2 i) {
    i *= 2;
    i.x++;
    if(i.x >= 1073741824) {
        i.x -= 1073741824;
        i.y++;
    }
    return i;
}

ivec2 int64_shr(ivec2 i) {
    if(i.y / 2 * 2 + 1 == i.y) {
        i.x += 1073741824;
    }
    return i / 2;
}

bool int64_is_even(ivec2 i) {
    return i.x / 2 * 2 == i.x;
}
 */