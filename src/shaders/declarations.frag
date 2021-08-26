#define MAX_DEPTH 30
//#define MAX_DEPTH 60

precision highp float;

uniform float u_whiteLines;
uniform float u_completelyReal;
uniform float u_drawAxes;
uniform vec2 u_zoomCenter;
uniform float u_viewportWidth;
uniform float u_canvasWidth;
uniform float u_canvasHeight;
uniform float u_phi;
uniform float u_theta;

vec3 hl2rgb(float h, float l)
{
    vec3 rgb = clamp(abs(mod(h*6.0+vec3(0.0,4.0,2.0),6.0)-3.0)-1.0,0.0,1.0);
    return l + (rgb - 0.5) * (1.0 - abs(2.0 * l - 1.0));
}

float atanPlus(float y, float x) {
    return mod(radians(360.0) + atan(y, x), radians(360.0));
}

vec2 iexpand(vec2 v) {
    return v * (vec2(1.0, 1.0) + sign(v) * vec2(-0.000001, 0.000001));
}

#define P31 2147483648
#define P32M1 4294967295

int left_shift(int i) {
    return i * 2;
}

int left_shift_increment(int i) {
    return i * 2 + 1;
}

ivec2 increment(ivec2 i) {
    return i.x == P32M1
        ? ivec2(0, i.y + 1)
        : ivec2(i.x + 1, i.y);
}

ivec2 left_shift(ivec2 i) {
    return i.x >= P31
        ? ivec2(2 * (i.x - P31), i.y * 2 + 1)
        : 2 * i;
}

ivec2 left_shift_increment(ivec2 i) {
    return increment(left_shift(i));
}

bool is_even(int i) {
    return i / 2 * 2 == i;
}

bool is_odd(int i) {
    return i / 2 * 2 != i;
}

bool is_even(ivec2 i) {
    return is_even(i.x);
}

ivec2 right_shift(ivec2 i) {
    return is_odd(i.y)
        ? ivec2(i.x / 2 + P31, i.y / 2)
        : i / 2;
}

int right_shift(int i) {
    return i / 2;
}
