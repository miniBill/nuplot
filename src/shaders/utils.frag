precision highp float;

uniform vec2 u_zoomCenter;
uniform float u_viewportWidth;
uniform float u_canvasWidth;
uniform float u_canvasHeight;
uniform float u_whiteLines;
uniform float u_completelyReal;

vec2 i(){
    return vec2(0,1);
}

vec2 pi() {
    return vec2(radians(180.0), 0.0);
}

vec2 by(vec2 a, vec2 b) {
    return vec2(a.x*b.x-a.y*b.y, a.x*b.y+a.y*b.x);
}

vec2 conj(vec2 a) {
    return vec2(a.x,-a.y);
}

vec2 div(vec2 a, vec2 b) {
    return vec2(((a.x*b.x+a.y*b.y)/(b.x*b.x+b.y*b.y)),((a.y*b.x-a.x*b.y)/(b.x*b.x+b.y*b.y)));
}

vec2 to_polar(vec2 c) {
    return vec2(sqrt(c.x*c.x+c.y*c.y), atan(c.y, c.x));
}

vec2 from_polar(vec2 p) {
    return vec2(p.x * cos(p.y), p.x * sin(p.y));
}

vec2 cexp(vec2 v) {
    return vec2(cos(v.y) * exp(v.x), sin(v.y) * exp(v.x));
}

vec2 cabs(vec2 v) {
    return vec2(sqrt(v.x*v.x + v.y*v.y), 0.0);
}

vec2 carg(vec2 v) {
    return vec2(atan(v.y, v.x), 0);
}

vec2 cln(vec2 b) {
    vec2 polar_b = to_polar(b);
    return vec2(log(polar_b.x), polar_b.y);
}

vec2 cpow(vec2 b, vec2 e) {
    return cexp(by(cln(b), e));
}

float sinh(float x) {
    return 0.5 * (exp(x) - exp(-x));
}

float cosh(float x) {
    return 0.5 * (exp(x) + exp(-x));
}

vec2 csin(vec2 z) {
    return vec2(sin(z.x) * cosh(z.y), cos(z.x) * sinh(z.y));
}

vec2 ccos(vec2 z) {
    return vec2(cos(z.x) * cosh(z.y), sin(z.x) * sinh(z.y));
} 

vec2 ctan(vec2 z) {
    return div(csin(z),ccos(z));
}

vec2 catan2(vec2 y, vec2 x) {
    vec2 z = x + i() * y;
    return vec2(atan(z.y, z.x), 0.0);
}

vec2 csinh(vec2 z) {
    return -(by(i(), csin(by(i(), z))));
}

vec2 ccosh(vec2 z) {
    return ccos(by(i(), z));
}

vec2 csqrt(vec2 z) {
    vec2 polar_z = to_polar(z);
    vec2 polar_sqrt = vec2(sqrt(polar_z.x), polar_z.y * 0.5);
    return from_polar(polar_sqrt);
}

vec2 cpw(vec2 c, vec2 t, vec2 f) {
    return c.x > 0.0 ? t : f;
}

vec3 palette(float deltaX, float deltaY, float x, float y, vec3 pixel) {
    vec3 yax = (x * (x - deltaX)) < 0.0 ? vec3(0,1,0) : vec3(0,0,0);
    vec3 xax = (y * (y - deltaY)) < 0.0 ? vec3(1,0,0) : vec3(0,0,0);
    return max(pixel, max(yax, xax));
}
