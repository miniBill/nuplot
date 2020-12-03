precision highp float;

uniform vec2 u_zoomCenter;
uniform float u_zoomSize;
uniform float u_canvasWidth;
uniform float u_canvasHeight;

vec2 i(){
    return vec2(0,1);
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

vec2 cexp(vec2 v) {
    return vec2(cos(v.y) * exp(v.x), sin(v.y) * exp(v.x));
}

vec2 clog(vec2 b) {
    vec2 polar_b = to_polar(b);
    return vec2(log(polar_b.x), polar_b.y);
}

vec2 cpow(vec2 b, vec2 e) {
    return cexp(by(clog(b), e));
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

vec2 csinh(vec2 z) {
    return -(by(i(), csin(by(i(), z))));
}

vec2 ccosh(vec2 z) {
    return ccos(by(i(), z));
}

vec3 palette(float x, float y) {
    float deltaX = u_zoomSize / u_canvasWidth;
    float deltaY = u_zoomSize / u_canvasHeight;
    vec3 white = pixel(deltaX, deltaY, x, y);
    vec3 yax = (x * (x - deltaX)) < 0.0 ? vec3(1,0,0) : vec3(0,0,0);
    vec3 xax = (y * (y - deltaY)) < 0.0 ? vec3(0,1,0) : vec3(0,0,0);
    return max(white, max(yax, xax));
}
