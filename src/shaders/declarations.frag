#define MAX_DEPTH 30

precision highp float;

uniform float u_whiteLines;
uniform float u_completelyReal;
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

vec2 dup(float x) {
    return vec2(x, x);
}

vec4 gnum(float f) {
    return vec4(f, 0, 0, 0);
}

float atanPlus(float y, float x) {
    return mod(radians(360.0) + atan(y, x), radians(360.0));
}
