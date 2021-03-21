#define MAX_DEPTH 30
//#define MAX_DEPTH 60

precision highp float;

uniform float u_whiteLines;
uniform float u_completelyReal;
uniform vec2 u_zoomCenter;
uniform float u_viewportWidth;
uniform float u_canvasWidth;
uniform float u_canvasHeight;
uniform float u_phi;
uniform float u_theta;

vec2 dup(float x) {
    return vec2(x, x);
}

vec4 gnum(float f) {
    return vec4(f, 0, 0, 0);
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

// Code adapted from code by Martin Stewart who adapted it from code by Andreas Köberle,
// who put it under the MIT license. So, for the adapted parts:
// 
// The MIT License (MIT)
// 
// Copyright (c) 2016 Andreas Köberle
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

const vec3 n = vec3(0.95047, 1.0, 1.08883);
const float t0 = 4.0 / 29.0;
const float t1 = 6.0 / 29.0;
const float t2 = 3.0 * pow(t1, 2.0);
const mat3 coeffs = mat3(
    3.2404542, -0.969266, 0.0556434, // first column
    -1.5371385, 1.8760108, -0.2040259,
    -0.4985314, 0.041556, 1.0572252
);

vec3 lessThanForMix(vec3 l, vec3 r) {
    return sign(r - l)/2.0 + vec3(0.5);
}

vec3 lab2xyz( vec3 t ) {
    return mix(
        t2 * (t - t0),
        pow(t, vec3(3.0)),
        lessThanForMix(vec3(t1), t)
    );
}

vec3 xyz2rgb(vec3 r) {
    return mix(
        1.055 * pow(r, vec3(1.0 / 2.4)) - 0.055,
        12.92 * r,
        lessThanForMix(r,vec3(0.00304))
    );
}

vec3 lab2rgb(float lightness, float labA, float labB ) {
    vec3 startY = vec3((lightness + 16.0) / 116.0);
    vec3 lab = startY + vec3(
        (labA / 500.0),
        0,
        -(labB / 200.0)
    );
    vec3 xyz = lab2xyz(lab) * n;
    vec3 rgb = xyz2rgb(coeffs * xyz);
    return clamp(rgb,0.0,1.0);
}

// luminance: [0, 100]?
// chroma: [0, 100]?
// hue: [0, 360], 30° is red
vec3 lch2rgb(float luminance, float chroma, float hue) {
    float hueInRadians = radians(hue);
    return lab2rgb( luminance, cos(hueInRadians) * chroma, sin(hueInRadians) * chroma );
}

// End of the adapted code

vec3 hl2rgb(float h, float l)
{
    return lch2rgb(l * 130.0, 80.0, h * 360.0 + 30.0);
    vec3 rgb = clamp(abs(mod(h*6.0+vec3(0.0,4.0,2.0),6.0)-3.0)-1.0,0.0,1.0);
    return l + (rgb - 0.5) * (1.0 - abs(2.0 * l - 1.0));
}
