#pragma once

#include <cstdio>
#include <cmath>

#define main() main_()
#define float double // Yes, really
#define abs fabs

struct vec2 {
    double x;
    double y;
    vec2() : x(0), y(0) {}
    vec2(double x, double y) : x(x), y(y) {}

    friend vec2 operator + (vec2 l, vec2 r) {
        return vec2(l.x + r.x, l.y + r.y);
    }

    friend vec2 operator - (vec2 l, vec2 r) {
        return vec2(l.x - r.x, l.y - r.y);
    }

    friend vec2 operator - (vec2 r) {
        return vec2(-r.x, -r.y);
    }

    friend vec2 operator * (double d, vec2 r) {
        return vec2(d * r.x, d * r.y);
    }

    friend vec2 operator / (vec2 l, double r) {
        return vec2(l.x / r, l.y / r);
    }
};

struct vec3 {
    double x;
    double y;
    double z;
    vec3(vec2 xy, double z) : x(xy.x), y(xy.y), z(z) {}
    vec3(double x) : x(x), y(0), z(0) {}
    vec3(double x, double y, double z) : x(x), y(y), z(z) {}

    friend vec3 operator + (double d, vec3 r) {
        return vec3(d + r.x, d + r.y, d + r.z);
    }

    friend vec3 operator + (vec3 l, vec3 r) {
        return vec3(l.x + r.x, l.y + r.y, l.z + r.z);
    }

    friend vec3 operator - (vec3 r, double d) {
        return vec3(r.x - d, r.y - d, r.z - d);
    }

    friend vec3 operator - (vec3 l, vec3 r) {
        return vec3(l.x - r.x, l.y - r.y, l.z - r.z);
    }

    friend vec3 operator - (vec3 l) {
        return vec3(-l.x, -l.y, -l.z);
    }

    friend vec3 operator * (vec3 r, double d) {
        return vec3(d * r.x, d * r.y, d * r.z);
    }

    friend vec3 operator * (double d, vec3 r) {
        return vec3(d * r.x, d * r.y, d * r.z);
    }

    friend vec3 operator / (vec3 r, double d) {
        return vec3(r.x / d, r.y / d, r.z / d);
    }
};

struct vec4 {
    double x;
    double y;
    double z;
    double w;
    vec4() : x(0), y(0), z(0), w(0) {}
    vec4(vec3 xyz, double w) : x(xyz.x), y(xyz.y), z(xyz.z), w(w) {}
    vec4(double x, vec3 yzw) : x(x), y(yzw.x), z(yzw.y), w(yzw.z) {}
    vec4(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}

    friend vec4 operator + (vec4 l, vec4 r) {
        return vec4(l.x + r.x, l.y + r.y, l.z + r.z, l.w + r.w);
    }

    friend vec4 operator - (vec4 r) {
        return vec4(-r.x, -r.y, -r.z, -r.w);
    }

    friend vec4 operator / (vec4 l, double r) {
        return vec4(l.x / r, l.y / r, l.z / r, l.w / r);
    }

    vec3 ywz() {
        return vec3(y, w, z);
    }

    vec3 yzw() {
        return vec3(y, z, w);
    }
};

struct FragCoord {
    vec2 xy;
    FragCoord() {}
    FragCoord(vec2 xy) : xy(xy) {}
};

FragCoord gl_FragCoord;
vec4 gl_FragColor;

FILE* debugFile;

vec3 abs(vec3 x) {
    return vec3(abs(x.x), abs(x.y), abs(x.z));
}

double mod(double x, double y) {
    return x - y * floor(x / y);
}

vec3 mod(vec3 l, double r) {
    return vec3(mod(l.x, r), mod(l.y, r), mod(l.z, r));
}

double clamp(double l, double mn, double mx) {
    return l < mn ? mn : l > mx ? mx : l;
}

vec3 clamp(vec3 l, double mn, double mx) {
    return vec3(
        clamp(l.x, mn, mx),
        clamp(l.y, mn, mx),
        clamp(l.z, mn, mx)
    );
}

double fract(double d) {
    return d >= 0 ? d - floor(d) : floor(d) - d; // probably wrong for negative numbers
}

double min(double l, double r) {
    return l <= r ? l : r;
}

double max(double l, double r) {
    return l >= r ? l : r;
}

double length(vec2 l) {
    return sqrt(l.x * l.x + l.y * l.y);
}

double length(vec3 l) {
    return sqrt(l.x * l.x + l.y * l.y + l.z * l.z);
}

double length(vec4 l) {
    return sqrt(l.x * l.x + l.y * l.y + l.z * l.z + l.w * l.w);
}

double atan(double y, double x) {
    return atan2(y, x);
}

double radians(double deg) {
    return M_PI * deg / 180.0;
}

template<class T>
T mix(T x, T y, double a) {
    return x * (1 - a) + y * a;
}

vec2 normalize_(vec2 x) {
    double l = length(x);
    if(l == 0) return x;
    return x / l;
}

vec3 normalize_(vec3 x) {
    double l = length(x);
    if(l == 0) return x;
    return x / l;
}

vec4 normalize_(vec4 x) {
    double l = length(x);
    if(l == 0) return x;
    return x / l;
}

vec3 cross_(vec3 a, vec3 b) {
    return vec3(
        a.y * b.z - a.z * b.y,
        a.z * b.x - a.x * b.z,
        a.x * b.y - a.y * b.x
    );
}

double dot(vec2 l, vec2 r) {
    return l.x * r.x + l.y * r.y;
}

double dot(vec3 l, vec3 r) {
    return l.x * r.x + l.y * r.y + l.z * r.z;
}

vec2 sin_(vec2 input) {
    return vec2(sin(input.x), sin(input.y));
}

vec2 ceil(vec2 input) {
    return vec2(ceil(input.x), ceil(input.y));
}

#define trace2(f) \
            vec2 f ## _(vec2 z); \
            vec2 f (vec2 z) { \
                vec2 res = f ## _(z); \
                if(trace) fprintf(debugFile, "  " #f "(%lf, %lf) = [%lf, %lf]\n", z.x, z.y, res.x, res.y); \
                return res; \
            }

#define notrace2(f) \
            vec2 f ## _(vec2 z); \
            vec2 f (vec2 z) { \
                return f ## _(z); \
            }

#define trace3(f) \
            vec3 f ## _(vec3 z); \
            vec3 f (vec3 z) { \
                vec3 res = f ## _(z); \
                if(trace) fprintf(debugFile, "  " #f "(%lf, %lf, %lf) = (%lf, %lf, %lf) [%lf]\n", z.x, z.y, z.z, res.x, res.y, res.z, length(res)); \
                return res; \
            }

#define notrace3(f) \
            vec3 f ## _(vec3 z); \
            vec3 f (vec3 z) { \
                return f ## _(z); \
            }

#define trace22(f) \
            vec2 f ## _(vec2 l, vec2 r); \
            vec2 f (vec2 l, vec2 r) { \
                vec2 res = f ## _(l, r); \
                if(trace) fprintf(debugFile, "  " #f "((%lf, %lf), (%lf, %lf)) = [%lf, %lf]\n", l.x, l.y, r.x, r.y, res.x, res.y); \
                return res; \
            }

#define notrace22(f) \
            vec2 f ## _(vec2 l, vec2 r); \
            vec2 f (vec2 l, vec2 r) { \
                return f ## _(l, r); \
            }

#define notrace33(f) \
            vec3 f ## _(vec3 l, vec3 r); \
            vec3 f (vec3 l, vec3 r) { \
                return f ## _(l, r); \
            }

#define trace33_2(f) \
            vec2 f ## _(vec3 l, vec3 r); \
            vec2 f (vec3 l, vec3 r) { \
                vec2 res = f ## _(l, r); \
                if(trace) fprintf(debugFile, "  " #f "((%lf, %lf, %lf), (%lf, %lf, %lf)) = [%lf, %lf]\n", l.x, l.y, l.z, r.x, r.y, r.z, res.x, res.y); \
                return res; \
            }

#define notrace33_2(f) \
            vec2 f ## _(vec3 l, vec3 r); \
            vec2 f (vec3 l, vec3 r) { \
                return f ## _(l, r); \
            }

#define trace222(f) \
            vec2 f ## _(vec2 l, vec2 m, vec2 r); \
            vec2 f (vec2 l, vec2 m, vec2 r) { \
                vec2 res = f ## _(l, m, r); \
                if(trace) fprintf(debugFile, "  " #f "((%lf, %lf), (%lf, %lf), (%lf, %lf)) = [%lf, %lf]\n", \
                    l.x, l.y, m.x, m.y, r.x, r.y, res.x, res.y); \
                return res; \
            }

#define notrace222(f) \
            vec2 f ## _(vec2 l, vec2 m, vec2 r); \
            vec2 f (vec2 l, vec2 m, vec2 r) { \
                return f ## _(l, m, r); \
            }

bool trace;

notrace2(cexp)
notrace2(cln)
notrace2(iabs)
notrace2(iceiling)
notrace2(icos)
notrace2(icosh)
notrace2(iexp)
notrace2(iinverse)
notrace2(iln)
notrace2(ineg)
notrace2(isin)
notrace2(isqrt)
notrace2(isquare)
notrace2(sin)
notrace22(by)
notrace22(iby)
notrace22(idiv)
notrace22(ieq)
notrace22(igt)
notrace22(ileq)
notrace22(ilt)
notrace22(ipow)
notrace22(merge)
notrace222(ipw)
notrace3(normal_0)
notrace3(normal_1)
notrace3(normal)
notrace3(normalize)
notrace33(cross)
trace33_2(interval_0)
trace33_2(interval_1)
trace33_2(interval)

bool bisect_(vec3 o, vec3 d, float max_distance, vec3& found, vec3& n);
bool bisect(vec3 l, vec3 r, float max_distance, vec3& found, vec3& n) {
    bool res = bisect_(l, r, max_distance, found, n);
    vec3 to = l + max_distance * r;
    if(trace)
        fprintf(debugFile, "bisect((%lf, %lf, %lf), (%lf, %lf, %lf)) = %d, found = (%lf, %lf, %lf), normal = (%lf, %lf, %lf) [found - eye = %lf]\n\n",
            l.x, l.y, l.z, to.x, to.y, to.z, res ? 1 : 0,
            found.x, found.y, found.z, n.x, n.y, n.z, length(found - l));
    return res;
}

bool bisect_0_(vec3 o, vec3 d, float max_distance, vec3& found, vec3& n);
bool bisect_0(vec3 l, vec3 r, float max_distance, vec3& found, vec3& n) {
    bool res = bisect_0_(l, r, max_distance, found, n);
    vec3 to = l + max_distance * r;
    if(trace)
        fprintf(debugFile, "bisect_0((%lf, %lf, %lf), (%lf, %lf, %lf)) = %d, found = (%lf, %lf, %lf), normal = (%lf, %lf, %lf) [%lf]\n\n",
            l.x, l.y, l.z, to.x, to.y, to.z, res ? 1 : 0,
            found.x, found.y, found.z, n.x, n.y, n.z, length(found - l));
    return res;
}

bool bisect_1_(vec3 o, vec3 d, float max_distance, vec3& found, vec3& n);
bool bisect_1(vec3 l, vec3 r, float max_distance, vec3& found, vec3& n) {
    bool res = bisect_1_(l, r, max_distance, found, n);
    vec3 to = l + max_distance * r;
    if(trace)
        fprintf(debugFile, "bisect_1((%lf, %lf, %lf), (%lf, %lf, %lf)) = %d, found = (%lf, %lf, %lf), normal = (%lf, %lf, %lf) [%lf]\n\n",
            l.x, l.y, l.z, to.x, to.y, to.z, res ? 1 : 0,
            found.x, found.y, found.z, n.x, n.y, n.z, length(found - l));
    return res;
}

vec4 vec4_(vec3 l, double r) {
    return vec4(l, r);
}

vec4 raytrace_(vec3 o, vec3 d, float max_distance);
vec4 raytrace(vec3 o, vec3 d, float max_distance) {
    return raytrace_(o, d, max_distance);
}
