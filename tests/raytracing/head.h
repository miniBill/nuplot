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

    friend vec2 operator * (double d, vec2 r) {
        return vec2(d * r.x, d * r.y);
    }
};

struct vec3 {
    double x;
    double y;
    double z;
    vec3(vec2 xy, double z) : x(xy.x), y(xy.y), z(z) {}
    vec3(double x = 0, double y = 0, double z = 0) : x(x), y(y), z(z) {}

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
    vec4(double x, double y, double z, double w) : x(x), y(y), z(z), w(w) {}
};

vec3 abs(vec3 x) {
    return vec3(fabs(x.x), fabs(x.y), fabs(x.z));
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

double atan(double y, double x) {
    return atan2(y, x);
}

template<class T>
T mix(T x, T y, double a) {
    return x * (1 - a) + y * a;
}

template<class T>
T normalize(T x) {
    return x / length(x);
}

struct FragCoord {
    vec2 xy;
    FragCoord() {}
    FragCoord(vec2 xy) : xy(xy) {}
};

FragCoord gl_FragCoord;
vec4 gl_FragColor;

FILE* debugFile;

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

notrace2(cln)
notrace2(iabs)
notrace2(ineg)
notrace22(ipow)
notrace2(iln)
notrace2(cexp)
notrace2(iexp)
notrace2(isquare)
notrace22(by)
notrace22(iby)
trace222(ipw)
trace22(ileq)
trace22(ilt)

vec2 interval_(vec3 l, vec3 r);
vec2 interval(vec3 l, vec3 r) {
    vec2 res = interval_(l, r);
    if(trace)
        fprintf(debugFile, "interval((%lf, %lf, %lf), (%lf, %lf, %lf) [%lf]) = [%lf, %lf]\n",
            l.x, l.y, l.z, r.x, r.y, r.z, length(r - l), res.x, res.y);
    return res;
}

vec3 bisect_(vec3 o, vec3 d, float max_distance);
vec3 bisect(vec3 l, vec3 r, float max_distance) {
    vec3 res = bisect_(l, r, max_distance);
    vec3 to = l + max_distance * r;
    if(trace)
        fprintf(debugFile, "bisect((%lf, %lf, %lf), (%lf, %lf, %lf)) = (%lf, %lf, %lf)\n\n",
            l.x, l.y, l.z, to.x, to.y, to.z, res.x, res.y, res.z);
    return res;
}

vec4 vec4_(vec3 l, double r) {
    return vec4(l, r);
}
