#define MAX_DEPTH 30
//#define MAX_DEPTH 60

#define P31 2147483648
#define P32M1 4294967295
#define PIHALF 1.5707963267948966
#define PI 3.141592653589793
#define TWOPI 6.283185307179586

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

///////////
// Utils //
///////////

float log10(float x) {
  return log(x) / log(10.0);
}

float sinh(float x) { return 0.5 * (exp(x) - exp(-x)); }
float cosh(float x) { return 0.5 * (exp(x) + exp(-x)); }
float tanh(float x) {
  if (abs(x) > 10.)
    return sign(x);
  float p = exp(x);
  float m = exp(-x);
  return (p - m) / (p + m);
}

float round(float v) { return floor(v + 0.5); }
vec2 round(vec2 v) { return floor(v + 0.5); }
vec3 round(vec3 v) { return floor(v + 0.5); }
vec4 round(vec4 v) { return floor(v + 0.5); }

float cbrt(float v) { return sign(v) * pow(abs(v), 1. / 3.); }

bool between(float x, float low, float high) { return low <= x && x <= high; }

vec2 dup(float x) { return vec2(x, x); }

vec3 hl2rgb(float h, float l) {
  vec3 rgb = clamp(abs(mod(h * 6. + vec3(0., 4., 2.), 6.) - 3.) - 1., 0., 1.);
  return l + (rgb - .5) * (1. - abs(2. * l - 1.));
}

vec4 gnum(float f) { return vec4(f, 0, 0, 0); }

float atanPlus(float y, float x) {
  return mod(radians(360.) + atan(y, x), radians(360.));
}

vec2 iexpand(vec2 v) {
  return v * (vec2(1., 1.) + sign(v) * vec2(-0.000001, 0.000001));
}

float thetaDelta(float theta) {
  if (u_whiteLines < 1.)
    return 100.;
  float thetaSix = theta * u_whiteLines + .5;
  float thetaNeigh = .05;
  return abs(fract(thetaSix) - .5) / thetaNeigh;
}

float axis(float coord, float otherCoord, float maxDelta) {
  float across = 1.0 - abs(coord/maxDelta);
  if(across < -12.0)
    return 0.0;
  float smallUnit = pow(10.0, ceil(log10(maxDelta)));
  if(across < 0.0 && abs(otherCoord) < maxDelta * 2.0)
    return 0.0;
  float unit = across < -6.0 ? smallUnit * 100.0 : across < -0.1 ? smallUnit * 10.0 : smallUnit * 5.0;
  float parallel = mod(abs(otherCoord), unit) < maxDelta ? 1.0 : 0.0;
  return max(0.0, max(across, parallel));
}

// Shifts for the recursive algorithm
int left_shift(int i) { return i * 2; }
int left_shift_increment(int i) { return i * 2 + 1; }

ivec2 increment(ivec2 i) {
  return i.x == P32M1 ? ivec2(0, i.y + 1) : ivec2(i.x + 1, i.y);
}

ivec2 left_shift(ivec2 i) {
  return i.x >= P31 ? ivec2(2 * (i.x - P31), i.y * 2 + 1) : 2 * i;
}

ivec2 left_shift_increment(ivec2 i) { return increment(left_shift(i)); }

bool is_even(int i) { return i / 2 * 2 == i; }
bool is_odd(int i) { return i / 2 * 2 != i; }
bool is_even(ivec2 i) { return is_even(i.x); }

ivec2 right_shift(ivec2 i) {
  return is_odd(i.y) ? ivec2(i.x / 2 + P31, i.y / 2) : i / 2;
}

int right_shift(int i) { return i / 2; }

/////////////
// Complex //
/////////////

// Constants
vec2 ci() { return vec2(0, 1); }

vec2 cpi() { return vec2(radians(180.), 0); }

vec2 ce() { return vec2(exp(1.), 0); }

// Operations
vec2 cby(vec2 a, vec2 b) {
  return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

vec2 cdiv(vec2 a, vec2 b) {
  float k = 1. / dot(b, b);
  float r = k * dot(a, b);
  float i = k * (a.y * b.x - a.x * b.y);
  return vec2(r, i);
}

vec2 cln(vec2 z) {
  if (z.y == 0. && z.x >= 0.) {
    return vec2(log(z.x), 0);
  }
  float px = length(z);
  float py = atan(z.y, z.x);
  return vec2(log(px), py);
}

vec2 cexp(vec2 z) {
  if (z.y == 0.) {
    return vec2(exp(z.x), 0);
  }
  return vec2(cos(z.y), sin(z.y)) * exp(z.x);
}

vec2 cpow(vec2 w, vec2 z) {
  if (w.x >= 0. && w.y == 0. && z.y == 0.) {
    return vec2(pow(w.x, z.x), 0);
  }
  return cexp(cby(cln(w), z));
}

// Trig
vec2 csin(vec2 z) {
  if (z.y == 0.0) {
    return vec2(sin(z.x), 0);
  }
  return vec2(sin(z.x) * cosh(z.y), cos(z.x) * sinh(z.y));
}

vec2 ccos(vec2 z) {
  if (z.y == 0.0) {
    return vec2(cos(z.x), 0);
  }
  return vec2(cos(z.x) * cosh(z.y), sin(z.x) * sinh(z.y));
}

vec2 ctan(vec2 z) {
  if (z.y == 0.0) {
    return vec2(tan(z.x), 0);
  }
  return cdiv(csin(z), ccos(z));
}

vec2 csqrt(vec2 z) {
  if (z.y == 0. && z.x >= 0.) {
    return vec2(sqrt(z.x), 0);
  }
  float r = pow(dot(z, z), .25);
  float t = atan(z.y, z.x) * .5;
  return r * vec2(cos(t), sin(t));
}

vec2 casin(vec2 z) {
  vec2 s = csqrt(vec2(1, 0) - cby(z, z));
  vec2 arg = s - cby(vec2(0, 1), z);
  return cby(vec2(0, 1), cln(arg));
}

vec2 cacos(vec2 z) { return cpi() * .5 - casin(z); }

vec2 catan(vec2 z) {
  if (z.y == 0.) {
    return vec2(atan(z.x), 0);
  }
  vec2 o = vec2(1, 0);
  vec2 iz = cby(vec2(0, 1), z);
  vec2 l = cdiv(o + iz, o - iz);
  return -0.5 * cby(vec2(0, 1), cln(l));
}

vec2 catan2(vec2 y, vec2 x) {
  vec2 z = vec2(x.x - y.y, x.y + y.x);
  return vec2(atan(z.y, z.x), 0.);
}

vec2 csinh(vec2 z) { return .5 * (cexp(z) - cexp(-z)); }
vec2 ccosh(vec2 z) { return .5 * (cexp(z) + cexp(-z)); }

vec2 ctanh(vec2 z) {
  vec2 p = cexp(z);
  vec2 m = cexp(-z);
  return cdiv(p - m, p + m);
}

// Power and sign
vec2 cabs(vec2 z) { return vec2(length(z), 0.); }

vec2 csign(vec2 z) { return vec2(sign(z.x), sign(z.y)); }

vec2 ccbrt(vec2 z) {
  if (z.y == 0.) {
    return vec2(sign(z.x) * pow(z.x, 1. / 3.), 0);
  }
  float r = pow(dot(z, z), 1. / 6.);
  float t = atan(z.y, z.x) / 3. + (z.x > 0. ? 0. : radians(120.));
  return r * vec2(cos(t), sin(t));
}

vec2 csquare(vec2 z) { return cby(z, z); }

vec2 clog10(vec2 z) { return cln(z) / log(10.); }

vec2 cre(vec2 z) { return vec2(z.x, 0); }
vec2 cim(vec2 z) { return vec2(z.y, 0); }

float arg(vec2 v) { return atan(v.y, v.x); }
vec2 carg(vec2 v) { return vec2(atan(v.y, v.x), 0); }

vec2 cpw(vec2 c, vec2 t, vec2 f) { return c.x > 0. ? t : f; }

// Rounding
vec2 cceil(vec2 z) { return ceil(z); }
vec2 cfloor(vec2 z) { return floor(z); }
vec2 cround(vec2 z) { return floor(z + vec2(0.5, 0.5)); }

// Comparison
vec2 cmin(vec2 l, vec2 r) { return l.x < r.x ? l : r; }
vec2 cmax(vec2 l, vec2 r) { return l.x > r.x ? l : r; }

// Other
vec2 cmod(vec2 l, vec2 r) { return vec2(mod(l.x, r.x), 0); }

vec2 cmbrot(vec2 x, vec2 y) {
  vec2 c = x + vec2(-y.y, y.x);
  float p = length(c - vec2(.25, 0));
  if (c.x <= p - 2.0 * p * p + .25 || length(c + vec2(1, 0)) <= .25)
    return vec2(0, 0);
  vec2 z = c;
  for (int i = 0; i < 4000; i++) {
    z = vec2(z.x * z.x - z.y * z.y, 2. * z.x * z.y) + c;
    if (length(z) > 1000000.) {
      float logLength = log(length(z));
      float nu = log(logLength / log(2.)) / log(2.);
      float fi = float(i) - nu;
      return vec2(sin(fi), cos(fi));
    }
  }
  return vec2(0, 0);
}

//////////////
// Interval //
//////////////

// Operations
vec2 ineg(vec2 v) { return vec2(-v.y, -v.x); }

vec2 iby(vec2 l, vec2 r) {
  float a = l.x * r.x;
  float b = l.x * r.y;
  float c = l.y * r.x;
  float d = l.y * r.y;
  float mn = min(min(a, b), min(c, d));
  float mx = max(max(a, b), max(c, d));
  return vec2(mn, mx);
}

vec2 iinverse(vec2 y) {
  if (y.x <= 0. && y.y >= 0.)
    return vec2(-1. / 0., 1. / 0.);
  if (y.y == 0.)
    return vec2(-1. / 0., 1. / y.x);
  if (y.x == 0.)
    return vec2(1. / y.y, 1. / 0.);
  return vec2(1. / y.y, 1. / y.x);
}

vec2 idiv(vec2 l, vec2 r) { return iby(l, iinverse(r)); }

float ipow(float b, int e) {
  float fe = float(e);
  if (mod(fe, 2.) == 0.)
    return pow(abs(b), fe);
  return b * pow(abs(b), fe - 1.);
}

vec2 ipow(vec2 b, int e) {
  if (e == 0)
    return vec2(1, 1);
  if (e == 1)
    return b;
  float xe = ipow(b.x, e);
  float ye = ipow(b.y, e);
  float mn = min(xe, ye);
  float mx = max(xe, ye);
  if (mod(float(e), 2.) == 0. && b.x <= 0. && b.y >= 0.) {
    return vec2(min(0., mn), max(0., mx));
  }
  return vec2(mn, mx);
}

vec2 iln(vec2 z) { return log(z); }
vec2 iexp(vec2 z) { return exp(z); }

vec2 ipow(vec2 b, float e) {
  if (abs(e - round(e)) < .000001)
    return ipow(b, int(e));
  return iexp(e * iln(b));
}

vec2 ipow(vec2 b, vec2 e) {
  if (e.y - e.x < .000001 && abs(e.x - round(e.x)) < .000001)
    return ipow(b, int(e.x));
  return iexp(iby(iln(b), e));
}

// Relations
vec2 ilt(vec2 l, vec2 r) { return vec2(r.x - l.y, r.y - l.x); }
vec2 ileq(vec2 l, vec2 r) { return vec2(r.x - l.y, r.y - l.x); }
vec2 ineq(vec2 l, vec2 r) { return vec2(r.x - l.y, r.y - l.x); }
vec2 ieq(vec2 l, vec2 r) { return vec2(l.x - r.y, l.y - r.x); }
vec2 igeq(vec2 l, vec2 r) { return vec2(l.x - r.y, l.y - r.x); }
vec2 igt(vec2 l, vec2 r) { return vec2(l.x - r.y, l.y - r.x); }

vec2 iabs(vec2 z) {
  if (z.x <= 0. && z.y >= 0.)
    return vec2(0, max(z.y, abs(z.x)));
  if (z.x <= 0.)
    return vec2(-z.y, -z.x);
  return z;
}

vec2 isin(vec2 v) {
  if (v.y - v.x > radians(360.)) {
    return vec2(-1., 1.);
  }
  float from = mod(v.x, radians(360.)); // [0, 360°]
  float to = from + v.y - v.x;          // [0, 720°]
  vec2 s = sin(vec2(from, to));
  vec2 res = vec2(min(s.x, s.y), max(s.x, s.y));
  if (between(radians(90.), from, to) || between(radians(90. + 360.), from, to))
    res.y = 1.;
  if (between(radians(270.), from, to) ||
      between(radians(270. + 360.), from, to))
    res.x = -1.;
  return res;
}

vec2 icos(vec2 v) {
  vec2 shift = radians(90.) * vec2(1, 1);
  return isin(v + shift);
}

vec2 itan(vec2 z) { return idiv(isin(z), icos(z)); }

vec2 isinh(vec2 v) { return vec2(sinh(v.x), sinh(v.y)); }

vec2 icosh(vec2 z) {
  if (z.x <= 0. && z.y >= 0.)
    return vec2(cosh(0.), max(z.y, abs(z.x)));
  if (z.x <= 0.)
    return vec2(cosh(-z.y), cosh(-z.x));
  return vec2(cosh(z.x), cosh(z.y));
}

vec2 itanh(vec2 z) { return vec2(tanh(z.x), tanh(z.y)); }

vec2 iasin(vec2 z) {
  // Don't use clamp so if z is fully outside range the result is empty
  vec2 clamped = vec2(max(z.x, -1.), min(z.y, 1.));
  return asin(clamped);
}

vec2 iacos(vec2 z) {
  // Don't use clamp so if z is fully outside range the result is empty
  vec2 clamped = vec2(max(z.x, -1.), min(z.y, 1.));
  return acos(clamped).yx;
}

vec2 iatan(vec2 z) { return atan(z); }

vec2 iatan2(vec2 y, vec2 x) {
  // TODO: improve this
  vec2 r = vec2(atan(y.x, x.x), atan(y.y, x.x));
  vec2 q = vec2(atan(y.x, x.y), atan(y.y, x.y));
  vec2 rs = vec2(min(r.x, r.y), max(r.x, r.y));
  vec2 qs = vec2(min(q.x, q.y), max(q.x, q.y));
  return vec2(min(rs.x, qs.x), max(rs.y, qs.y));
}

vec2 ire(vec2 v) { return v; }
vec2 iim(vec2 v) { return vec2(0); }

vec2 iarg(vec2 z) {
  if (z.y < 0.)
    return dup(radians(180.));
  if (z.x >= 0.)
    return vec2(0);
  return vec2(0., radians(180.));
}

vec2 isign(vec2 z) { return sign(z); }

vec2 ilog10(vec2 z) { return log(z) / log(10.); }

vec2 ipw(vec2 c, vec2 t, vec2 f) {
  if (c.y <= 0.)
    return f;
  if (c.x > 0.)
    return t;
  return vec2(min(t.x, f.x), max(t.y, f.y));
}

vec2 isquare(vec2 z) {
  vec2 s = z * z;
  float mx = max(s.x, s.y);
  if (z.x <= 0. && z.y >= 0.)
    return vec2(0., mx);
  float mn = min(s.x, s.y);
  return vec2(mn, mx);
}

vec2 isqrt(vec2 v) { return vec2(sqrt(max(0., v.x)), sqrt(max(0., v.y))); }
vec2 icbrt(vec2 v) { return vec2(cbrt(v.x), cbrt(v.y)); }

vec2 iceil(vec2 z) { return ceil(z); }
vec2 ifloor(vec2 z) { return floor(z); }
vec2 iround(vec2 z) { return round(z); }

vec2 imin(vec2 l, vec2 r) { return vec2(min(l.x, r.x), min(l.y, r.y)); }
vec2 imax(vec2 l, vec2 r) { return vec2(max(l.x, r.x), max(l.y, r.y)); }
vec2 imod(vec2 x, vec2 y) { return x + ineg(iby(y, ifloor(idiv(x, y)))); }

//////////////////////////////////////////////////
// With derivatives - used to calculate normals //
//////////////////////////////////////////////////

// Operations
vec4 gneg(vec4 v) { return -v; }

vec4 gby(vec4 l, vec4 r) { return vec4(l.x * r.x, l.x * r.yzw + r.x * l.yzw); }

vec4 gdiv(vec4 l, vec4 r) {
  return vec4(l.x / r.x, (r.x * l.yzw - l.x * r.yzw) / pow(r.x, 2.));
}

vec4 gpow(vec4 b, float e) {
  return vec4(pow(b.x, e), e * pow(b.x, e - 1.) * b.yzw);
}

vec4 gexp(vec4 z) { return vec4(exp(z.x), exp(z.x) * z.yzw); }
vec4 gln(vec4 z) { return vec4(log(z.x), z.yzw / z.x); }

vec4 gpow(vec4 b, vec4 e) {
  if (floor(e.x) == e.x && e.y == 0. && e.z == 0. && e.w == 0.) {
    return gpow(b, e.x);
  }
  return gexp(gby(gln(b), e));
}

vec4 gabs(vec4 v) { return vec4(abs(v.x), sign(v.x) * v.yzw); }

vec4 gsin(vec4 v) { return vec4(sin(v.x), cos(v.x) * v.yzw); }
vec4 gcos(vec4 v) { return vec4(cos(v.x), -sin(v.x) * v.yzw); }
vec4 gtan(vec4 z) {
  float c = cos(z.x);
  return vec4(tan(z.x), 1. / (c * c) * z.yzw);
}

vec4 gsinh(vec4 v) { return vec4(sinh(v.x), cosh(v.x) * v.yzw); }
vec4 gcosh(vec4 v) { return vec4(cosh(v.x), sinh(v.x) * v.yzw); }
vec4 gtanh(vec4 z) {
  float c = 1. / cosh(z.x);
  return vec4(tanh(z.x), c * c * z.yzw);
}

vec4 gasin(vec4 v) { return vec4(asin(v.x), v.yzw / sqrt(1. - v.x * v.x)); }
vec4 gacos(vec4 v) { return vec4(acos(v.x), -v.yzw / sqrt(1. - v.x * v.x)); }
vec4 gatan(vec4 v) { return vec4(atan(v.x), v.yzw / (1. + v.x * v.x)); }

vec4 gatan2(vec4 y, vec4 x) {
  return vec4(atan(y.x, x.x), y.yzw / x.yzw / (1. + y.x * y.x / x.x / x.x));
}

vec4 gre(vec4 v) { return v; }
vec4 gim(vec4 v) { return vec4(0, 0, 0, 1); }
vec4 garg(vec4 z) { return gnum(z.x >= 0. ? 0. : radians(180.)); }

vec4 glog10(vec4 z) { return vec4(log(z.x), z.yzw / z.x) / log(10.); }
vec4 gpw(vec4 c, vec4 t, vec4 f) { return c.x > 0. ? t : f; }
vec4 gsquare(vec4 z) { return vec4(z.x * z.x, 2. * z.x * z.yzw); }
vec4 gsqrt(vec4 v) { return vec4(sqrt(v.x), .5 * pow(v.x, -.5) * v.yzw); }
vec4 gcbrt(vec4 v) { return vec4(cbrt(v.x), pow(v.x, -2. / 3.) / 3. * v.yzw); }

vec4 gmin(vec4 l, vec4 r) { return l.x < r.x ? l : r; }
vec4 gmax(vec4 l, vec4 r) { return l.x > r.x ? l : r; }
vec4 gmod(vec4 l, vec4 r) { return vec4(mod(l.x, r.x), l.yzw); }

vec4 gsign(vec4 v) { return gnum(sign(v.x)); }

vec4 gceil(vec4 z) { return gnum(ceil(z.x)); }
vec4 gfloor(vec4 z) { return gnum(floor(z.x)); }
vec4 ground(vec4 z) { return gnum(round(z.x)); }

//////////////////////
// Bisect, raytrace //
//////////////////////

// bool bisect${suffix}(vec3 o, mat3 d, float max_distance, out vec3 found) {
//     mat3 from = mat3(o, o, vec3(0));
//     mat3 to = from + max_distance * d;
//     float ithreshold = ${threshold};
//     int depth = 0;
//     int choices = 0;
//     for(int it = 0; it < ${max_iterations}; it++) {
//         mat3 midpoint = 0.5 * (from + to);
//         vec2 front = interval${suffix}(from, midpoint);
//         vec2 back = interval${suffix}(midpoint, to);
//         if(depth >= ${max_depth}
//             || (front.y - front.x < ithreshold && front.x <= 0.0 && front.y >= 0.0)
//             || (back.y - back.x < ithreshold && back.x <= 0.0 && back.y >= 0.0)
//             ) {
//                 found = mix(midpoint[0], midpoint[1], 0.5);
//                 return true;
//             }
//         if(front.x <= 0.0 && front.y >= 0.0) {
//             to = midpoint;
//             depth++;
//             choices = left_shift(choices);
//         } else if(back.x <= 0.0 && back.y >= 0.0) {
//             from = midpoint;
//             depth++;
//             choices = left_shift_increment(choices);
//         } else {
//             // This could be possibly helped by https://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightBinSearch
//             for(int j = ${max_depth} - 1; j > 0; j--) {
//                 if(j > depth)
//                     continue;
//                 depth--;
//                 choices = right_shift(choices);
//                 if(is_even(choices)) {
//                     midpoint = to;
//                     to = to + (to - from);
//                     vec2 back = interval${suffix}(midpoint, to);
//                     if(back.x <= 0.0 && back.y >= 0.0) {
//                         from = midpoint;
//                         depth++;
//                         choices = left_shift_increment(choices);
//                         break;
//                     }
//                 } else {
//                     from = from - (to - from);
//                 }
//             }
//             if(depth == 0)
//                 return false;
//         }
//     }
//     return false;
// }

// bool sphere_bisect${suffix}(vec3 o, mat3 d, float max_distance, out vec3 found) {
//   vec3 center = vec3(${center.x},${center.y},${center.z});

//   vec3 to_center = o - center;
//   float b = dot(to_center, 0.5 * (d[0] + d[1]));
//   float c = dot(to_center, to_center) - ${radius} * ${radius};
//   float delta = b*b - c;
//   if(delta < 0.0)
//       return false;
//   float x = -b - sqrt(delta);
//   if(x < ${threshold})
//       return false;
//   found = o + x * 0.5 * (d[0] + d[1]);
//   return true;
// }
