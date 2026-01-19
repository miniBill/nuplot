module Glsl.Functions.NuPlot exposing
    ( log101, clog102, ilog102, glog104
    , sinh1, csinh2, isinh2, gsinh4
    , cosh1, ccosh2, icosh2, gcosh4
    , tanh1, ctanh2, itanh2, gtanh4
    , round1, round2, round3, round4, cround2, iround2
    , ground4
    , cbrt1, ccbrt2, icbrt2, gcbrt4
    , between111
    , dup1
    , hl2rgb11
    , gnum1
    , atanPlus11
    , iexpand2
    , thetaDelta1
    , axis111
    , left_shifti1, left_shifti2
    , left_shift_incrementi1, left_shift_incrementi2
    , incrementi2
    , is_eveni1, is_eveni2
    , is_oddi1
    , right_shifti2, right_shifti1
    , ci
    , cpi
    , ce
    , cby22, iby22, gby44
    , cdiv22, idiv22, gdiv44
    , cln2, iln2, gln4
    , cexp2, iexp2, gexp4
    , cpow22, ipow1i1, ipow2i1, ipow21, ipow22, gpow41
    , gpow44
    , csin2, isin2, gsin4
    , ccos2, icos2, gcos4
    , ctan2, itan2, gtan4
    , csqrt2, isqrt2, gsqrt4
    , casin2, iasin2, gasin4
    , cacos2, iacos2, gacos4
    , catan2, iatan2, gatan4
    , catan222, iatan222, gatan244
    , cabs2, iabs2, gabs4
    , csign2, isign2, gsign4
    , csquare2, isquare2, gsquare4
    , cre2, ire2, gre4
    , cim2, iim2, gim4
    , arg2, carg2, iarg2, garg4
    , cpw222, ipw222, gpw444
    , cceil2, iceil2, gceil4
    , cfloor2, ifloor2, gfloor4
    , cmin22, imin22, gmin44
    , cmax22, imax22, gmax44
    , cmod22, imod22, gmod44
    , cmbrot22
    , ineg2, gneg4
    , iinverse2
    , ilt22
    , ileq22
    , ineq22
    , ieq22
    , igeq22
    , igt22
    , u_whiteLines, u_completelyReal, u_drawAxes, u_zoomCenter, u_viewportWidth, u_canvasWidth
    , u_canvasHeight, u_phi, u_theta
    )

{-| #log10

@docs log101, clog102, ilog102, glog104

#sinh

@docs sinh1, csinh2, isinh2, gsinh4

#cosh

@docs cosh1, ccosh2, icosh2, gcosh4

#tanh

@docs tanh1, ctanh2, itanh2, gtanh4

#round

@docs round1, round2, round3, round4, cround2, iround2
@docs ground4

#cbrt

@docs cbrt1, ccbrt2, icbrt2, gcbrt4

#between

@docs between111

#dup

@docs dup1

#hl2rgb

@docs hl2rgb11

#num

@docs gnum1

#atanPlus

@docs atanPlus11

#expand

@docs iexpand2

#thetaDelta

@docs thetaDelta1

#axis

@docs axis111

#left\_shift

@docs left_shifti1, left_shifti2

#left\_shift\_increment

@docs left_shift_incrementi1, left_shift_incrementi2

#increment

@docs incrementi2

#is\_even

@docs is_eveni1, is_eveni2

#is\_odd

@docs is_oddi1

#right\_shift

@docs right_shifti2, right_shifti1

#i

@docs ci

#pi

@docs cpi

#e

@docs ce

#by

@docs cby22, iby22, gby44

#div

@docs cdiv22, idiv22, gdiv44

#ln

@docs cln2, iln2, gln4

#exp

@docs cexp2, iexp2, gexp4

#pow

@docs cpow22, ipow1i1, ipow2i1, ipow21, ipow22, gpow41
@docs gpow44

#sin

@docs csin2, isin2, gsin4

#cos

@docs ccos2, icos2, gcos4

#tan

@docs ctan2, itan2, gtan4

#sqrt

@docs csqrt2, isqrt2, gsqrt4

#asin

@docs casin2, iasin2, gasin4

#acos

@docs cacos2, iacos2, gacos4

#atan

@docs catan2, iatan2, gatan4

#atan2

@docs catan222, iatan222, gatan244

#abs

@docs cabs2, iabs2, gabs4

#sign

@docs csign2, isign2, gsign4

#square

@docs csquare2, isquare2, gsquare4

#re

@docs cre2, ire2, gre4

#im

@docs cim2, iim2, gim4

#arg

@docs arg2, carg2, iarg2, garg4

#pw

@docs cpw222, ipw222, gpw444

#ceil

@docs cceil2, iceil2, gceil4

#floor

@docs cfloor2, ifloor2, gfloor4

#min

@docs cmin22, imin22, gmin44

#max

@docs cmax22, imax22, gmax44

#mod

@docs cmod22, imod22, gmod44

#mbrot

@docs cmbrot22

#neg

@docs ineg2, gneg4

#inverse

@docs iinverse2

#lt

@docs ilt22

#leq

@docs ileq22

#neq

@docs ineq22

#eq

@docs ieq22

#geq

@docs igeq22

#gt

@docs igt22

#uniforms

@docs u_whiteLines, u_completelyReal, u_drawAxes, u_zoomCenter, u_viewportWidth, u_canvasWidth
@docs u_canvasHeight, u_phi, u_theta

-}

import Glsl


log101Body : String
log101Body =
    "return log(x) / log(10.);"


sinh1Body : String
sinh1Body =
    "return 0.5 * (exp(x) - exp((-x)));"


cosh1Body : String
cosh1Body =
    "return 0.5 * (exp(x) + exp((-x)));"


tanh1Body : String
tanh1Body =
    """{
    if (abs(x) > 10.) return sign(x);
    float p = exp(x);
    float m = exp((-x));
    return (p - m) / (p + m);
}"""


round1Body : String
round1Body =
    "return floor((v + 0.5));"


round2Body : String
round2Body =
    "return floor((v + 0.5));"


round3Body : String
round3Body =
    "return floor((v + 0.5));"


round4Body : String
round4Body =
    "return floor((v + 0.5));"


cbrt1Body : String
cbrt1Body =
    "return sign(v) * pow((abs(v)), (1. / 3.));"


between111Body : String
between111Body =
    "return low <= x && x <= high;"


dup1Body : String
dup1Body =
    "return vec2(x, x);"


hl2rgb11Body : String
hl2rgb11Body =
    """{
    vec3 rgb = clamp((abs((mod((h * 6. + vec3(0., 4., 2.)), 6.) - 3.)) - 1.), 0., 1.);
    return l + (rgb - 0.5) * (1. - abs((2. * l - 1.)));
}"""


gnum1Body : String
gnum1Body =
    "return vec4(f, 0, 0, 0);"


atanPlus11Body : String
atanPlus11Body =
    "return mod((radians(360.) + atan(y, x)), (radians(360.)));"


iexpand2Body : String
iexpand2Body =
    "return v * (vec2(1., 1.) + sign(v) * vec2(-0.000001, 0.000001));"


thetaDelta1Body : String
thetaDelta1Body =
    """{
    if (u_whiteLines < 1.) return 100.;
    float thetaSix = theta * u_whiteLines + 0.5;
    float thetaNeigh = 0.05;
    return abs((fract(thetaSix) - 0.5)) / thetaNeigh;
}"""


axis111Body : String
axis111Body =
    """{
    float across = 1. - abs((coord / maxDelta));
    if (across < -12.) return 0.;
    float smallUnit = pow(10., (ceil((log10(maxDelta)))));
    if (across < 0. && abs(otherCoord) < maxDelta * 2.) return 0.;
    float unit = across < -6. ? smallUnit * 100. : across < -0.1 ? smallUnit * 10. : smallUnit * 5.;
    float parallel = mod((abs(otherCoord)), unit) < maxDelta ? 1. : 0.;
    return max(0., (max(across, parallel)));
}"""


left_shifti1Body : String
left_shifti1Body =
    "return i * 2;"


left_shift_incrementi1Body : String
left_shift_incrementi1Body =
    "return i * 2 + 1;"


incrementi2Body : String
incrementi2Body =
    "return i.x == P32M1 ? ivec2(0, (i.y + 1)) : ivec2((i.x + 1), (i.y));"


left_shifti2Body : String
left_shifti2Body =
    "return i.x >= P31 ? ivec2((2 * (i.x - P31)), (i.y * 2 + 1)) : 2 * i;"


left_shift_incrementi2Body : String
left_shift_incrementi2Body =
    "return increment((left_shift(i)));"


is_eveni1Body : String
is_eveni1Body =
    "return i / 2 * 2 == i;"


is_oddi1Body : String
is_oddi1Body =
    "return i / 2 * 2 != i;"


is_eveni2Body : String
is_eveni2Body =
    "return is_even((i.x));"


right_shifti2Body : String
right_shifti2Body =
    "return is_odd((i.y)) ? ivec2((i.x / 2 + P31), (i.y / 2)) : i / 2;"


right_shifti1Body : String
right_shifti1Body =
    "return i / 2;"


ciBody : String
ciBody =
    "return vec2(0, 1);"


cpiBody : String
cpiBody =
    "return vec2((radians(180.)), 0);"


ceBody : String
ceBody =
    "return vec2((exp(1.)), 0);"


cby22Body : String
cby22Body =
    "return vec2((a.x * b.x - a.y * b.y), (a.x * b.y + a.y * b.x));"


cdiv22Body : String
cdiv22Body =
    """{
    float k = 1. / dot(b, b);
    float r = k * dot(a, b);
    float i = k * (a.y * b.x - a.x * b.y);
    return vec2(r, i);
}"""


cln2Body : String
cln2Body =
    """{
    if (z.y == 0. && z.x >= 0.) return vec2((log((z.x))), 0);
    float px = length(z);
    float py = atan((z.y), (z.x));
    return vec2((log(px)), py);
}"""


cexp2Body : String
cexp2Body =
    """{
    if (z.y == 0.) return vec2((exp((z.x))), 0);
    return vec2((cos((z.y))), (sin((z.y)))) * exp((z.x));
}"""


cpow22Body : String
cpow22Body =
    """{
    if (w.x >= 0. && w.y == 0. && z.y == 0.) return vec2((pow((w.x), (z.x))), 0);
    return cexp((cby((cln(w)), z)));
}"""


csin2Body : String
csin2Body =
    """{
    if (z.y == 0.) return vec2((sin((z.x))), 0);
    return vec2((sin((z.x)) * cosh((z.y))), (cos((z.x)) * sinh((z.y))));
}"""


ccos2Body : String
ccos2Body =
    """{
    if (z.y == 0.) return vec2((cos((z.x))), 0);
    return vec2((cos((z.x)) * cosh((z.y))), (sin((z.x)) * sinh((z.y))));
}"""


ctan2Body : String
ctan2Body =
    """{
    if (z.y == 0.) return vec2((tan((z.x))), 0);
    return cdiv((csin(z)), (ccos(z)));
}"""


csqrt2Body : String
csqrt2Body =
    """{
    if (z.y == 0. && z.x >= 0.) return vec2((sqrt((z.x))), 0);
    float r = pow((dot(z, z)), 0.25);
    float t = atan((z.y), (z.x)) * 0.5;
    return r * vec2((cos(t)), (sin(t)));
}"""


casin2Body : String
casin2Body =
    """{
    vec2 s = csqrt((vec2(1, 0) - cby(z, z)));
    vec2 arg = s - cby((vec2(0, 1)), z);
    return cby((vec2(0, 1)), (cln(arg)));
}"""


cacos2Body : String
cacos2Body =
    "return cpi() * 0.5 - casin(z);"


catan2Body : String
catan2Body =
    """{
    if (z.y == 0.) return vec2((atan((z.x))), 0);
    vec2 o = vec2(1, 0);
    vec2 iz = cby((vec2(0, 1)), z);
    vec2 l = cdiv((o + iz), (o - iz));
    return -0.5 * cby((vec2(0, 1)), (cln(l)));
}"""


catan222Body : String
catan222Body =
    """{
    vec2 z = vec2((x.x - y.y), (x.y + y.x));
    return vec2((atan((z.y), (z.x))), 0.);
}"""


csinh2Body : String
csinh2Body =
    "return 0.5 * (cexp(z) - cexp((-z)));"


ccosh2Body : String
ccosh2Body =
    "return 0.5 * (cexp(z) + cexp((-z)));"


ctanh2Body : String
ctanh2Body =
    """{
    vec2 p = cexp(z);
    vec2 m = cexp((-z));
    return cdiv((p - m), (p + m));
}"""


cabs2Body : String
cabs2Body =
    "return vec2((length(z)), 0.);"


csign2Body : String
csign2Body =
    "return vec2((sign((z.x))), (sign((z.y))));"


ccbrt2Body : String
ccbrt2Body =
    """{
    if (z.y == 0.) return vec2((sign((z.x)) * pow((z.x), (1. / 3.))), 0);
    float r = pow((dot(z, z)), (1. / 6.));
    float t = atan((z.y), (z.x)) / 3. + (z.x > 0. ? 0. : radians(120.));
    return r * vec2((cos(t)), (sin(t)));
}"""


csquare2Body : String
csquare2Body =
    "return cby(z, z);"


clog102Body : String
clog102Body =
    "return cln(z) / log(10.);"


cre2Body : String
cre2Body =
    "return vec2((z.x), 0);"


cim2Body : String
cim2Body =
    "return vec2((z.y), 0);"


arg2Body : String
arg2Body =
    "return atan((v.y), (v.x));"


carg2Body : String
carg2Body =
    "return vec2((atan((v.y), (v.x))), 0);"


cpw222Body : String
cpw222Body =
    "return c.x > 0. ? t : f;"


cceil2Body : String
cceil2Body =
    "return ceil(z);"


cfloor2Body : String
cfloor2Body =
    "return floor(z);"


cround2Body : String
cround2Body =
    "return floor((z + vec2(0.5, 0.5)));"


cmin22Body : String
cmin22Body =
    "return l.x < r.x ? l : r;"


cmax22Body : String
cmax22Body =
    "return l.x > r.x ? l : r;"


cmod22Body : String
cmod22Body =
    "return vec2((mod((l.x), (r.x))), 0);"


cmbrot22Body : String
cmbrot22Body =
    """{
    vec2 c = x + vec2((-y.y), (y.x));
    float p = length((c - vec2(0.25, 0)));
    if (c.x <= p - 2. * p * p + 0.25 || length((c + vec2(1, 0))) <= 0.25) return vec2(0, 0);
    vec2 z = c;
    for ( int i = 0;; i < 4000; i++) {
        {
            z = vec2((z.x * z.x - z.y * z.y), (2. * z.x * z.y)) + c;
            if (length(z) > 1000000.) {
                    float logLength = log((length(z)));
                    float nu = log((logLength / log(2.))) / log(2.);
                    float fi = float(i) - nu;
                    return vec2((sin(fi)), (cos(fi)));
                }
        }
    }
    return vec2(0, 0);
}"""


ineg2Body : String
ineg2Body =
    "return vec2((-v.y), (-v.x));"


iby22Body : String
iby22Body =
    """{
    float a = l.x * r.x;
    float b = l.x * r.y;
    float c = l.y * r.x;
    float d = l.y * r.y;
    float mn = min((min(a, b)), (min(c, d)));
    float mx = max((max(a, b)), (max(c, d)));
    return vec2(mn, mx);
}"""


iinverse2Body : String
iinverse2Body =
    """{
    if (y.x <= 0. && y.y >= 0.) return vec2((-1. / 0.), (1. / 0.));
    if (y.y == 0.) return vec2((-1. / 0.), (1. / y.x));
    if (y.x == 0.) return vec2((1. / y.y), (1. / 0.));
    return vec2((1. / y.y), (1. / y.x));
}"""


idiv22Body : String
idiv22Body =
    "return iby(l, (iinverse(r)));"


ipow1i1Body : String
ipow1i1Body =
    """{
    float fe = float(e);
    if (mod(fe, 2.) == 0.) return pow((abs(b)), fe);
    return b * pow((abs(b)), (fe - 1.));
}"""


ipow2i1Body : String
ipow2i1Body =
    """{
    if (e == 0) return vec2(1, 1);
    if (e == 1) return b;
    float xe = ipow((b.x), e);
    float ye = ipow((b.y), e);
    float mn = min(xe, ye);
    float mx = max(xe, ye);
    if (mod((float(e)), 2.) == 0. && b.x <= 0. && b.y >= 0.) return vec2((min(0., mn)), (max(0., mx)));
    return vec2(mn, mx);
}"""


iln2Body : String
iln2Body =
    "return log(z);"


iexp2Body : String
iexp2Body =
    "return exp(z);"


ipow21Body : String
ipow21Body =
    """{
    if (abs((e - round(e))) < 0.000001) return ipow(b, (int(e)));
    return iexp((e * iln(b)));
}"""


ipow22Body : String
ipow22Body =
    """{
    if (e.y - e.x < 0.000001 && abs((e.x - round((e.x)))) < 0.000001) return ipow(b, (int((e.x))));
    return iexp((iby((iln(b)), e)));
}"""


ilt22Body : String
ilt22Body =
    "return vec2((r.x - l.y), (r.y - l.x));"


ileq22Body : String
ileq22Body =
    "return vec2((r.x - l.y), (r.y - l.x));"


ineq22Body : String
ineq22Body =
    "return vec2((r.x - l.y), (r.y - l.x));"


ieq22Body : String
ieq22Body =
    "return vec2((l.x - r.y), (l.y - r.x));"


igeq22Body : String
igeq22Body =
    "return vec2((l.x - r.y), (l.y - r.x));"


igt22Body : String
igt22Body =
    "return vec2((l.x - r.y), (l.y - r.x));"


iabs2Body : String
iabs2Body =
    """{
    if (z.x <= 0. && z.y >= 0.) return vec2(0, (max((z.y), (abs((z.x))))));
    if (z.x <= 0.) return vec2((-z.y), (-z.x));
    return z;
}"""


isin2Body : String
isin2Body =
    """{
    if (v.y - v.x > radians(360.)) return vec2(-1., 1.);
    float from = mod((v.x), (radians(360.)));
    float to = from + v.y - v.x;
    vec2 s = sin((vec2(from, to)));
    vec2 res = vec2((min((s.x), (s.y))), (max((s.x), (s.y))));
    if (between((radians(90.)), from, to) || between((radians((90. + 360.))), from, to)) res.y = 1.;
    if (between((radians(270.)), from, to) || between((radians((270. + 360.))), from, to)) res.x = -1.;
    return res;
}"""


icos2Body : String
icos2Body =
    """{
    vec2 shift = radians(90.) * vec2(1, 1);
    return isin((v + shift));
}"""


itan2Body : String
itan2Body =
    "return idiv((isin(z)), (icos(z)));"


isinh2Body : String
isinh2Body =
    "return vec2((sinh((v.x))), (sinh((v.y))));"


icosh2Body : String
icosh2Body =
    """{
    if (z.x <= 0. && z.y >= 0.) return vec2((cosh(0.)), (max((z.y), (abs((z.x))))));
    if (z.x <= 0.) return vec2((cosh((-z.y))), (cosh((-z.x))));
    return vec2((cosh((z.x))), (cosh((z.y))));
}"""


itanh2Body : String
itanh2Body =
    "return vec2((tanh((z.x))), (tanh((z.y))));"


iasin2Body : String
iasin2Body =
    """{
    vec2 clamped = vec2((max((z.x), -1.)), (min((z.y), 1.)));
    return asin(clamped);
}"""


iacos2Body : String
iacos2Body =
    """{
    vec2 clamped = vec2((max((z.x), -1.)), (min((z.y), 1.)));
    return acos(clamped).yx;
}"""


iatan2Body : String
iatan2Body =
    "return atan(z);"


iatan222Body : String
iatan222Body =
    """{
    vec2 r = vec2((atan((y.x), (x.x))), (atan((y.y), (x.x))));
    vec2 q = vec2((atan((y.x), (x.y))), (atan((y.y), (x.y))));
    vec2 rs = vec2((min((r.x), (r.y))), (max((r.x), (r.y))));
    vec2 qs = vec2((min((q.x), (q.y))), (max((q.x), (q.y))));
    return vec2((min((rs.x), (qs.x))), (max((rs.y), (qs.y))));
}"""


ire2Body : String
ire2Body =
    "return v;"


iim2Body : String
iim2Body =
    "return vec2(0);"


iarg2Body : String
iarg2Body =
    """{
    if (z.y < 0.) return dup((radians(180.)));
    if (z.x >= 0.) return vec2(0);
    return vec2(0., (radians(180.)));
}"""


isign2Body : String
isign2Body =
    "return sign(z);"


ilog102Body : String
ilog102Body =
    "return log(z) / log(10.);"


ipw222Body : String
ipw222Body =
    """{
    if (c.y <= 0.) return f;
    if (c.x > 0.) return t;
    return vec2((min((t.x), (f.x))), (max((t.y), (f.y))));
}"""


isquare2Body : String
isquare2Body =
    """{
    vec2 s = z * z;
    float mx = max((s.x), (s.y));
    if (z.x <= 0. && z.y >= 0.) return vec2(0., mx);
    float mn = min((s.x), (s.y));
    return vec2(mn, mx);
}"""


isqrt2Body : String
isqrt2Body =
    "return vec2((sqrt((max(0., (v.x))))), (sqrt((max(0., (v.y))))));"


icbrt2Body : String
icbrt2Body =
    "return vec2((cbrt((v.x))), (cbrt((v.y))));"


iceil2Body : String
iceil2Body =
    "return ceil(z);"


ifloor2Body : String
ifloor2Body =
    "return floor(z);"


iround2Body : String
iround2Body =
    "return round(z);"


imin22Body : String
imin22Body =
    "return vec2((min((l.x), (r.x))), (min((l.y), (r.y))));"


imax22Body : String
imax22Body =
    "return vec2((max((l.x), (r.x))), (max((l.y), (r.y))));"


imod22Body : String
imod22Body =
    "return x + ineg((iby(y, (ifloor((idiv(x, y)))))));"


gneg4Body : String
gneg4Body =
    "return -v;"


gby44Body : String
gby44Body =
    "return vec4((l.x * r.x), (l.x * r.yzw + r.x * l.yzw));"


gdiv44Body : String
gdiv44Body =
    "return vec4((l.x / r.x), ((r.x * l.yzw - l.x * r.yzw) / pow((r.x), 2.)));"


gpow41Body : String
gpow41Body =
    "return vec4((pow((b.x), e)), (e * pow((b.x), (e - 1.)) * b.yzw));"


gexp4Body : String
gexp4Body =
    "return vec4((exp((z.x))), (exp((z.x)) * z.yzw));"


gln4Body : String
gln4Body =
    "return vec4((log((z.x))), (z.yzw / z.x));"


gpow44Body : String
gpow44Body =
    """{
    if (floor((e.x)) == e.x && e.y == 0. && e.z == 0. && e.w == 0.) return gpow(b, (e.x));
    return gexp((gby((gln(b)), e)));
}"""


gabs4Body : String
gabs4Body =
    "return vec4((abs((v.x))), (sign((v.x)) * v.yzw));"


gsin4Body : String
gsin4Body =
    "return vec4((sin((v.x))), (cos((v.x)) * v.yzw));"


gcos4Body : String
gcos4Body =
    "return vec4((cos((v.x))), (-sin((v.x)) * v.yzw));"


gtan4Body : String
gtan4Body =
    """{
    float c = cos((z.x));
    return vec4((tan((z.x))), (1. / (c * c) * z.yzw));
}"""


gsinh4Body : String
gsinh4Body =
    "return vec4((sinh((v.x))), (cosh((v.x)) * v.yzw));"


gcosh4Body : String
gcosh4Body =
    "return vec4((cosh((v.x))), (sinh((v.x)) * v.yzw));"


gtanh4Body : String
gtanh4Body =
    """{
    float c = 1. / cosh((z.x));
    return vec4((tanh((z.x))), (c * c * z.yzw));
}"""


gasin4Body : String
gasin4Body =
    "return vec4((asin((v.x))), (v.yzw / sqrt((1. - v.x * v.x))));"


gacos4Body : String
gacos4Body =
    "return vec4((acos((v.x))), (-v.yzw / sqrt((1. - v.x * v.x))));"


gatan4Body : String
gatan4Body =
    "return vec4((atan((v.x))), (v.yzw / (1. + v.x * v.x)));"


gatan244Body : String
gatan244Body =
    "return vec4((atan((y.x), (x.x))), (y.yzw / x.yzw / (1. + y.x * y.x / x.x / x.x)));"


gre4Body : String
gre4Body =
    "return v;"


gim4Body : String
gim4Body =
    "return vec4(0, 0, 0, 1);"


garg4Body : String
garg4Body =
    "return gnum((z.x >= 0. ? 0. : radians(180.)));"


glog104Body : String
glog104Body =
    "return vec4((log((z.x))), (z.yzw / z.x)) / log(10.);"


gpw444Body : String
gpw444Body =
    "return c.x > 0. ? t : f;"


gsquare4Body : String
gsquare4Body =
    "return vec4((z.x * z.x), (2. * z.x * z.yzw));"


gsqrt4Body : String
gsqrt4Body =
    "return vec4((sqrt((v.x))), (0.5 * pow((v.x), -0.5) * v.yzw));"


gcbrt4Body : String
gcbrt4Body =
    "return vec4((cbrt((v.x))), (pow((v.x), (-2. / 3.)) / 3. * v.yzw));"


gmin44Body : String
gmin44Body =
    "return l.x < r.x ? l : r;"


gmax44Body : String
gmax44Body =
    "return l.x > r.x ? l : r;"


gmod44Body : String
gmod44Body =
    "return vec4((mod((l.x), (r.x))), (l.yzw));"


gsign4Body : String
gsign4Body =
    "return gnum((sign((v.x))));"


gceil4Body : String
gceil4Body =
    "return gnum((ceil((z.x))));"


gfloor4Body : String
gfloor4Body =
    "return gnum((floor((z.x))));"


ground4Body : String
ground4Body =
    "return gnum((round((z.x))));"


log101 : Glsl.Expression Float -> Glsl.Expression Float
log101 x =
    Glsl.unsafeCall1 "log10" [ logBody, log101Body ] x


clog102 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
clog102 z =
    Glsl.unsafeCall1 "clog10" [ logBody, clnBody, clog102Body ] z


ilog102 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
ilog102 z =
    Glsl.unsafeCall1 "ilog10" [ logBody, ilog102Body ] z


glog104 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
glog104 z =
    Glsl.unsafeCall1 "glog10" [ logBody, vec4Body, glog104Body ] z


sinh1 : Glsl.Expression Float -> Glsl.Expression Float
sinh1 x =
    Glsl.unsafeCall1 "sinh" [ expBody, sinh1Body ] x


csinh2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
csinh2 z =
    Glsl.unsafeCall1 "csinh" [ cexpBody, csinh2Body ] z


isinh2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
isinh2 v =
    Glsl.unsafeCall1 "isinh" [ vec2Body, sinhBody, isinh2Body ] v


gsinh4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gsinh4 v =
    Glsl.unsafeCall1 "gsinh" [ vec4Body, sinhBody, coshBody, gsinh4Body ] v


cosh1 : Glsl.Expression Float -> Glsl.Expression Float
cosh1 x =
    Glsl.unsafeCall1 "cosh" [ expBody, cosh1Body ] x


ccosh2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
ccosh2 z =
    Glsl.unsafeCall1 "ccosh" [ cexpBody, ccosh2Body ] z


icosh2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
icosh2 z =
    Glsl.unsafeCall1
        "icosh"
        [ vec2Body, coshBody, maxBody, absBody, icosh2Body ]
        z


gcosh4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gcosh4 v =
    Glsl.unsafeCall1 "gcosh" [ vec4Body, coshBody, sinhBody, gcosh4Body ] v


tanh1 : Glsl.Expression Float -> Glsl.Expression Float
tanh1 x =
    Glsl.unsafeCall1 "tanh" [ absBody, signBody, expBody, tanh1Body ] x


ctanh2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
ctanh2 z =
    Glsl.unsafeCall1 "ctanh" [ cexpBody, cdivBody, ctanh2Body ] z


itanh2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
itanh2 z =
    Glsl.unsafeCall1 "itanh" [ vec2Body, tanhBody, itanh2Body ] z


gtanh4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gtanh4 z =
    Glsl.unsafeCall1 "gtanh" [ coshBody, vec4Body, tanhBody, gtanh4Body ] z


round1 : Glsl.Expression Float -> Glsl.Expression Float
round1 v =
    Glsl.unsafeCall1 "round" [ floorBody, round1Body ] v


round2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
round2 v =
    Glsl.unsafeCall1 "round" [ floorBody, round2Body ] v


round3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
round3 v =
    Glsl.unsafeCall1 "round" [ floorBody, round3Body ] v


round4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
round4 v =
    Glsl.unsafeCall1 "round" [ floorBody, round4Body ] v


cround2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
cround2 z =
    Glsl.unsafeCall1 "cround" [ floorBody, vec2Body, cround2Body ] z


iround2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iround2 z =
    Glsl.unsafeCall1 "iround" [ roundBody, iround2Body ] z


ground4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
ground4 z =
    Glsl.unsafeCall1 "ground" [ gnumBody, roundBody, ground4Body ] z


cbrt1 : Glsl.Expression Float -> Glsl.Expression Float
cbrt1 v =
    Glsl.unsafeCall1 "cbrt" [ powBody, absBody, signBody, cbrt1Body ] v


ccbrt2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
ccbrt2 z =
    Glsl.unsafeCall1
        "ccbrt"
        [ vec2Body
        , powBody
        , signBody
        , dotBody
        , radiansBody
        , atanBody
        , cosBody
        , sinBody
        , ccbrt2Body
        ]
        z


icbrt2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
icbrt2 v =
    Glsl.unsafeCall1 "icbrt" [ vec2Body, cbrtBody, icbrt2Body ] v


gcbrt4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gcbrt4 v =
    Glsl.unsafeCall1 "gcbrt" [ vec4Body, cbrtBody, powBody, gcbrt4Body ] v


between111 :
    Glsl.Expression Float
    -> Glsl.Expression Float
    -> Glsl.Expression Float
    -> Glsl.Expression Bool
between111 x low high =
    Glsl.unsafeCall3 "between" [ between111Body ] x low high


dup1 : Glsl.Expression Float -> Glsl.Expression Glsl.Vec2
dup1 x =
    Glsl.unsafeCall1 "dup" [ vec2Body, dup1Body ] x


hl2rgb11 : Glsl.Expression Float -> Glsl.Expression Float -> Glsl.Expression Glsl.Vec3
hl2rgb11 h l =
    Glsl.unsafeCall2
        "hl2rgb"
        [ clampBody, absBody, modBody, vec3Body, hl2rgb11Body ]
        h
        l


gnum1 : Glsl.Expression Float -> Glsl.Expression Glsl.Vec4
gnum1 f =
    Glsl.unsafeCall1 "gnum" [ vec4Body, gnum1Body ] f


atanPlus11 : Glsl.Expression Float -> Glsl.Expression Float -> Glsl.Expression Float
atanPlus11 y x =
    Glsl.unsafeCall2
        "atanPlus"
        [ modBody, atanBody, radiansBody, atanPlus11Body ]
        y
        x


iexpand2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iexpand2 v =
    Glsl.unsafeCall1 "iexpand" [ vec2Body, signBody, iexpand2Body ] v


thetaDelta1 : Glsl.Expression Float -> Glsl.Expression Float
thetaDelta1 theta =
    Glsl.unsafeCall1 "thetaDelta" [ absBody, fractBody, thetaDelta1Body ] theta


axis111 :
    Glsl.Expression Float
    -> Glsl.Expression Float
    -> Glsl.Expression Float
    -> Glsl.Expression Float
axis111 coord otherCoord maxDelta =
    Glsl.unsafeCall3
        "axis"
        [ absBody, powBody, ceilBody, log10Body, modBody, maxBody, axis111Body ]
        coord
        otherCoord
        maxDelta


left_shifti1 : Glsl.Expression Int -> Glsl.Expression Int
left_shifti1 i =
    Glsl.unsafeCall1 "left_shift" [ left_shifti1Body ] i


left_shifti2 : Glsl.Expression Glsl.IVec2 -> Glsl.Expression Glsl.IVec2
left_shifti2 i =
    Glsl.unsafeCall1 "left_shift" [ ivec2Body, left_shifti2Body ] i


left_shift_incrementi1 : Glsl.Expression Int -> Glsl.Expression Int
left_shift_incrementi1 i =
    Glsl.unsafeCall1 "left_shift_increment" [ left_shift_incrementi1Body ] i


left_shift_incrementi2 : Glsl.Expression Glsl.IVec2 -> Glsl.Expression Glsl.IVec2
left_shift_incrementi2 i =
    Glsl.unsafeCall1
        "left_shift_increment"
        [ incrementBody, left_shiftBody, left_shift_incrementi2Body ]
        i


incrementi2 : Glsl.Expression Glsl.IVec2 -> Glsl.Expression Glsl.IVec2
incrementi2 i =
    Glsl.unsafeCall1 "increment" [ ivec2Body, incrementi2Body ] i


is_eveni1 : Glsl.Expression Int -> Glsl.Expression Bool
is_eveni1 i =
    Glsl.unsafeCall1 "is_even" [ is_eveni1Body ] i


is_eveni2 : Glsl.Expression Glsl.IVec2 -> Glsl.Expression Bool
is_eveni2 i =
    Glsl.unsafeCall1 "is_even" [ is_evenBody, is_eveni2Body ] i


is_oddi1 : Glsl.Expression Int -> Glsl.Expression Bool
is_oddi1 i =
    Glsl.unsafeCall1 "is_odd" [ is_oddi1Body ] i


right_shifti2 : Glsl.Expression Glsl.IVec2 -> Glsl.Expression Glsl.IVec2
right_shifti2 i =
    Glsl.unsafeCall1
        "right_shift"
        [ ivec2Body, is_oddBody, right_shifti2Body ]
        i


right_shifti1 : Glsl.Expression Int -> Glsl.Expression Int
right_shifti1 i =
    Glsl.unsafeCall1 "right_shift" [ right_shifti1Body ] i


ci : Glsl.Expression Glsl.Vec2
ci =
    Glsl.unsafeCall0 "ci" [ vec2Body, ciBody ]


cpi : Glsl.Expression Glsl.Vec2
cpi =
    Glsl.unsafeCall0 "cpi" [ vec2Body, radiansBody, cpiBody ]


ce : Glsl.Expression Glsl.Vec2
ce =
    Glsl.unsafeCall0 "ce" [ vec2Body, expBody, ceBody ]


cby22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
cby22 a b =
    Glsl.unsafeCall2 "cby" [ vec2Body, cby22Body ] a b


iby22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
iby22 l r =
    Glsl.unsafeCall2 "iby" [ minBody, maxBody, vec2Body, iby22Body ] l r


gby44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
gby44 l r =
    Glsl.unsafeCall2 "gby" [ vec4Body, gby44Body ] l r


cdiv22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
cdiv22 a b =
    Glsl.unsafeCall2 "cdiv" [ dotBody, vec2Body, cdiv22Body ] a b


idiv22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
idiv22 l r =
    Glsl.unsafeCall2 "idiv" [ ibyBody, iinverseBody, idiv22Body ] l r


gdiv44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
gdiv44 l r =
    Glsl.unsafeCall2 "gdiv" [ vec4Body, powBody, gdiv44Body ] l r


cln2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
cln2 z =
    Glsl.unsafeCall1
        "cln"
        [ vec2Body, logBody, lengthBody, atanBody, cln2Body ]
        z


iln2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iln2 z =
    Glsl.unsafeCall1 "iln" [ logBody, iln2Body ] z


gln4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gln4 z =
    Glsl.unsafeCall1 "gln" [ vec4Body, logBody, gln4Body ] z


cexp2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
cexp2 z =
    Glsl.unsafeCall1 "cexp" [ vec2Body, expBody, cosBody, sinBody, cexp2Body ] z


iexp2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iexp2 z =
    Glsl.unsafeCall1 "iexp" [ expBody, iexp2Body ] z


gexp4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gexp4 z =
    Glsl.unsafeCall1 "gexp" [ vec4Body, expBody, gexp4Body ] z


cpow22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
cpow22 w z =
    Glsl.unsafeCall2
        "cpow"
        [ vec2Body, powBody, cexpBody, cbyBody, clnBody, cpow22Body ]
        w
        z


ipow1i1 : Glsl.Expression Float -> Glsl.Expression Int -> Glsl.Expression Float
ipow1i1 b e =
    Glsl.unsafeCall2
        "ipow"
        [ floatBody, modBody, powBody, absBody, ipow1i1Body ]
        b
        e


ipow2i1 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Int
    -> Glsl.Expression Glsl.Vec2
ipow2i1 b e =
    Glsl.unsafeCall2
        "ipow"
        [ vec2Body
        , ipowBody
        , minBody
        , maxBody
        , modBody
        , floatBody
        , ipow2i1Body
        ]
        b
        e


ipow21 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Float
    -> Glsl.Expression Glsl.Vec2
ipow21 b e =
    Glsl.unsafeCall2
        "ipow"
        [ absBody, roundBody, ipowBody, intBody, iexpBody, ilnBody, ipow21Body ]
        b
        e


ipow22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
ipow22 b e =
    Glsl.unsafeCall2
        "ipow"
        [ absBody
        , roundBody
        , ipowBody
        , intBody
        , iexpBody
        , ibyBody
        , ilnBody
        , ipow22Body
        ]
        b
        e


gpow41 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Float
    -> Glsl.Expression Glsl.Vec4
gpow41 b e =
    Glsl.unsafeCall2 "gpow" [ vec4Body, powBody, gpow41Body ] b e


gpow44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
gpow44 b e =
    Glsl.unsafeCall2
        "gpow"
        [ floorBody, gpowBody, gexpBody, gbyBody, glnBody, gpow44Body ]
        b
        e


csin2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
csin2 z =
    Glsl.unsafeCall1
        "csin"
        [ vec2Body, sinBody, coshBody, sinhBody, cosBody, csin2Body ]
        z


isin2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
isin2 v =
    Glsl.unsafeCall1
        "isin"
        [ radiansBody
        , vec2Body
        , modBody
        , sinBody
        , minBody
        , maxBody
        , betweenBody
        , isin2Body
        ]
        v


gsin4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gsin4 v =
    Glsl.unsafeCall1 "gsin" [ vec4Body, sinBody, cosBody, gsin4Body ] v


ccos2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
ccos2 z =
    Glsl.unsafeCall1
        "ccos"
        [ vec2Body, cosBody, coshBody, sinhBody, sinBody, ccos2Body ]
        z


icos2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
icos2 v =
    Glsl.unsafeCall1 "icos" [ vec2Body, radiansBody, isinBody, icos2Body ] v


gcos4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gcos4 v =
    Glsl.unsafeCall1 "gcos" [ vec4Body, cosBody, sinBody, gcos4Body ] v


ctan2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
ctan2 z =
    Glsl.unsafeCall1
        "ctan"
        [ vec2Body, tanBody, cdivBody, csinBody, ccosBody, ctan2Body ]
        z


itan2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
itan2 z =
    Glsl.unsafeCall1 "itan" [ idivBody, isinBody, icosBody, itan2Body ] z


gtan4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gtan4 z =
    Glsl.unsafeCall1 "gtan" [ cosBody, vec4Body, tanBody, gtan4Body ] z


csqrt2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
csqrt2 z =
    Glsl.unsafeCall1
        "csqrt"
        [ vec2Body
        , sqrtBody
        , powBody
        , dotBody
        , atanBody
        , cosBody
        , sinBody
        , csqrt2Body
        ]
        z


isqrt2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
isqrt2 v =
    Glsl.unsafeCall1 "isqrt" [ vec2Body, sqrtBody, maxBody, isqrt2Body ] v


gsqrt4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gsqrt4 v =
    Glsl.unsafeCall1 "gsqrt" [ vec4Body, sqrtBody, powBody, gsqrt4Body ] v


casin2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
casin2 z =
    Glsl.unsafeCall1
        "casin"
        [ csqrtBody, cbyBody, vec2Body, clnBody, casin2Body ]
        z


iasin2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iasin2 z =
    Glsl.unsafeCall1
        "iasin"
        [ vec2Body, maxBody, minBody, asinBody, iasin2Body ]
        z


gasin4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gasin4 v =
    Glsl.unsafeCall1 "gasin" [ vec4Body, asinBody, sqrtBody, gasin4Body ] v


cacos2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
cacos2 z =
    Glsl.unsafeCall1 "cacos" [ casinBody, cpiBody, cacos2Body ] z


iacos2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iacos2 z =
    Glsl.unsafeCall1
        "iacos"
        [ vec2Body, maxBody, minBody, acosBody, iacos2Body ]
        z


gacos4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gacos4 v =
    Glsl.unsafeCall1 "gacos" [ vec4Body, acosBody, sqrtBody, gacos4Body ] v


catan2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
catan2 z =
    Glsl.unsafeCall1
        "catan"
        [ vec2Body, atanBody, cbyBody, cdivBody, clnBody, catan2Body ]
        z


iatan2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iatan2 z =
    Glsl.unsafeCall1 "iatan" [ atanBody, iatan2Body ] z


gatan4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gatan4 v =
    Glsl.unsafeCall1 "gatan" [ vec4Body, atanBody, gatan4Body ] v


catan222 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
catan222 y x =
    Glsl.unsafeCall2 "catan2" [ vec2Body, atanBody, catan222Body ] y x


iatan222 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
iatan222 y x =
    Glsl.unsafeCall2
        "iatan2"
        [ vec2Body, atanBody, minBody, maxBody, iatan222Body ]
        y
        x


gatan244 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
gatan244 y x =
    Glsl.unsafeCall2 "gatan2" [ vec4Body, atanBody, gatan244Body ] y x


cabs2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
cabs2 z =
    Glsl.unsafeCall1 "cabs" [ vec2Body, lengthBody, cabs2Body ] z


iabs2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iabs2 z =
    Glsl.unsafeCall1 "iabs" [ vec2Body, maxBody, absBody, iabs2Body ] z


gabs4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gabs4 v =
    Glsl.unsafeCall1 "gabs" [ vec4Body, absBody, signBody, gabs4Body ] v


csign2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
csign2 z =
    Glsl.unsafeCall1 "csign" [ vec2Body, signBody, csign2Body ] z


isign2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
isign2 z =
    Glsl.unsafeCall1 "isign" [ signBody, isign2Body ] z


gsign4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gsign4 v =
    Glsl.unsafeCall1 "gsign" [ gnumBody, signBody, gsign4Body ] v


csquare2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
csquare2 z =
    Glsl.unsafeCall1 "csquare" [ cbyBody, csquare2Body ] z


isquare2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
isquare2 z =
    Glsl.unsafeCall1 "isquare" [ maxBody, vec2Body, minBody, isquare2Body ] z


gsquare4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gsquare4 z =
    Glsl.unsafeCall1 "gsquare" [ vec4Body, gsquare4Body ] z


cre2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
cre2 z =
    Glsl.unsafeCall1 "cre" [ vec2Body, cre2Body ] z


ire2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
ire2 v =
    Glsl.unsafeCall1 "ire" [ ire2Body ] v


gre4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gre4 v =
    Glsl.unsafeCall1 "gre" [ gre4Body ] v


cim2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
cim2 z =
    Glsl.unsafeCall1 "cim" [ vec2Body, cim2Body ] z


iim2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iim2 v =
    Glsl.unsafeCall1 "iim" [ vec2Body, iim2Body ] v


gim4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gim4 v =
    Glsl.unsafeCall1 "gim" [ vec4Body, gim4Body ] v


arg2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Float
arg2 v =
    Glsl.unsafeCall1 "arg" [ atanBody, arg2Body ] v


carg2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
carg2 v =
    Glsl.unsafeCall1 "carg" [ vec2Body, atanBody, carg2Body ] v


iarg2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iarg2 z =
    Glsl.unsafeCall1 "iarg" [ dupBody, radiansBody, vec2Body, iarg2Body ] z


garg4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
garg4 z =
    Glsl.unsafeCall1 "garg" [ gnumBody, radiansBody, garg4Body ] z


cpw222 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
cpw222 c t f =
    Glsl.unsafeCall3 "cpw" [ cpw222Body ] c t f


ipw222 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
ipw222 c t f =
    Glsl.unsafeCall3 "ipw" [ vec2Body, minBody, maxBody, ipw222Body ] c t f


gpw444 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
gpw444 c t f =
    Glsl.unsafeCall3 "gpw" [ gpw444Body ] c t f


cceil2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
cceil2 z =
    Glsl.unsafeCall1 "cceil" [ ceilBody, cceil2Body ] z


iceil2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iceil2 z =
    Glsl.unsafeCall1 "iceil" [ ceilBody, iceil2Body ] z


gceil4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gceil4 z =
    Glsl.unsafeCall1 "gceil" [ gnumBody, ceilBody, gceil4Body ] z


cfloor2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
cfloor2 z =
    Glsl.unsafeCall1 "cfloor" [ floorBody, cfloor2Body ] z


ifloor2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
ifloor2 z =
    Glsl.unsafeCall1 "ifloor" [ floorBody, ifloor2Body ] z


gfloor4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gfloor4 z =
    Glsl.unsafeCall1 "gfloor" [ gnumBody, floorBody, gfloor4Body ] z


cmin22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
cmin22 l r =
    Glsl.unsafeCall2 "cmin" [ cmin22Body ] l r


imin22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
imin22 l r =
    Glsl.unsafeCall2 "imin" [ vec2Body, minBody, imin22Body ] l r


gmin44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
gmin44 l r =
    Glsl.unsafeCall2 "gmin" [ gmin44Body ] l r


cmax22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
cmax22 l r =
    Glsl.unsafeCall2 "cmax" [ cmax22Body ] l r


imax22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
imax22 l r =
    Glsl.unsafeCall2 "imax" [ vec2Body, maxBody, imax22Body ] l r


gmax44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
gmax44 l r =
    Glsl.unsafeCall2 "gmax" [ gmax44Body ] l r


cmod22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
cmod22 l r =
    Glsl.unsafeCall2 "cmod" [ vec2Body, modBody, cmod22Body ] l r


imod22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
imod22 x y =
    Glsl.unsafeCall2
        "imod"
        [ inegBody, ibyBody, ifloorBody, idivBody, imod22Body ]
        x
        y


gmod44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
gmod44 l r =
    Glsl.unsafeCall2 "gmod" [ vec4Body, modBody, gmod44Body ] l r


cmbrot22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
cmbrot22 x y =
    Glsl.unsafeCall2
        "cmbrot"
        [ vec2Body
        , lengthBody
        , logBody
        , floatBody
        , sinBody
        , cosBody
        , cmbrot22Body
        ]
        x
        y


ineg2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
ineg2 v =
    Glsl.unsafeCall1 "ineg" [ vec2Body, ineg2Body ] v


gneg4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
gneg4 v =
    Glsl.unsafeCall1 "gneg" [ gneg4Body ] v


iinverse2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
iinverse2 y =
    Glsl.unsafeCall1 "iinverse" [ vec2Body, iinverse2Body ] y


ilt22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
ilt22 l r =
    Glsl.unsafeCall2 "ilt" [ vec2Body, ilt22Body ] l r


ileq22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
ileq22 l r =
    Glsl.unsafeCall2 "ileq" [ vec2Body, ileq22Body ] l r


ineq22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
ineq22 l r =
    Glsl.unsafeCall2 "ineq" [ vec2Body, ineq22Body ] l r


ieq22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
ieq22 l r =
    Glsl.unsafeCall2 "ieq" [ vec2Body, ieq22Body ] l r


igeq22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
igeq22 l r =
    Glsl.unsafeCall2 "igeq" [ vec2Body, igeq22Body ] l r


igt22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
igt22 l r =
    Glsl.unsafeCall2 "igt" [ vec2Body, igt22Body ] l r


u_whiteLines : Glsl.Expression Float
u_whiteLines =
    Glsl.unsafeVar "u_whiteLines"


u_completelyReal : Glsl.Expression Float
u_completelyReal =
    Glsl.unsafeVar "u_completelyReal"


u_drawAxes : Glsl.Expression Float
u_drawAxes =
    Glsl.unsafeVar "u_drawAxes"


u_zoomCenter : Glsl.Expression Glsl.Vec2
u_zoomCenter =
    Glsl.unsafeVar "u_zoomCenter"


u_viewportWidth : Glsl.Expression Float
u_viewportWidth =
    Glsl.unsafeVar "u_viewportWidth"


u_canvasWidth : Glsl.Expression Float
u_canvasWidth =
    Glsl.unsafeVar "u_canvasWidth"


u_canvasHeight : Glsl.Expression Float
u_canvasHeight =
    Glsl.unsafeVar "u_canvasHeight"


u_phi : Glsl.Expression Float
u_phi =
    Glsl.unsafeVar "u_phi"


u_theta : Glsl.Expression Float
u_theta =
    Glsl.unsafeVar "u_theta"
