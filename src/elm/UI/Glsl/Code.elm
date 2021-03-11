module UI.Glsl.Code exposing (constantToGlsl, deindent, expressionToGlsl, floatToGlsl, intervalFunctionToGlsl, intervalOperationToGlsl, mainGlsl, straightFunctionToGlsl, straightOperationToGlsl, suffixToBisect, thetaDelta, threshold, toSrc3D, toSrcContour, toSrcImplicit, toSrcParametric, toSrcPolar, toSrcRelation)

import Expression exposing (Expression(..), FunctionName(..), KnownFunction(..), PrintExpression(..), RelationOperation(..))
import UI.Glsl.Model exposing (GlslConstant(..), GlslFunction(..), GlslOperation(..))


constantToGlsl : GlslConstant -> String
constantToGlsl c =
    case c of
        I ->
            """
            vec2 i() {
                return vec2(0,1);
            }
            """

        Pi ->
            """
            vec2 pi() {
                return vec2(radians(180.0), 0.0);
            }
            """

        E ->
            """
            vec2 e() {
                return vec2(exp(1.0), 0.0);
            }
            """


straightOperationToGlsl : GlslOperation -> String
straightOperationToGlsl op =
    case op of
        GlslAddition ->
            ""

        GlslMultiplication ->
            """
            vec2 by(vec2 a, vec2 b) {
                return vec2(a.x*b.x-a.y*b.y, a.x*b.y+a.y*b.x);
            }

            vec2 by(vec2 a, vec2 b, vec2 c) {
                return by(by(a, b), c);
            }
            """

        GlslNegation ->
            ""

        GlslDivision ->
            """
            vec2 div(vec2 a, vec2 b) {
                float k = 1.0 / dot(b, b);
                float r = k * dot(a, b);
                float i = k * (a.y*b.x - a.x*b.y);
                return vec2(r, i);
            }
            """

        GlslPower ->
            """
            vec2 cpow(vec2 w, vec2 z) {
                if(w.x >= 0.0 && w.y == 0.0 && z.y == 0.0) {
                    return vec2(pow(w.x, z.x), 0);
                }
                return cexp(by(cln(w), z));
            }
            """

        GlslRelations ->
            ""


intervalOperationToGlsl : GlslOperation -> String
intervalOperationToGlsl op =
    case op of
        GlslAddition ->
            ""

        GlslNegation ->
            """
            vec2 ineg(vec2 v) {
                return vec2(-v.y, -v.x);
            }

            vec4 gneg(vec4 v) {
                return -v;
            }
            """

        GlslMultiplication ->
            """
            vec2 iby(vec2 l, vec2 r) {
                float a = l.x * r.x;
                float b = l.x * r.y;
                float c = l.y * r.x;
                float d = l.y * r.y;
                float mn = min(min(a,b),min(c,d));
                float mx = max(max(a,b),max(c,d));
                return vec2(mn, mx);
            }

            vec4 gby(vec4 l, vec4 r) {
                return vec4(
                    l.x * r.x,
                    l.x * r.yzw + r.x * l.yzw);
            }
            """

        GlslDivision ->
            """
            vec2 iinverse(vec2 y) {
                if(y.x <= 0.0 && y.y >= 0.0)
                    return vec2(-1.0 / 0.0, 1.0 / 0.0);
                if(y.y == 0.0)
                    return vec2(-1.0 / 0.0, 1.0 / y.x);
                if(y.x == 0.0)
                    return vec2(1.0 / y.y, 1.0 / 0.0);
                return vec2(1.0 / y.y, 1.0 / y.x);
            }

            vec2 idiv(vec2 l, vec2 r) {
                return iby(l, iinverse(r));
            }

            vec4 gdiv(vec4 l, vec4 r) {
                return vec4(
                    l.x / r.x,
                    (r.x * l.yzw - l.x * r.yzw) / pow(r.x, 2.0)
                );
            }
            """

        GlslPower ->
            """
            float ipow(float b, int e) {
                float fe = float(e);
                if(mod(fe, 2.0) == 0.0)
                    return pow(abs(b), fe);
                else
                    return b * pow(abs(b), fe - 1.0);
            }

            vec2 ipow(vec2 b, int e) {
                if(e == 0)
                    return vec2(1,1);
                if(e == 1)
                    return b;
                float xe = ipow(b.x, e);
                float ye = ipow(b.y, e);
                float mn = min(xe, ye);
                float mx = max(xe, ye);
                if(mod(float(e), 2.0) == 0.0 && b.x <= 0.0 && b.y >= 0.0) {
                    return vec2(
                        min(0.0, mn),
                        max(0.0, mx)
                    );
                }
                return vec2(mn, mx);
            }

            vec2 ipow(vec2 b, vec2 e) {
                return iexp(iby(iln(b), e));
            }

            vec4 gpow(vec4 b, int e) {
                return vec4(pow(b.x, float(e)), float(e) * pow(b.x, float(e - 1)) * b.yzw);
            }

            vec4 gpow(vec4 b, vec4 e) {
                int ie = int(e.x);
                if(float(ie) == e.x && e.y == 0.0 && e.z == 0.0 && e.w == 0.0) {
                    return gpow(b, ie);
                }
                return gexp(gby(gln(b), e));
            }
            """

        GlslRelations ->
            """
            vec2 ilt(vec2 l, vec2 r) {
                return vec2(r.x - l.y, r.y - l.x);
            }

            vec2 ileq(vec2 l, vec2 r) {
                return vec2(r.x - l.y, r.y - l.x);
            }

            vec2 ieq(vec2 l, vec2 r) {
                return vec2(l.x - r.y, l.y - r.x);
            }

            vec2 igeq(vec2 l, vec2 r) {
                return vec2(l.x - r.y, l.y - r.x);
            }

            vec2 igt(vec2 l, vec2 r) {
                return vec2(l.x - r.y, l.y - r.x);
            }
            """


straightFunctionToGlsl : GlslFunction -> String
straightFunctionToGlsl name =
    case name of
        Sinh11 ->
            """
            float sinh(float x) {
                return 0.5 * (exp(x) - exp(-x));
            }
            """

        Cosh11 ->
            """
            float cosh(float x) {
                return 0.5 * (exp(x) + exp(-x));
            }
            """

        Tanh11 ->
            """
            float tanh(float x) {
                if(abs(x) > 10.0)
                    return sign(x);
                float p = exp(x);
                float m = exp(-x);
                return (p - m) / (p + m);
            }
            """

        Sin22 ->
            """
            vec2 csin(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(sin(z.x), 0);
                }
                return vec2(sin(z.x) * cosh(z.y), cos(z.x) * sinh(z.y));
            }
            """

        Cos22 ->
            """
            vec2 ccos(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(cos(z.x), 0);
                }
                return vec2(cos(z.x) * cosh(z.y), sin(z.x) * sinh(z.y));
            }
            """

        Tan22 ->
            """
            vec2 ctan(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(tan(z.x), 0);
                }
                return div(csin(z), ccos(z));
            }
            """

        Asin22 ->
            """
            vec2 casin(vec2 z) {
                vec2 s = csqrt(vec2(1, 0) - by(z, z));
                vec2 arg = s - by(vec2(0, 1), z);
                return by(vec2(0, 1), cln(arg));
            }
            """

        Acos22 ->
            """
            vec2 cacos(vec2 z) {
                return pi() * 0.5 - casin(z);
            }
            """

        Atan22 ->
            """
            vec2 catan(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(atan(z.x), 0);
                }
                vec2 o = vec2(1, 0);
                vec2 iz = by(vec2(0, 1), z);
                vec2 l = div(o + iz, o - iz);
                return -0.5 * by(vec2(0, 1), cln(l));
            }
            """

        Atan222 ->
            """
            vec2 catan2(vec2 y, vec2 x) {
                vec2 z = vec2(x.x - y.y, x.y + y.x);
                return vec2(atan(z.y, z.x), 0.0);
            }
            """

        Sinh22 ->
            """
            vec2 csinh(vec2 z) {
                return 0.5 * (cexp(z) - cexp(-z));
            }
            """

        Cosh22 ->
            """
            vec2 ccosh(vec2 z) {
                return 0.5 * (cexp(z) + cexp(-z));
            }
            """

        Tanh22 ->
            """
            vec2 ctanh(vec2 z) {
                vec2 p = cexp(z);
                vec2 m = cexp(-z);
                return div(p - m, p + m);
            }
            """

        Abs22 ->
            """
            vec2 cabs(vec2 z) {
                return vec2(length(z), 0.0);
            }
            """

        Sign22 ->
            """
            vec2 csign(vec2 z) {
                return vec2(sign(z.x), sign(z.y));
            }
            """

        Sqrt22 ->
            """
            vec2 csqrt(vec2 z) {
                if(z.y == 0.0 && z.x >= 0.0) {
                    return vec2(sqrt(z.x), 0);
                }
                float r = pow(dot(z, z), 0.25);
                float t = atan(z.y, z.x) * 0.5;
                return r * vec2(cos(t), sin(t));
            }
            """

        Cbrt22 ->
            """
            vec2 ccbrt(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(sign(z.x) * pow(z.x, 1.0 / 3.0), 0);
                }
                float r = pow(dot(z, z), 1.0 / 6.0);
                float t = atan(z.y, z.x) / 3.0 + (z.x > 0.0 ? 0.0 : radians(120.0));
                return r * vec2(cos(t), sin(t));
            }
            """

        Square22 ->
            """
            vec2 csquare(vec2 z) {
                return by(z, z);
            }
            """

        Ln22 ->
            """
            vec2 cln(vec2 z) {
                if(z.y == 0.0 && z.x >= 0.0) {
                    return vec2(log(z.x), 0);
                }
                float px = length(z);
                float py = atan(z.y, z.x);
                return vec2(log(px), py);
            }
            """

        Log1022 ->
            """
            vec2 clog10(vec2 z) {
                return div(cln(z), vec2(log(10.0), 0));
            }
            """

        Exp22 ->
            """
            vec2 cexp(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(exp(z.x), 0);
                }
                return vec2(cos(z.y) * exp(z.x), sin(z.y) * exp(z.x));
            }
            """

        Re22 ->
            """
            vec2 cre(vec2 z) {
                return vec2(z.x, 0.0);
            }
            """

        Im22 ->
            """
            vec2 cim(vec2 z) {
                return vec2(z.y, 0.0);
            }
            """

        Arg22 ->
            """
            vec2 carg(vec2 v) {
                return vec2(atan(v.y, v.x), 0);
            }
            """

        Pw22 ->
            """
            vec2 cpw(vec2 c, vec2 t, vec2 f) {
                return c.x > 0.0 ? t : f;
            }
            """

        Ceiling22 ->
            """
            vec2 cceiling(vec2 z) {
                return ceil(z);
            }
            """

        Floor22 ->
            """
            vec2 cfloor(vec2 z) {
                return floor(z);
            }
            """

        Round22 ->
            """
            vec2 cround(vec2 z) {
                return floor(z + vec2(0.5, 0.5));
            }
            """

        Min222 ->
            """
            vec2 cmin(vec2 l, vec2 r) {
                return l.x < r.x ? l : r;
            }
            """

        Max222 ->
            """
            vec2 cmax(vec2 l, vec2 r) {
                return l.x > r.x ? l : r;
            }
            """

        Mod22 ->
            """
            vec2 cmod(vec2 l, vec2 r) {
                return vec2(mod(l.x, r.x), 0);
            }
            """


intervalFunctionToGlsl : GlslFunction -> String
intervalFunctionToGlsl name =
    case name of
        Abs22 ->
            """
            vec2 iabs(vec2 z) {
                if(z.x <= 0.0 && z.y >= 0.0)
                    return vec2(0.0, max(z.y, abs(z.x)));
                if(z.x <= 0.0)
                    return vec2(-z.y, -z.x);
                return z;
            }

            vec4 gabs(vec4 v) {
                return vec4(abs(v.x), sign(v.x) * v.yzw);
            }
            """

        Acos22 ->
            """
            TODO Acos22
            """

        Arg22 ->
            """
            vec2 iarg(vec2 z) {
                if(z.y < 0.0)
                    return dup(radians(180.0));
                if(z.x >= 0.0)
                    return vec2(0);
                return vec2(0.0, radians(180.0));
            }

            vec4 garg(vec4 z) {
                return gnum(z.x >= 0.0 ? 0.0 : radians(180.0));
            }
            """

        Sign22 ->
            """
            vec2 isign(vec2 z) {
                return sign(z);
            }

            vec4 gsign(vec4 v) {
                return gnum(sign(v.x));
            }
            """

        Asin22 ->
            """
            TODO Asin22
            """

        Atan22 ->
            """
            TODO Atan22
            """

        Atan222 ->
            """
            TODO Atan222
            """

        Ceiling22 ->
            """
            vec2 iceiling(vec2 z) {
                return ceil(z);
            }

            vec4 gceiling(vec4 z) {
                return gnum(ceil(z.x));
            }
            """

        Cos22 ->
            """
            vec2 icos(vec2 v) {
                vec2 shift = radians(90.0) * vec2(1,1);
                return isin(v + shift);
            }

            vec4 gcos(vec4 v) {
                return vec4(cos(v.x), -sin(v.x) * v.yzw);
            }
            """

        Cosh11 ->
            ""

        Cosh22 ->
            """
            vec2 icosh(vec2 z) {
                if(z.x <= 0.0 && z.y >= 0.0)
                    return vec2(cosh(0.0), max(z.y, abs(z.x)));
                if(z.x <= 0.0)
                    return vec2(cosh(-z.y), cosh(-z.x));
                return vec2(cosh(z.x), cosh(z.y));
            }

            vec4 gcosh(vec4 v) {
                return vec4(cosh(v.x), sinh(v.x) * v.yzw);
            }
            """

        Exp22 ->
            """
            vec2 iexp(vec2 z) {
                return vec2(exp(z.x), exp(z.y));
            }

            vec4 gexp(vec4 z) {
                return vec4(
                    exp(z.x),
                    exp(z.x) * z.yzw
                );
            }
            """

        Floor22 ->
            """
            vec2 ifloor(vec2 z) {
                return floor(z);
            }

            vec4 gfloor(vec4 z) {
                return gnum(floor(z.x));
            }
            """

        Ln22 ->
            """
            vec2 iln(vec2 z) {
                return vec2(log(z.x), log(z.y));
            }

            vec4 gln(vec4 z) {
                return vec4(log(z.x), z.yzw / z.x);
            }
            """

        Log1022 ->
            """
            TODO Log1022
            """

        Pw22 ->
            """
            vec2 ipw(vec2 c, vec2 t, vec2 f) {
                if(c.y <= 0.0)
                    return f;
                if(c.x > 0.0)
                    return t;
                return vec2(min(t.x, f.x), max(t.y, f.y));
            }

            vec4 gpw(vec4 c, vec4 t, vec4 f) {
                return c.x > 0.0 ? t : f;
            }
            """

        Re22 ->
            """
            vec2 ire(vec2 v) {
                return v;
            }

            vec4 gre(vec4 v) {
                return v;
            }
            """

        Im22 ->
            """
            vec2 iim(vec2 v) {
                return vec2(0);
            }

            vec4 gim(vec4 v) {
                return vec4(0, 0, 0, 1.0);
            }
            """

        Round22 ->
            """
            TODO Round22
            """

        Sin22 ->
            """
            bool between(float x, float low, float high) {
                return low <= x && x <= high;
            }

            vec2 isin(vec2 v) {
                if(v.y - v.x > radians(360.0)) {
                    return vec2(-1.0, 1.0);
                }
                float from = mod(v.x, radians(360.0)); // [0, 360°]
                float to = from + v.y - v.x; // [0, 720°]
                vec2 s = sin(vec2(from, to));
                vec2 res = vec2(min(s.x, s.y), max(s.x, s.y));
                if(between(radians(90.0), from, to) || between(radians(90.0 + 360.0), from, to))
                    res.y = 1.0;
                if(between(radians(270.0), from, to) || between(radians(270.0 + 360.0), from, to))
                    res.x = -1.0;
                return res;
            }

            vec4 gsin(vec4 v) {
                return vec4(sin(v.x), cos(v.x) * v.yzw);
            }
            """

        Sinh11 ->
            ""

        Sinh22 ->
            """
            vec2 isinh(vec2 v) {
                return vec2(sinh(v.x), sinh(v.y));
            }

            vec4 gsinh(vec4 v) {
                return vec4(sinh(v.x), cosh(v.x) * v.yzw);
            }
            """

        Sqrt22 ->
            """
            vec2 isqrt(vec2 v) {
                return vec2(sqrt(max(0.0, v.x)), sqrt(max(0.0, v.y)));
            }

            vec4 gsqrt(vec4 v) {
                return vec4(sqrt(v.x), 0.5 * pow(v.x, -0.5) * v.yzw);
            }
            """

        Cbrt22 ->
            """
            float cbrt(float v) {
                return sign(v) * pow(abs(v), 1.0 / 3.0);
            }

            vec2 icbrt(vec2 v) {
                return vec2(cbrt(v.x), cbrt(v.y));
            }

            vec4 gcbrt(vec4 v) {
                return vec4(cbrt(v.x), pow(v.x, -2.0 / 3.0) / 3.0 * v.yzw);
            }
            """

        Square22 ->
            """
            vec2 isquare(vec2 z) {
                vec2 s = z * z;
                float mx = max(s.x, s.y);
                if(z.x <= 0.0 && z.y >= 0.0)
                    return vec2(0.0, mx);
                float mn = min(s.x, s.y);
                return vec2(mn, mx);
            }

            vec4 gsquare(vec4 z) {
                return vec4(z.x*z.x, 2.0 * z.x * z.yzw);
            }
            """

        Tan22 ->
            """
            vec2 itan(vec2 z) {
                return idiv(isin(z), icos(z));
            }

            vec4 gtan(vec4 z) {
                float c = cos(z.x);
                return vec4(tan(z.x), 1.0 / (c * c) * z.yzw);
            }
            """

        Tanh11 ->
            """
            """

        Tanh22 ->
            """
            vec2 itanh(vec2 z) {
                return vec2(tanh(z.x), tanh(z.y));
            }

            vec4 gtanh(vec4 z) {
                float c = 1.0 / cosh(z.x);
                return vec4(tanh(z.x), c * c * z.yzw);
            }
            """

        Min222 ->
            """
            vec2 imin(vec2 l, vec2 r) {
                return vec2(min(l.x, r.x), min(l.y, r.y));
            }

            vec4 gmin(vec4 l, vec4 r) {
                return l.x < r.x ? l : r;
            }
            """

        Max222 ->
            """
            vec2 imax(vec2 l, vec2 r) {
                return vec2(max(l.x, r.x), max(l.y, r.y));
            }

            vec4 gmax(vec4 l, vec4 r) {
                return l.x > r.x ? l : r;
            }
            """

        Mod22 ->
            """
            vec2 imod(vec2 x, vec2 y) {
                return x + ineg(iby(y, ifloor(idiv(x, y))));
            }
            
            vec4 gmod(vec4 l, vec4 r) {
                return vec4(mod(l.x, r.x), l.yzw);
            }
            """


toSrcImplicit : String -> Expression -> String
toSrcImplicit suffix e =
    """
    float f""" ++ suffix ++ """(float x, float y) {
        vec2 complex = """ ++ expressionToGlsl e ++ """;
        if(abs(complex.y) > """ ++ floatToGlsl epsilon ++ """) {
            return -1.0;
        }
        return complex.x > 0.0 ? 1.0 : 0.0;
    }

    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        x += deltaX / 2.0;
        y += deltaY / 2.0;
        float h = f""" ++ suffix ++ """(x,y);
        float l = f""" ++ suffix ++ """(x - deltaX,y);
        float ul = f""" ++ suffix ++ """(x - deltaX,y - deltaY);
        float u = f""" ++ suffix ++ """(x,y - deltaY);
        return (h != l || h != u || h != ul)
            && (h >= 0.0 && l >= 0.0 && ul >= 0.0 && u >= 0.0)
                ? vec3(1,1,1) : vec3(0,0,0);
    }
    """


toSrcPolar : String -> Expression -> String
toSrcPolar suffix e =
    """
    float f""" ++ suffix ++ """(float x, float y, float deltaT, float ot) {
        float r = sqrt(x*x + y*y);
        float t = atanPlus(y, x) + deltaT;
        // Avoid the branch cut at {x > 0, y = 0}
        if(abs(t - ot) > radians(180.0)) {
            if(t < ot) t += radians(360.0);
            else t -= radians(360.0);
        }
        vec2 complex = """ ++ expressionToGlsl e ++ """;
        if(abs(complex.y) > """ ++ floatToGlsl epsilon ++ """) {
            return -1.0;
        }
        return complex.x > 0.0 ? 1.0 : 0.0;
    }

    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        float t = 0.0;
        x += deltaX / 2.0;
        y += deltaY / 2.0;
        float ot = atanPlus(y, x);

        for(int i = 0; i < MAX_ITERATIONS / 10; i++) {
            float h = f""" ++ suffix ++ """(x, y, t, ot);
            float l = f""" ++ suffix ++ """(x - deltaX, y, t, ot);
            float u = f""" ++ suffix ++ """(x, y - deltaY, t, ot);
            float ul = f""" ++ suffix ++ """(x - deltaX, y - deltaY, t, ot);
            if(h < 0.0 || l < 0.0 || u < 0.0 || ul < 0.0)
                break;
            if(h != l || h != u || h != ul)
                return vec3(1,1,1);
            t += radians(360.0);
            ot += radians(360.0);
        }
        return vec3(0,0,0);
    }
    """


toSrcParametric : Bool -> String -> Expression -> String
toSrcParametric expandIntervals suffix e =
    """
    vec2 interval""" ++ suffix ++ """(vec2 p, float from, float to) {
        vec2 x = vec2(p.x,p.x);
        vec2 y = vec2(p.y,p.y);
        vec2 t = from < to ? vec2(from, to) : vec2(to, from);
        return """ ++ expressionToIntervalGlsl expandIntervals e ++ """;
    }

    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        float max_distance = pow(2.0, 10.0);
        float from = -max_distance / 2.0;
        float to = max_distance / 2.0;
        vec2 p = vec2(x, y);
        int depth = 0;
        int choices = 0;
        float ithreshold = 10.0 * deltaX * deltaX;
        for(int it = 0; it < MAX_ITERATIONS; it++) {
            float midpoint = mix(from, to, 0.5);
            vec2 front = interval""" ++ suffix ++ """(p, from, midpoint);
            vec2 back = interval""" ++ suffix ++ """(p, midpoint, to);
            if(depth >= MAX_DEPTH
                || (front.y - front.x < ithreshold && front.x <= 0.0 && front.y >= 0.0)
                || (back.y - back.x < ithreshold && back.x <= 0.0 && back.y >= 0.0)
                )
                    return vec3(1,1,1);
            if(front.x <= 0.0 && front.y >= 0.0) {
                to = midpoint;
                depth++;
                choices *= 2;
            } else if(back.x <= 0.0 && back.y >= 0.0) {
                from = midpoint;
                depth++;
                choices = choices * 2 + 1;
            } else {
                // This could be possibly helped by https://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightBinSearch
                for(int j = MAX_DEPTH - 1; j > 0; j--) {
                    if(j > depth)
                        continue;
                    depth--;
                    choices /= 2;
                    if(choices / 2 * 2 == choices) {
                        midpoint = to;
                        to = to + (to - from);
                        vec2 back = interval""" ++ suffix ++ """(p, midpoint, to);
                        if(back.x <= 0.0 && back.y >= 0.0) {
                            from = midpoint;
                            depth++;
                            choices = choices * 2 + 1;
                            break;
                        }
                    } else {
                        from = from - (to - from);
                    }
                }
                if(depth == 0)
                    return vec3(0,0,0);
            }
        }
        return vec3(0,0,0);
    }
    """


thetaDelta : String
thetaDelta =
    """
    float thetaDelta(float theta) {
        if(u_whiteLines < 1.0)
            return 100.0;
        float thetaSix = theta * u_whiteLines + 0.5;
        float thetaNeigh = 0.05;
        return abs(fract(thetaSix) - 0.5) / thetaNeigh;
    }
    """


toSrcContour : String -> Expression -> String
toSrcContour suffix e =
    """
    vec3 pixel""" ++ suffix ++ """_o(float deltaX, float deltaY, float x, float y) {
        vec2 z = """ ++ expressionToGlsl e ++ """;

        float theta = atan(z.y, z.x) / radians(360.0);

        float logRadius = log2(length(z));
        float powerRemainder = fract(logRadius);
        float squished = 0.7 - powerRemainder * 0.4;

        if(u_completelyReal > 0.0) {
            float haf = fract(logRadius) / 6.0;
            if(z.x > 0.0)
                return hl2rgb(haf + 0.3, squished);
            else
                return hl2rgb(haf - 0.1, 1.0 - squished);
        }

        float td = thetaDelta(theta);
        float l = mix(1.0, squished, smoothstep(0.0, 1.0, td));
        return hl2rgb(theta, l);
    }

    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        // Antialiasing

        float dist = 1.0 / 3.0;
        vec3 a = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x + dist * deltaX, y + dist * deltaY);
        vec3 b = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x - dist * deltaX, y - dist * deltaY);
        vec3 c = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x + dist * deltaX, y - dist * deltaY);
        vec3 d = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x - dist * deltaX, y + dist * deltaY);

        vec3 diff = abs(max(a, max(b, max(c, d))) - min(a, min(b, min(c, d))));
        if (diff.x < dist && diff.y < dist && diff.z < dist)
            return (a + b + c + d) / 4.0;

        vec3 e = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x + 2.0 * dist * deltaX, y);
        vec3 f = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x - 2.0 * dist * deltaX, y);
        vec3 g = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x, y - 2.0 * dist * deltaY);
        vec3 h = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x, y + 2.0 * dist * deltaY);

        return (a + b + c + d + e + f + g + h) / 8.0;
    }
    """


toSrcRelation : String -> Expression -> String
toSrcRelation suffix e =
    """
    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        vec2 complex = """ ++ expressionToGlsl e ++ """;
        return complex.x > 0.0 && abs(complex.y) < """ ++ floatToGlsl epsilon ++ """ ? vec3(0.8,0.5,0.5) : vec3(0,0,0);
    }
    """


epsilon : Float
epsilon =
    0.00001


wordWrap : String -> String
wordWrap =
    let
        step e ( l, acc ) =
            if String.length e + String.length l > 40 then
                ( e, l :: acc )

            else
                ( l ++ e, acc )
    in
    String.split " "
        >> List.foldl step ( "", [] )
        >> (\( l, a ) ->
                if String.isEmpty l then
                    a

                else
                    l :: a
           )
        >> List.reverse
        >> String.join "\n            "


expressionToGlsl : Expression -> String
expressionToGlsl =
    Expression.toPrintExpression
        >> expressionToGlslPrec 0
        >> wordWrap


expressionToIntervalGlsl : Bool -> Expression -> String
expressionToIntervalGlsl expandIntervals =
    Expression.toPrintExpression
        >> expressionToIntervalGlslPrec expandIntervals 0
        >> wordWrap


expressionToNormalGlsl : Expression -> String
expressionToNormalGlsl =
    Expression.toPrintExpression
        >> expressionToNormalGlslPrec 0
        >> wordWrap


expressionToGlslPrec : Int -> PrintExpression -> String
expressionToGlslPrec p expr =
    let
        noninfix op c =
            paren (p > 10) <| op ++ expressionToGlslPrec 11 c

        infixl_ n op l r =
            paren (p > n) <| expressionToGlslPrec n l ++ op ++ expressionToGlslPrec (n + 1) r

        apply name ex =
            paren (p > 10) <| name ++ "(" ++ String.join ", " (List.map (expressionToGlslPrec 0) ex) ++ ")"
    in
    case expr of
        PVariable "i" ->
            "vec2(0,1)"

        PVariable "pi" ->
            "vec2(radians(180.0),0.0)"

        PVariable "e" ->
            "vec2(exp(1.0),0)"

        PVariable v ->
            "vec2(" ++ v ++ ",0)"

        PInteger v ->
            "vec2(" ++ String.fromInt v ++ ",0)"

        PFloat f ->
            "vec2(" ++ floatToGlsl f ++ ",0)"

        PNegate expression ->
            "(" ++ noninfix "-" expression ++ ")"

        PAdd l (PNegate r) ->
            infixl_ 6 " - " l r

        PAdd l r ->
            infixl_ 6 " + " l r

        PRel op l r ->
            if op == "<=" || op == "<" then
                infixl_ 6 " - " r l

            else if op == "=" then
                "(abs(" ++ infixl_ 6 " - " r l ++ "))"

            else
                infixl_ 6 " - " l r

        PBy l r ->
            apply "by" [ l, r ]

        PDiv l r ->
            apply "div" [ l, r ]

        PPower (PVariable "i") r ->
            apply "cpow" [ PVariable "i", r ]

        PPower (PVariable v) (PInteger 2) ->
            "vec2(" ++ v ++ "*" ++ v ++ ",0)"

        PPower l r ->
            apply "cpow" [ l, r ]

        PApply (KnownFunction Simplify) [ e ] ->
            expressionToGlslPrec p e

        PApply name ex ->
            if List.any (\( _, v ) -> name == KnownFunction v) Expression.variadicFunctions then
                case List.map (expressionToGlslPrec 0) ex of
                    [] ->
                        "vec2(0)"

                    head :: tail ->
                        List.foldl (\e a -> "c" ++ Expression.functionNameToString name ++ "(" ++ a ++ "," ++ e ++ ")") head tail

            else
                apply ("c" ++ Expression.functionNameToString name) ex

        PList es ->
            "vec" ++ String.fromInt (List.length es) ++ "(" ++ String.join ", " (List.map (expressionToGlslPrec 0) es) ++ ")"

        PReplace var e ->
            expressionToGlslPrec p (Expression.pfullSubstitute var e)

        -- If this happens, it's too late
        PLambda _ _ ->
            "vec2(0)"


paren : Bool -> String -> String
paren b c =
    if b then
        "(" ++ c ++ ")"

    else
        c


expressionToIntervalGlslPrec : Bool -> Int -> PrintExpression -> String
expressionToIntervalGlslPrec expandIntervals p expr =
    let
        infixl_ n op l r =
            paren (p > n) <| expressionToIntervalGlslPrec expandIntervals n l ++ op ++ expressionToIntervalGlslPrec expandIntervals (n + 1) r

        apply name ex =
            paren (p > 10) <| name ++ "(" ++ String.join ", " (List.map (expressionToIntervalGlslPrec expandIntervals 0) ex) ++ ")"

        expand e =
            if expandIntervals then
                "iexpand(" ++ e ++ ")"

            else
                e
    in
    case expr of
        PVariable "pi" ->
            "dup(radians(180.0))"

        PVariable "e" ->
            "dup(exp(1.0))"

        PVariable v ->
            v

        PInteger v ->
            "dup(" ++ String.fromInt v ++ ".0)"

        PFloat f ->
            "dup(" ++ floatToGlsl f ++ ")"

        PNegate expression ->
            "ineg(" ++ expressionToIntervalGlslPrec expandIntervals 10 expression ++ ")"

        PAdd l r ->
            expand <| infixl_ 6 " + " l r

        PRel op l r ->
            let
                name =
                    case op of
                        "<" ->
                            "ilt"

                        "<=" ->
                            "ileq"

                        "=" ->
                            "ieq"

                        ">=" ->
                            "igeq"

                        ">" ->
                            "igt"

                        _ ->
                            "iuknownrelop"
            in
            expand <| name ++ "(" ++ expressionToIntervalGlslPrec expandIntervals 10 l ++ ", " ++ expressionToIntervalGlslPrec expandIntervals 10 r ++ ")"

        PBy l r ->
            expand <| apply "iby" [ l, r ]

        PDiv l r ->
            expand <| apply "idiv" [ l, r ]

        PPower (PVariable "i") (PInteger 2) ->
            "dup(-1.0)"

        PPower l (PInteger 2) ->
            expand <| "isquare(" ++ expressionToIntervalGlslPrec expandIntervals 0 l ++ ")"

        PPower l (PInteger ri) ->
            let
                ris =
                    String.fromInt ri
            in
            expand <| "ipow(" ++ expressionToIntervalGlslPrec expandIntervals 0 l ++ ", " ++ ris ++ ")"

        PPower l r ->
            expand <| apply "ipow" [ l, r ]

        PApply name ex ->
            if List.any (\( _, v ) -> name == KnownFunction v) Expression.variadicFunctions then
                case List.map (expressionToIntervalGlslPrec expandIntervals 0) ex of
                    [] ->
                        "vec2(0)"

                    head :: tail ->
                        expand <| List.foldl (\e a -> "i" ++ Expression.functionNameToString name ++ "(" ++ a ++ "," ++ e ++ ")") head tail

            else if name == KnownFunction Sin || name == KnownFunction Cos then
                apply ("i" ++ Expression.functionNameToString name) ex

            else
                expand <| apply ("i" ++ Expression.functionNameToString name) ex

        PList es ->
            "vec" ++ String.fromInt (List.length es) ++ "(" ++ String.join ", " (List.map (expressionToIntervalGlslPrec expandIntervals 0) es) ++ ")"

        PReplace var e ->
            expressionToIntervalGlslPrec expandIntervals p (Expression.pfullSubstitute var e)

        -- If this happens, it's too late
        PLambda _ _ ->
            "vec2(0)"


expressionToNormalGlslPrec : Int -> PrintExpression -> String
expressionToNormalGlslPrec p expr =
    let
        infixl_ n op l r =
            paren (p > n) <| expressionToNormalGlslPrec n l ++ op ++ expressionToNormalGlslPrec (n + 1) r

        apply name ex =
            paren (p > 10) <| name ++ "(" ++ String.join ", " (List.map (expressionToNormalGlslPrec 0) ex) ++ ")"
    in
    case expr of
        PVariable "pi" ->
            "gnum(radians(180.0))"

        PVariable "e" ->
            "gnum(exp(1.0))"

        PVariable "x" ->
            "vec4(x,1,0,0)"

        PVariable "y" ->
            "vec4(y,0,1,0)"

        PVariable "z" ->
            "vec4(z,0,0,1)"

        PVariable v ->
            v

        PInteger v ->
            "gnum(" ++ String.fromInt v ++ ".0)"

        PFloat f ->
            "gnum(" ++ floatToGlsl f ++ ")"

        PNegate expression ->
            "gneg(" ++ expressionToNormalGlslPrec 10 expression ++ ")"

        PAdd l r ->
            infixl_ 6 " + " l r

        PRel op l r ->
            if op == "<=" || op == "<" then
                infixl_ 6 " - " r l

            else if op == "=" then
                "(abs(" ++ infixl_ 6 " - " r l ++ "))"

            else
                infixl_ 6 " - " l r

        PBy l r ->
            apply "gby" [ l, r ]

        PDiv l r ->
            apply "gdiv" [ l, r ]

        PPower l (PInteger 2) ->
            apply "gsquare" [ l ]

        PPower l (PInteger ri) ->
            "gpow(" ++ expressionToNormalGlslPrec 0 l ++ ", " ++ String.fromInt ri ++ ")"

        PPower l r ->
            apply "gpow" [ l, r ]

        PApply name ex ->
            if List.any (\( _, v ) -> name == KnownFunction v) Expression.variadicFunctions then
                case List.map (expressionToNormalGlslPrec 0) ex of
                    [] ->
                        "vec2(0)"

                    head :: tail ->
                        List.foldl (\e a -> "g" ++ Expression.functionNameToString name ++ "(" ++ a ++ "," ++ e ++ ")") head tail

            else
                apply ("g" ++ Expression.functionNameToString name) ex

        PList es ->
            "vec" ++ String.fromInt (List.length es) ++ "(" ++ String.join ", " (List.map (expressionToNormalGlslPrec 0) es) ++ ")"

        PReplace var e ->
            expressionToNormalGlslPrec p (Expression.pfullSubstitute var e)

        -- If this happens, it's too late
        PLambda _ _ ->
            "vec4(0)"


floatToGlsl : Float -> String
floatToGlsl f =
    let
        s =
            String.fromFloat f
    in
    if String.contains "." s || String.contains "e" s then
        s

    else
        s ++ ".0"


mainGlsl : Bool -> List { name : String, color : Bool } -> List String -> String
mainGlsl rayDifferentials pixel2 pixel3 =
    case ( pixel2, pixel3 ) of
        ( _, [] ) ->
            main2D pixel2 ++ "\n\nvoid main () { gl_FragColor = pixel2(); }"

        ( [], _ ) ->
            main3D rayDifferentials pixel3 ++ "\n\nvoid main () { gl_FragColor = pixel3(); }"

        _ ->
            main2D pixel2 ++ "\n\n" ++ main3D rayDifferentials pixel3 ++ "\n\nvoid main () { gl_FragColor = max(pixel2(), pixel3()); }"


main2D : List { name : String, color : Bool } -> String
main2D pixels =
    let
        addPixel i { name, color } =
            let
                k =
                    if color then
                        "hl2rgb(" ++ h ++ ", 0.5)" ++ " * " ++ name

                    else
                        name

                h =
                    floatToGlsl <| (toFloat (i + 2) / pi)
            in
            """
            curr = """ ++ k ++ """(deltaX, deltaY, x, y);
            px = curr == vec3(0,0,0) ? px : curr;"""

        inner =
            pixels
                |> List.indexedMap addPixel
                |> String.concat
    in
    deindent 8 <|
        """
        float log10(float x) {
            return log(x) / log(10.0);
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

        vec4 pixel2 () {
            vec2 canvasSize = vec2(u_canvasWidth, u_canvasHeight);
            vec2 uv_centered = gl_FragCoord.xy - 0.5 * canvasSize;

            vec2 viewportSize = (u_viewportWidth / u_canvasWidth) * canvasSize;
            vec2 uv = uv_centered / canvasSize * viewportSize;
            vec2 c = u_zoomCenter + uv;
            float x = c.x;
            float y = c.y;

            float deltaX = u_viewportWidth / u_canvasWidth;
            float deltaY = u_viewportWidth / u_canvasHeight;
            vec3 px = vec3(0,0,0);
            vec3 curr;"""
            ++ inner
            ++ """
            float maxDelta = max(deltaX, deltaY);
            vec3 yax = axis(x, y, maxDelta) * vec3(0,1,0);
            vec3 xax = axis(y, x, maxDelta) * vec3(1,0,0);
            return vec4(max(px, max(xax, yax)), 1.0);
        }
        """


toSrc3D : Bool -> String -> Expression -> String
toSrc3D expandIntervals suffix e =
    """
    vec3 normal""" ++ suffix ++ """(vec3 p) {
        float x = p.x;
        float y = p.y;
        float z = p.z;
        vec4 gradient = """ ++ expressionToNormalGlsl e ++ """;
        return normalize(gradient.yzw);
    }

    vec2 interval""" ++ suffix ++ """(mat2x3 f, mat2x3 t) {
        vec3 mn = min(f[0], t[0]);
        vec3 mx = max(f[1], t[1]);
        vec2 x = vec2(mn.x, mx.x);
        vec2 y = vec2(mn.y, mx.y);
        vec2 z = vec2(mn.z, mx.z);
        return """ ++ expressionToIntervalGlsl expandIntervals e ++ """;
    }
    """


main3D : Bool -> List String -> String
main3D rayDifferentials suffixes =
    let
        iif b t f =
            if b then
                t

            else
                f

        block =
            """
            vec4 pixel3 () {
                float eye_dist = 2.0 * u_viewportWidth;
                vec2 canvasSize = vec2(u_canvasWidth, u_canvasHeight);
                vec2 uv_centered = gl_FragCoord.xy - 0.5 * canvasSize;
                vec2 uv_normalized = 1.0 / u_canvasHeight * uv_centered;

                float t = u_theta + 0.58;
                float p = -2.0 * u_phi;
                vec3 eye = eye_dist * normalize(vec3(
                    cos(t) * sin(p),
                    cos(t) * -cos(p),
                    sin(t)
                ));

                vec3 target = vec3(0);
                vec3 to_target = normalize(target-eye);
                vec3 across = normalize(cross(to_target, vec3(0,0,1)));
                vec3 up = normalize(cross(across, to_target));

                vec3 canvas_center = eye + to_target;
                vec3 canvas_point = canvas_center + uv_normalized.x * across + uv_normalized.y * up;

                vec3 ray_direction = normalize(canvas_point - eye);

                vec3 diffs = abs(fwidth(ray_direction));
                float k = """ ++ iif rayDifferentials "0.001" "0.0" ++ """;
                mat2x3 d = mat2x3(ray_direction - k * diffs, ray_direction + k * diffs);

                float max_distance = 100.0 * eye_dist;
                return raytrace(canvas_point, d, max_distance);
            }
        """
    in
    deindent 12 <| raytrace suffixes ++ block


raytrace : List String -> String
raytrace suffixes =
    let
        colorCoeff =
            floatToGlsl (1.19 - 0.2 * toFloat (List.length suffixes))

        innerTrace =
            String.join "\n" (List.indexedMap suffixToRay suffixes)

        innerLightTrace =
            String.join "\n" (List.map suffixToRayLight suffixes)

        suffixToRay i suffix =
            """
                if(bisect""" ++ suffix ++ """(o, d, max_distance, f) && length(f - o) < curr_distance) {
                    found_index = """ ++ String.fromInt i ++ """;
                    found = f;
                    curr_distance = length(found - o);
                }
            """

        suffixToRayLight suffix =
            """
                    else if(bisect""" ++ suffix ++ """(offseted, light_direction, light_distance, f)) {
                        light_coeff = 0.2;
                    }
            """
    in
    """
            vec4 raytrace(vec3 o, mat2x3 d, float max_distance) {
                vec3 found = vec3(0);
                float curr_distance = max_distance;
                int found_index = -1;
                vec3 f = vec3(0);
                vec3 n = vec3(0);
                """ ++ innerTrace ++ """
                if(found_index >= 0) {
                    float hue_based_on_index = (float(found_index))*radians(360.0 / 1.1);

                    vec3 ld = normalize(vec3(-0.3, 0.0, 1.0));
                    vec3 diffs = d[1] - d[0];
                    mat2x3 light_direction = mat2x3(ld - 0.5 * diffs, ld + 0.5 * diffs);
                    float light_distance = max_distance;
                    float light_coeff = 0.45;
                    vec3 offseted = found + 0.0001 * max_distance * normalize(o - found);
                    if(0 == 1) { }
                    """ ++ innerLightTrace ++ """

                    vec3 px = mix(
                        hl2rgb(hue_based_on_index, light_coeff),
                        hl2rgb(found.z * 0.5, light_coeff),
                        max(0.2, """ ++ colorCoeff ++ """)
                    );
                    return vec4(px, 1.0);
                } else {
                    return vec4(0.0, 0.0, 0.0, 1.0);
                }
            }
    """


threshold : String
threshold =
    "0.000001 * max_distance"


suffixToBisect : String -> String
suffixToBisect suffix =
    """
            bool bisect""" ++ suffix ++ """(vec3 o, mat2x3 d, float max_distance, out vec3 found) {
                mat2x3 from = mat2x3(o, o);
                mat2x3 to = from + max_distance * d;
                float ithreshold = """ ++ threshold ++ """;
                int depth = 0;
                int choices = 0;
                for(int it = 0; it < MAX_ITERATIONS; it++) {
                    mat2x3 midpoint = 0.5 * (from + to);
                    vec2 front = interval""" ++ suffix ++ """(from, midpoint);
                    vec2 back = interval""" ++ suffix ++ """(midpoint, to);
                    if(depth >= MAX_DEPTH
                        || (front.y - front.x < ithreshold && front.x <= 0.0 && front.y >= 0.0)
                        || (back.y - back.x < ithreshold && back.x <= 0.0 && back.y >= 0.0)
                        ) {
                            found = mix(midpoint[0], midpoint[1], 0.5);
                            return true;
                        }
                    if(front.x <= 0.0 && front.y >= 0.0) {
                        to = midpoint;
                        depth++;
                        choices = left_shift(choices);
                    } else if(back.x <= 0.0 && back.y >= 0.0) {
                        from = midpoint;
                        depth++;
                        choices = left_shift_increment(choices);
                    } else {
                        // This could be possibly helped by https://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightBinSearch
                        for(int j = MAX_DEPTH - 1; j > 0; j--) {
                            if(j > depth)
                                continue;
                            depth--;
                            choices = right_shift(choices);
                            if(is_even(choices)) {
                                midpoint = to;
                                to = to + (to - from);
                                vec2 back = interval""" ++ suffix ++ """(midpoint, to);
                                if(back.x <= 0.0 && back.y >= 0.0) {
                                    from = midpoint;
                                    depth++;
                                    choices = left_shift_increment(choices);
                                    break;
                                }
                            } else {
                                from = from - (to - from);
                            }
                        }
                        if(depth == 0)
                            return false;
                    }
                }
                return false;
            }
    """


deindent : Int -> String -> String
deindent i =
    let
        s =
            String.repeat i " "
    in
    String.split "\n"
        >> List.map
            (\l ->
                if String.startsWith s l then
                    String.dropLeft i l

                else
                    l
            )
        >> String.join "\n"
