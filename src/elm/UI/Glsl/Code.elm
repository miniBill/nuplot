module UI.Glsl.Code exposing (constantToGlsl, deindent, intervalFunctionToGlsl, intervalOperationToGlsl, mainGlsl, straightFunctionToGlsl, straightOperationToGlsl, thetaDelta, toSrc3D, toSrcContour, toSrcImplicit, toSrcRelation)

import Expression exposing (Expression, PrintExpression(..), RelationOperation(..))
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
                if(w.y == 0.0 && z.y == 0.0) {
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
            vec2 ipow(vec2 b, vec2 e) {
                return iexp(iby(iln(b), e));
            }

            vec4 gpow(vec4 b, vec4 e) {
                return gexp(gby(gln(b), e));
            }
            """

        GlslRelations ->
            """
            vec2 ilt(vec2 l, vec2 r) {
                return l.y < r.x ? dup(1.0) : l.x > r.y ? dup(0.0) : vec2(0.0, 1.0);
            }

            vec2 ileq(vec2 l, vec2 r) {
                return l.y <= r.x ? dup(1.0) : l.x > r.y ? dup(0.0) : vec2(0.0, 1.0);
            }

            vec2 ieq(vec2 l, vec2 r) {
                return
                    (l.x == l.y && l.y == r.x && r.x == r.y)
                        ? dup(1.0)
                    : (l.y < r.x || r.y < l.x)
                        ? dup(0.0)
                    : vec2(0.0, 1.0);
            }

            vec2 igeq(vec2 l, vec2 r) {
                return ileq(r, l);
            }

            vec2 igt(vec2 l, vec2 r) {
                return ilt(r, l);
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

        Sqrt22 ->
            """
            vec2 csqrt(vec2 z) {
                if(z.y == 0.0 && z.x >= 0.0) {
                    return vec2(sqrt(z.x), 0);
                }
                float r = pow(dot(z, z), 0.25);
                float t = atan(z.y, z.x) * 0.5;
                return vec2(r * cos(t), r * sin(t));
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
            TODO Arg22
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
            TODO Ceiling22
            """

        Cos22 ->
            """
            TODO Cos22
            """

        Cosh11 ->
            ""

        Cosh22 ->
            """
            vec2 icosh(vec2 v) {
                if(z.x <= 0.0 && z.y >= 0.0)
                    return vec2(cosh(0.0), max(z.y, abs(z.x)));
                if(z.x <= 0.0)
                    return vec2(cosh(-z.y), cosh(-z.x));
                return vec2(cosh(z.x), cosh(z.y));
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
            TODO Floor22
            """

        Im22 ->
            """
            TODO Im22
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
                if(c.x >= 1.0)
                    return t;
                return vec2(min(t.x, f.x), max(t.y, f.y));
            }
            """

        Re22 ->
            """
            TODO Re22
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

            vec2 merge(vec2 l, vec2 r) {
                return vec2(min(l.x, r.x), max(l.y, r.y));
            }

            vec2 isin_piece(vec2 v) {
                vec2 s = sin(v);
                return merge(s, s);
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
                if(between(radians(270.0), from, to))
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

        Square22 ->
            """
            vec2 isquare(vec2 z) {
                if(z.x <= 0.0 && z.y >= 0.0)
                    return vec2(0.0, pow(max(z.y, abs(z.x)), 2.0));
                if(z.x <= 0.0)
                    return vec2(z.y*z.y, z.x*z.x);
                return vec2(z.x*z.x, z.y*z.y);
            }

            vec4 gsquare(vec4 z) {
                return vec4(z.x*z.x, 2.0 * z.x * z.yzw);
            }
            """

        Tan22 ->
            """
            TODO Tan22
            """

        Tanh11 ->
            """
            TODO Tanh11
            """

        Tanh22 ->
            """
            TODO Tanh22
            """


toSrcImplicit : String -> Expression -> String
toSrcImplicit suffix e =
    """
    float f""" ++ suffix ++ """(float x, float y) {
        vec2 complex = """ ++ expressionToGlsl e ++ """;
        if(abs(complex.y) > """ ++ String.fromFloat epsilon ++ """) {
            return -1.0;
        }
        return complex.x > 0.0 ? 1.0 : 0.0;
    }

    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        float h = f""" ++ suffix ++ """(x,y);
        float l = f""" ++ suffix ++ """(x - deltaX,y);
        float ul = f""" ++ suffix ++ """(x - deltaX,y - deltaY);
        float u = f""" ++ suffix ++ """(x,y - deltaY);
        return (h != l || h != u || h != ul)
            && (h >= 0.0 && l >= 0.0 && ul >= 0.0 && u >= 0.0)
                ? vec3(1,1,1) : vec3(0,0,0);
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
        return complex.x > 0.0 && abs(complex.y) < """ ++ String.fromFloat epsilon ++ """ ? vec3(0.8,0.5,0.5) : vec3(0,0,0);
    }
    """


epsilon : Float
epsilon =
    0.00001


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


expressionToIntervalGlsl : Expression -> String
expressionToIntervalGlsl =
    Expression.toPrintExpression
        >> expressionToIntervalGlslPrec 0
        >> wordWrap


expressionToNormalGlsl : Expression -> String
expressionToNormalGlsl =
    Expression.toPrintExpression
        >> expressionToNormalGlslPrec 0
        >> wordWrap


expressionToGlslPrec : Int -> PrintExpression -> String
expressionToGlslPrec p expr =
    let
        paren b c =
            if b then
                "(" ++ c ++ ")"

            else
                c

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
            "vec2(" ++ String.fromFloat f ++ ",0)"

        PNegate expression ->
            "(" ++ noninfix "-" expression ++ ")"

        PAdd l (PNegate r) ->
            infixl_ 6 " - " l r

        PAdd l r ->
            infixl_ 6 " + " l r

        PRel rel l r ->
            "vec2((" ++ expressionToGlslPrec 10 l ++ ".x " ++ rel ++ " " ++ expressionToGlslPrec 10 r ++ ".x) ? 1.0 : 0.0,0.0)"

        PBy l r ->
            apply "by" [ l, r ]

        PDiv l r ->
            apply "div" [ l, r ]

        PPower (PVariable "i") r ->
            apply "cpow" [ PVariable "i", r ]

        PPower (PVariable v) (PInteger 2) ->
            "vec2(" ++ v ++ "*" ++ v ++ ",0)"

        PPower (PVariable v) (PInteger i) ->
            "vec2(pow(" ++ v ++ "," ++ String.fromInt i ++ ".0))"

        PPower l r ->
            apply "cpow" [ l, r ]

        PApply name ex ->
            apply ("c" ++ Expression.functionNameToString name) ex

        PList es ->
            "vec" ++ String.fromInt (List.length es) ++ "(" ++ String.join ", " (List.map (expressionToGlslPrec 0) es) ++ ")"

        PReplace var e ->
            expressionToGlslPrec p (Expression.pfullSubstitute var e)


expressionToIntervalGlslPrec : Int -> PrintExpression -> String
expressionToIntervalGlslPrec p expr =
    let
        paren b c =
            if b then
                "(" ++ c ++ ")"

            else
                c

        noninfix op c =
            paren (p > 10) <| op ++ expressionToIntervalGlslPrec 11 c

        infixl_ n op l r =
            paren (p > n) <| expressionToIntervalGlslPrec n l ++ op ++ expressionToIntervalGlslPrec (n + 1) r

        apply name ex =
            paren (p > 10) <| name ++ "(" ++ String.join ", " (List.map (expressionToIntervalGlslPrec 0) ex) ++ ")"
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
            "dup(" ++ String.fromFloat f ++ ")"

        PNegate expression ->
            "ineg(" ++ expressionToIntervalGlslPrec 10 expression ++ ")"

        PAdd l r ->
            infixl_ 6 " + " l r

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
            name ++ "(" ++ expressionToIntervalGlslPrec 10 l ++ ", " ++ expressionToIntervalGlslPrec 10 r ++ ")"

        PBy l r ->
            apply "iby" [ l, r ]

        PDiv l r ->
            apply "idiv" [ l, r ]

        PPower (PVariable "i") (PInteger 2) ->
            "dup(-1.0)"

        PPower l (PInteger 2) ->
            "isquare(" ++ expressionToIntervalGlslPrec 0 l ++ ")"

        PPower l r ->
            apply "ipow" [ l, r ]

        PApply name ex ->
            apply ("i" ++ Expression.functionNameToString name) ex

        PList es ->
            "vec" ++ String.fromInt (List.length es) ++ "(" ++ String.join ", " (List.map (expressionToIntervalGlslPrec 0) es) ++ ")"

        PReplace var e ->
            expressionToIntervalGlslPrec p (Expression.pfullSubstitute var e)


expressionToNormalGlslPrec : Int -> PrintExpression -> String
expressionToNormalGlslPrec p expr =
    let
        paren b c =
            if b then
                "(" ++ c ++ ")"

            else
                c

        noninfix op c =
            paren (p > 10) <| op ++ expressionToNormalGlslPrec 11 c

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
            "gnum(" ++ String.fromFloat f ++ ")"

        PNegate expression ->
            "gneg(" ++ expressionToNormalGlslPrec 10 expression ++ ")"

        PAdd l r ->
            infixl_ 6 " + " l r

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
            "gnum((" ++ expressionToGlslPrec 10 l ++ op ++ expressionToGlslPrec 10 r ++ ") ? 1.0 : 0.0)"

        PBy l r ->
            apply "gby" [ l, r ]

        PDiv l r ->
            apply "gdiv" [ l, r ]

        PPower l (PInteger 2) ->
            apply "gsquare" [ l ]

        PPower l r ->
            apply "gpow" [ l, r ]

        PApply name ex ->
            apply ("g" ++ Expression.functionNameToString name) ex

        PList es ->
            "vec" ++ String.fromInt (List.length es) ++ "(" ++ String.join ", " (List.map (expressionToNormalGlslPrec 0) es) ++ ")"

        PReplace var e ->
            expressionToNormalGlslPrec p (Expression.pfullSubstitute var e)


mainGlsl : List { name : String, color : Bool } -> List String -> String
mainGlsl pixel2 pixel3 =
    case ( pixel2, pixel3 ) of
        ( _, [] ) ->
            main2D pixel2 ++ "\n\nvoid main () { gl_FragColor = pixel2(); }"

        ( [], _ ) ->
            main3D pixel3 ++ "\n\nvoid main () { gl_FragColor = pixel3(); }"

        _ ->
            main2D pixel2 ++ "\n\n" ++ main3D pixel3 ++ "\n\nvoid main () { gl_FragColor = max(pixel2(), pixel3()); }"


main2D : List { name : String, color : Bool } -> String
main2D pixels =
    let
        addPixel i { name, color } =
            let
                k =
                    if color then
                        "hl2rgb(" ++ String.fromFloat (toFloat (i + 2) / pi) ++ ", 0.5)" ++ " * " ++ name

                    else
                        name
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
        float ax(float coord, float delta) {
            return max(0.0, 1.0 - abs(coord/delta));
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
            vec3 yax = ax(x, deltaX * 2.0) * vec3(0,1,0);
            vec3 xax = ax(y, deltaY * 2.0) * vec3(1,0,0);
            return vec4(max(px, max(xax, yax)), 1.0);
        }
        """


toSrc3D : String -> Expression -> String
toSrc3D suffix e =
    """
    vec3 normal""" ++ suffix ++ """(vec3 p) {
        float x = p.x;
        float y = p.z;
        float z = p.y;
        vec4 gradient = """ ++ expressionToNormalGlsl e ++ """;
        return normalize(gradient.ywz);
    }

    vec2 interval""" ++ suffix ++ """(vec3 f, vec3 t) {
        vec2 x = f.x < t.x ? vec2(f.x, t.x) : vec2(t.x, f.x);
        vec2 z = f.y < t.y ? vec2(f.y, t.y) : vec2(t.y, f.y);
        vec2 y = f.z < t.z ? vec2(f.z, t.z) : vec2(t.z, f.z);
        return """ ++ expressionToIntervalGlsl e ++ """;
    }
    """


main3D : List String -> String
main3D suffixes =
    let
        head =
            String.join "\n" <| List.map suffixToBisect suffixes ++ [ raytrace ]

        colorCoeff =
            String.fromFloat (1.19 - 0.2 * toFloat (List.length suffixes))

        innerTrace =
            String.join "\n" (List.indexedMap suffixToRay suffixes)

        innerLightTrace =
            String.join "\n" (List.map suffixToRayLight suffixes)

        raytrace =
            """
            vec4 raytrace(vec3 o, vec3 d, float max_distance, vec3 light) {
                vec3 found = o + 2.0 * max_distance * d;
                vec3 normal = vec3(0);
                float curr_distance = max_distance;
                int found_index = -1;
                vec3 f = vec3(0);
                vec3 n = vec3(0);
                """ ++ innerTrace ++ """
                if(length(found - o) < max_distance) {
                    float h = (float(found_index))*radians(360.0 / 1.1);
                    float fy = found.y * 0.5;

                    float light_distance = length(light - found);
                    vec3 light_direction = normalize(light - found);
                    bool in_light = true;
                    vec3 offseted = found + 0.001 * normal;
                    normal = -faceforward(normal, normal, light_direction);
                    if(0 == 1) { }
                    """ ++ innerLightTrace ++ """
                    float l = in_light ? mix(0.4, 0.6, dot(normal, light_direction)) : 0.2;

                    vec3 px = mix(
                        hl2rgb(h, l),
                        hl2rgb(fy, l),
                        max(0.2, """ ++ colorCoeff ++ """)
                    );
                    return vec4(px, 1.0);
                } else {
                    return vec4(vec3(0),1.0);
                }
            }
            """

        suffixToRay i suffix =
            """
                if(bisect""" ++ suffix ++ """(o, d, max_distance, f, n) && length(f - o) < curr_distance) {
                    found_index = """ ++ String.fromInt i ++ """;
                    found = f;
                    normal = n;
                    curr_distance = length(found - o);
                }
            """

        suffixToRayLight suffix =
            """
                else if(bisect""" ++ suffix ++ """(offseted, light_direction, light_distance, f, n)) {
                    in_light = false;
                }
            """

        suffixToBisect suffix =
            """
            bool bisect""" ++ suffix ++ """(vec3 o, vec3 d, float max_distance, out vec3 found, out vec3 n) {
                vec3 from = o;
                vec3 to = o + max_distance * d;
                float threshold = 0.01;
                bool path[MAXDEPTH];
                int depth = 0;
                int recover_depth = 0;
                vec3 recover_from = vec3(0);
                vec3 recover_to = vec3(0);
                for(int it = 0; it < MAXDEPTH; it++) {
                    vec3 midpoint = mix(from, to, 0.5);
                    vec2 front = interval""" ++ suffix ++ """(from, midpoint);
                    vec2 back = interval""" ++ suffix ++ """(midpoint, to);
                    if((front.y - front.x < threshold || back.y - back.x < threshold)
                        && length(from - to) < pow(threshold, 3.0) * max_distance) {
                            found = midpoint;
                            n = normal""" ++ suffix ++ """(midpoint);
                            return true;
                        }
                    if(front.x <= 0.0 && front.y >= 0.0) {
                        to = midpoint;
                        depth++;
                        for(int j = 0; j < MAXDEPTH; j++) if(j == depth) path[j] = true;
                        if(back.x <= 0.0 && back.y >= 0.0) {
                            recover_depth = depth;
                            recover_from = from;
                            recover_to = to;
                        }
                    } else if(back.x <= 0.0 && back.y >= 0.0) {
                        from = midpoint;
                        depth++;
                        for(int j = 0; j < MAXDEPTH; j++) if(j == depth) path[j] = false;
                    } else if(recover_depth > 0) {
                        for(int j = MAXDEPTH - 1; j >= 0; j--) {
                            path[j] = false;
                            if(j == recover_depth)
                                break;
                        }
                        from = recover_from;
                        to = recover_to;
                        depth = recover_depth;
                        recover_depth = -1;
                    } else {
                        for(int j = 0; j < MAXDEPTH; j++) {
                            if(depth == 0)
                                return false;
                            depth--;
                            bool found = false;
                            for(int j = 0; j < MAXDEPTH; j++)
                                if(j == depth) {
                                    if(path[j]) {
                                        midpoint = to;
                                        to = to + (to - from);
                                        vec2 back = interval""" ++ suffix ++ """(midpoint, to);
                                        if(back.x <= 0.0 && back.y >= 0.0) {
                                            from = midpoint;
                                            depth++;
                                            path[j] = false;
                                            found = true;
                                        }
                                        break;
                                    } else {
                                        from = from - (to - from);
                                    }
                                }
                            if(found)
                                break;
                        }
                    }
                }
                return false;
            }
            """
    in
    deindent 12 <|
        head
            ++ """
            float ax(float coord, float delta) {
                return max(0.0, 1.0 - abs(coord/delta));
            }

            vec3 color(float y) {
                float logRadius = log2(abs(y));
                float powerRemainder = fract(logRadius);
                float squished = 0.7 - powerRemainder * 0.4;

                float haf = fract(logRadius) / 6.0;
                if(y > 0.0)
                    return hl2rgb(haf + 0.3, squished);
                else
                    return hl2rgb(haf - 0.1, 1.0 - squished);
            }

            vec4 pixel3 () {
                vec3 eye = vec3(0, u_viewportWidth, -1.5*u_viewportWidth);
                vec3 light = vec3(-0.5*u_viewportWidth, 2.0 * u_viewportWidth, 0);
                float focal_dist = 1.0;

                vec2 canvasSize = vec2(u_canvasWidth, u_canvasHeight);
                vec2 uv_centered = gl_FragCoord.xy - 0.5 * canvasSize;

                vec2 uv = 1.0 / u_canvasHeight * uv_centered;

                vec3 target = vec3(u_zoomCenter, 0);
                vec3 to_center = normalize(target-eye);
                vec3 canvas_center = eye + to_center;
                vec3 canvas_point = canvas_center + vec3(uv.x, uv.y * to_center.z, -uv.y * to_center.y);

                vec3 ray_direction = normalize(canvas_point - eye);
                float max_distance = 100.0 * length(eye);
                return raytrace(canvas_point, ray_direction, max_distance, light);
            }
        """


deindent : Int -> String -> String
deindent i =
    String.split "\n"
        >> List.map (String.dropLeft i)
        >> String.join "\n"
