module UI.Glsl.Code exposing (Uniforms, constantToGlsl, deindent, expressionToGlsl, intervalFunctionToGlsl, intervalOperationToGlsl, mainGlsl, straightFunctionToGlsl, straightOperationToGlsl, suffixToBisect, thetaDelta, threshold, toSrc3D, toSrcContour, toSrcImplicit, toSrcParametric, toSrcPolar, toSrcRelation, toSrcVectorField2D)

import Dict
import Expression exposing (FunctionName(..), KnownFunction(..), PrintExpression(..), toPrintExpression)
import UI.Glsl.Generator as Generator exposing (Expression, File, FunDecl, Statement, TypedName, Vec2, Vec3, Vec4, abs_, add, ands, arr, assign, by, byF, call0, call1, call2, call4, ceil_, decl, def, div, dot2, dotted2, dotted3, dotted4, eq, exp, float, floatT, floatToGlsl, fun0, fun1, fun2, fun3, geq, gl_FragColor, gl_FragCoord, gt, if_, int, log, lt, mat3T, max_, min_, mod, negate_, normalize, one, pow, radians_, return, sign, subtract, ternary, unknown, unknownFunDecl, unknownTypedName, unsafeCall, vec2, vec2T, vec2Zero, vec3, vec3T, vec3Zero, vec4, vec4T, vec4Zero, vec4_3_1, voidT, zero)
import UI.Glsl.Model exposing (GlslConstant(..), GlslFunction(..), GlslOperation(..))


type alias Uniforms =
    { u_canvasWidth : Expression Float
    , u_canvasHeight : Expression Float
    , u_viewportWidth : Expression Float
    , u_viewportHeight : Expression Float
    , u_drawAxes : Expression Float
    , u_zoomCenter : Expression Vec2
    }


constantToGlsl : GlslConstant -> FunDecl
constantToGlsl c =
    case c of
        I ->
            fun0 vec2T "i" <|
                [ return <| vec2 zero one ]

        Pi ->
            fun0 vec2T "pi" <|
                [ return <| vec2 (radians_ <| float 180) zero ]

        E ->
            fun0 vec2T "e" <|
                [ return <| vec2 (exp one) zero ]


by2Tuple : ( FunDecl, Expression Vec2 -> Expression Vec2 -> Expression Vec2 )
by2Tuple =
    fun2 vec2T "by" (vec2T "a") (vec2T "b") <|
        \a b ->
            let
                da =
                    dotted2 a

                db =
                    dotted2 b
            in
            [ return <|
                vec2
                    (subtract
                        (by da.x db.x)
                        (by da.y db.y)
                    )
                    (add
                        (by da.x db.y)
                        (by da.y db.x)
                    )
            ]


by2Decl : FunDecl
by2Decl =
    Tuple.first by2Tuple


by2 : Expression Vec2 -> Expression Vec2 -> Expression Vec2
by2 =
    Tuple.second by2Tuple


by3Tuple : ( FunDecl, Expression Vec2 -> Expression Vec2 -> Expression Vec2 -> Expression Vec2 )
by3Tuple =
    fun3 vec2T "by" (vec2T "a") (vec2T "b") (vec2T "c") <|
        \a b c ->
            [ return <| by2 (by2 a b) c ]


by3Decl : FunDecl
by3Decl =
    Tuple.first by3Tuple


by3 : Expression Vec2 -> Expression Vec2 -> Expression Vec2 -> Expression Vec2
by3 =
    Tuple.second by3Tuple


div2Tuple : ( FunDecl, Expression Vec2 -> Expression Vec2 -> Expression Vec2 )
div2Tuple =
    fun2 vec2T "div" (vec2T "a") (vec2T "b") <|
        \a b ->
            let
                da =
                    dotted2 a

                db =
                    dotted2 b
            in
            def floatT "k" (div one <| dot2 b b) <|
                \k ->
                    def floatT "r" (by k <| dot2 a b) <|
                        \r ->
                            def floatT
                                "i"
                                (by k
                                    (subtract (by da.y db.x) (by da.x db.y))
                                )
                            <|
                                \i ->
                                    [ return <| vec2 r i ]


div2Decl : FunDecl
div2Decl =
    Tuple.first div2Tuple


div2 : Expression Vec2 -> Expression Vec2 -> Expression Vec2
div2 =
    Tuple.second div2Tuple


log10Tuple : ( FunDecl, Expression Float -> Expression Float )
log10Tuple =
    fun1 floatT "log10" (floatT "x") <|
        \x -> [ return <| div (log x) (log <| float 10) ]


log10Decl : FunDecl
log10Decl =
    Tuple.first log10Tuple


log10 : Expression Float -> Expression Float
log10 =
    Tuple.second log10Tuple


straightOperationToGlsl : GlslOperation -> List FunDecl
straightOperationToGlsl op =
    case op of
        GlslAddition ->
            []

        GlslMultiplication ->
            [ by2Decl
            , by3Decl
            ]

        GlslNegation ->
            []

        GlslDivision ->
            [ div2Decl
            ]

        GlslPower ->
            [ Tuple.first <|
                fun2 vec2T "cpow" (vec2T "w") (vec2T "z") <|
                    \w z ->
                        let
                            dw =
                                dotted2 w

                            dz =
                                dotted2 z
                        in
                        [ return <|
                            ternary
                                (ands
                                    [ geq dw.x zero
                                    , eq dw.y zero
                                    , eq dz.y zero
                                    ]
                                )
                                (vec2 (pow dw.x dz.x) zero)
                                (call1 (unknownTypedName "cexp") <| by (call1 (unknownTypedName "cln") w) z)
                        ]
            ]

        GlslRelations ->
            []


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
                if(e.y - e.x < 0.000001 && abs(e.x - round(e.x)) < 0.000001)
                    return ipow(b, int(e.x));
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


straightFunctionToGlsl : GlslFunction -> List FunDecl
straightFunctionToGlsl name =
    case name of
        Sinh11 ->
            [ Tuple.first <|
                fun1 floatT "sinh" (floatT "x") <|
                    \x -> [ return <| by (float 0.5) <| subtract (exp x) (exp <| negate_ x) ]
            ]

        Cosh11 ->
            [ Tuple.first <|
                fun1 floatT "cosh" (floatT "x") <|
                    \x -> [ return <| by (float 0.5) <| add (exp x) (exp <| negate_ x) ]
            ]

        Tanh11 ->
            [ Tuple.first <|
                fun1 floatT "tanh" (floatT "x") <|
                    \x ->
                        if_ (gt (abs_ x) (float 10))
                            (return <| sign x)
                            :: (def floatT "p" (exp x) <|
                                    \p ->
                                        def floatT "m" (exp <| negate_ x) <|
                                            \m ->
                                                [ return <| div (subtract p m) (add p m)
                                                ]
                               )
            ]

        Sin22 ->
            [ unknownFunDecl
                { name = "csin"
                , type_ = "vec2 -> vec2"
                , body =
                    """
            vec2 csin(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(sin(z.x), 0);
                }
                return vec2(sin(z.x) * cosh(z.y), cos(z.x) * sinh(z.y));
            }
            """
                }
            ]

        Cos22 ->
            [ unknownFunDecl
                { name =
                    "ccos"
                , type_ = "TODO"
                , body = """
            vec2 ccos(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(cos(z.x), 0);
                }
                return vec2(cos(z.x) * cosh(z.y), sin(z.x) * sinh(z.y));
            }
            """
                }
            ]

        Tan22 ->
            [ unknownFunDecl
                { name =
                    "ctan"
                , type_ = "TODO"
                , body = """
            vec2 ctan(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(tan(z.x), 0);
                }
                return div(csin(z), ccos(z));
            }
            """
                }
            ]

        Asin22 ->
            [ unknownFunDecl
                { name =
                    "casin"
                , type_ = "TODO"
                , body = """
            vec2 casin(vec2 z) {
                vec2 s = csqrt(vec2(1, 0) - by(z, z));
                vec2 arg = s - by(vec2(0, 1), z);
                return by(vec2(0, 1), cln(arg));
            }
            """
                }
            ]

        Acos22 ->
            [ unknownFunDecl
                { name =
                    "cacos"
                , type_ = "TODO"
                , body = """
            vec2 cacos(vec2 z) {
                return pi() * 0.5 - casin(z);
            }
            """
                }
            ]

        Atan22 ->
            [ unknownFunDecl
                { name =
                    "catan"
                , type_ = "TODO"
                , body = """
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
                }
            ]

        Atan222 ->
            [ unknownFunDecl
                { name =
                    "catan2"
                , type_ = "TODO"
                , body = """
            vec2 catan2(vec2 y, vec2 x) {
                vec2 z = vec2(x.x - y.y, x.y + y.x);
                return vec2(atan(z.y, z.x), 0.0);
            }
            """
                }
            ]

        Sinh22 ->
            [ unknownFunDecl { name = "csinh", type_ = "TODO", body = """
            vec2 csinh(vec2 z) {
                return 0.5 * (cexp(z) - cexp(-z));
            }
            """ } ]

        Cosh22 ->
            [ unknownFunDecl { name = "ccosh", type_ = "TODO", body = """
            vec2 ccosh(vec2 z) {
                return 0.5 * (cexp(z) + cexp(-z));
            }
            """ } ]

        Tanh22 ->
            [ unknownFunDecl { name = "ctanh", type_ = "TODO", body = """
            vec2 ctanh(vec2 z) {
                vec2 p = cexp(z);
                vec2 m = cexp(-z);
                return div(p - m, p + m);
            }
            """ } ]

        Abs22 ->
            [ unknownFunDecl { name = "cabs", type_ = "TODO", body = """
            vec2 cabs(vec2 z) {
                return vec2(length(z), 0.0);
            }
            """ } ]

        Sign22 ->
            [ unknownFunDecl { name = "csign", type_ = "TODO", body = """
            vec2 csign(vec2 z) {
                return vec2(sign(z.x), sign(z.y));
            }
            """ } ]

        Sqrt22 ->
            [ unknownFunDecl { name = "csqrt", type_ = "TODO", body = """
            vec2 csqrt(vec2 z) {
                if(z.y == 0.0 && z.x >= 0.0) {
                    return vec2(sqrt(z.x), 0);
                }
                float r = pow(dot(z, z), 0.25);
                float t = atan(z.y, z.x) * 0.5;
                return r * vec2(cos(t), sin(t));
            }
            """ } ]

        Cbrt22 ->
            [ unknownFunDecl { name = "ccbrt", type_ = "TODO", body = """
            vec2 ccbrt(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(sign(z.x) * pow(z.x, 1.0 / 3.0), 0);
                }
                float r = pow(dot(z, z), 1.0 / 6.0);
                float t = atan(z.y, z.x) / 3.0 + (z.x > 0.0 ? 0.0 : radians(120.0));
                return r * vec2(cos(t), sin(t));
            }
            """ } ]

        Square22 ->
            [ unknownFunDecl { name = "csquare", type_ = "TODO", body = """
            vec2 csquare(vec2 z) {
                return by(z, z);
            }
            """ } ]

        Ln22 ->
            [ unknownFunDecl { name = "cln", type_ = "TODO", body = """
            vec2 cln(vec2 z) {
                if(z.y == 0.0 && z.x >= 0.0) {
                    return vec2(log(z.x), 0);
                }
                float px = length(z);
                float py = atan(z.y, z.x);
                return vec2(log(px), py);
            }
            """ } ]

        Log1022 ->
            [ unknownFunDecl { name = "clog10", type_ = "TODO", body = """
            vec2 clog10(vec2 z) {
                return div(cln(z), vec2(log(10.0), 0));
            }
            """ } ]

        Exp22 ->
            [ unknownFunDecl { name = "cexp", type_ = "TODO", body = """
            vec2 cexp(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(exp(z.x), 0);
                }
                return vec2(cos(z.y) * exp(z.x), sin(z.y) * exp(z.x));
            }
            """ } ]

        Re22 ->
            [ unknownFunDecl { name = "cre", type_ = "TODO", body = """
            vec2 cre(vec2 z) {
                return vec2(z.x, 0.0);
            }
            """ } ]

        Im22 ->
            [ unknownFunDecl { name = "cim", type_ = "TODO", body = """
            vec2 cim(vec2 z) {
                return vec2(z.y, 0.0);
            }
            """ } ]

        Arg22 ->
            [ unknownFunDecl { name = "arg", type_ = "TODO", body = """
            float arg(vec2 v) {
                return atan(v.y, v.x);
            }

            vec2 carg(vec2 v) {
                return vec2(atan(v.y, v.x), 0);
            }
            """ } ]

        Pw22 ->
            [ unknownFunDecl { name = "cpw", type_ = "TODO", body = """
            vec2 cpw(vec2 c, vec2 t, vec2 f) {
                return c.x > 0.0 ? t : f;
            }
            """ } ]

        Ceiling22 ->
            [ unknownFunDecl { name = "cceiling", type_ = "TODO", body = """
            vec2 cceiling(vec2 z) {
                return ceil(z);
            }
            """ } ]

        Floor22 ->
            [ unknownFunDecl { name = "cfloor", type_ = "TODO", body = """
            vec2 cfloor(vec2 z) {
                return floor(z);
            }
            """ } ]

        Round22 ->
            [ unknownFunDecl { name = "cround", type_ = "TODO", body = """
            vec2 cround(vec2 z) {
                return floor(z + vec2(0.5, 0.5));
            }
            """ } ]

        Min222 ->
            [ unknownFunDecl { name = "cmin", type_ = "TODO", body = """
            vec2 cmin(vec2 l, vec2 r) {
                return l.x < r.x ? l : r;
            }
            """ } ]

        Max222 ->
            [ unknownFunDecl { name = "cmax", type_ = "TODO", body = """
            vec2 cmax(vec2 l, vec2 r) {
                return l.x > r.x ? l : r;
            }
            """ } ]

        Mod22 ->
            [ unknownFunDecl { name = "cmod", type_ = "TODO", body = """
            vec2 cmod(vec2 l, vec2 r) {
                return vec2(mod(l.x, r.x), 0);
            }
            """ } ]

        Mbrot22 ->
            [ unknownFunDecl { name = "cmbrot", type_ = "TODO", body = """
            vec2 cmbrot(vec2 x, vec2 y) {
                vec2 c = x + vec2(-y.y, y.x);

                float p = length(c - vec2(0.25, 0));
                if(c.x <= p - 2.0*p*p + 0.25 || length(c + vec2(1,0)) <= 0.25)
                    return vec2(0,0);

                vec2 z = c;
                for(int i = 0; i < 4000; i++) {
                    z = vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y) + c;
                    if(length(z) > 1000000.0) {
                        float logLength = log(length(z));
                        float nu = log(logLength / log(2.0)) / log(2.0);
                        float fi = float(i) - nu;
                        return vec2(sin(fi),cos(fi));
                    }
                }
                return vec2(0,0);
            }
            """ } ]


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
            vec2 iacos(vec2 z) {
                // Don't use clamp so if z is fully outside range the result is empty
                vec2 clamped = vec2(
                    max(z.x, -1.0),
                    min(z.y, 1.0)
                );
                return acos(clamped).yx;
            }

            vec4 gacos(vec4 v) {
                return vec4(acos(v.x), -v.yzw / sqrt(1.0 - v.x * v.x));
            }
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
            vec2 iasin(vec2 z) {
                // Don't use clamp so if z is fully outside range the result is empty
                vec2 clamped = vec2(
                    max(z.x, -1.0),
                    min(z.y, 1.0)
                );
                return asin(clamped);
            }

            vec4 gasin(vec4 v) {
                return vec4(asin(v.x), v.yzw / sqrt(1.0 - v.x * v.x));
            }
            """

        Atan22 ->
            """
            vec2 iatan(vec2 z) {
                return atan(z);
            }

            vec4 gatan(vec4 v) {
                return vec4(atan(v.x), v.yzw / (1.0 + v.x * v.x));
            }
            """

        Atan222 ->
            """
            vec2 iatan2(vec2 y, vec2 x) {
                // TODO: improve this
                vec2 r = vec2(atan(y.x, x.x), atan(y.y, x.x));
                vec2 q = vec2(atan(y.x, x.y), atan(y.y, x.y));
                vec2 rs = vec2(min(r.x, r.y), max(r.x, r.y));
                vec2 qs = vec2(min(q.x, q.y), max(q.x, q.y));
                return vec2(min(rs.x,qs.x),max(rs.y,qs.y));
            }

            vec4 gatan2(vec4 y, vec4 x) {
                return vec4(atan(y.x, x.x), y.yzw / x.yzw / (1.0 + y.x * y.x / x.x / x.x));
            }
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
            vec2 ilog10(vec2 z) {
                return log(z) / log(10.0);
            }

            vec4 glog10(vec4 z) {
                return vec4(log(z.x), z.yzw / z.x) / log(10.0);
            }
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
            vec2 iround(vec2 z) {
                return round(z);
            }

            vec4 ground(vec4 z) {
                return gnum(round(z.x));
            }
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

        Mbrot22 ->
            """TODO"""


toSrcImplicit : String -> Expression.Expression -> String
toSrcImplicit suffix e =
    let
        antialiasingSamples =
            7
    in
    """
    float f""" ++ suffix ++ """(float x, float y) {
        vec2 complex = """ ++ Generator.expressionToGlsl (expressionToGlsl [ ( "x", unknown "x" ), ( "y", unknown "y" ) ] e) ++ """;
        if(abs(complex.y) > """ ++ floatToGlsl epsilon ++ """) {
            return 0.0;
        }
        return complex.x > 0.0 ? 1.0 : -1.0;
    }

    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        float sum = 0.0;
        float samples = """ ++ floatToGlsl antialiasingSamples ++ """ * 2.0 + 1.0;
        samples *= samples;
        float coeff = 0.0875;
        for(int w = -""" ++ String.fromInt antialiasingSamples ++ """; w <= """ ++ String.fromInt antialiasingSamples ++ """; w++)
            for(int h = -""" ++ String.fromInt antialiasingSamples ++ """; h <= """ ++ String.fromInt antialiasingSamples ++ """; h++) {
                float piece = f""" ++ suffix ++ """(x + deltaX * coeff * float(w), y + deltaX * coeff * float(h));
                if(piece == 0.0)
                    return vec3(0);
                sum += piece;
            }

        float perc = (samples - abs(sum)) / samples;
        perc = pow(perc, 0.2);
        return perc * vec3(1,1,1);
    }
    """


toSrcPolar : String -> Expression.Expression -> String
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
        vec2 complex = """ ++ Generator.expressionToGlsl (expressionToGlsl [ ( "x", unknown "x" ), ( "y", unknown "y" ), ( "r", unknown "r" ), ( "t", unknown "t" ) ] e) ++ """;
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


toSrcParametric : Bool -> String -> Expression.Expression -> String
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


toSrcVectorField2D : String -> Expression.Expression -> Expression.Expression -> String
toSrcVectorField2D suffix x y =
    """
    vec2 vector""" ++ suffix ++ """(float x, float y) {
        vec2 xv = """ ++ Generator.expressionToGlsl (expressionToGlsl [ ( "x", unknown "x" ), ( "y", unknown "y" ) ] x) ++ """;
        vec2 yv = """ ++ Generator.expressionToGlsl (expressionToGlsl [ ( "x", unknown "x" ), ( "y", unknown "y" ) ] y) ++ """;
        return abs(xv.y) + abs(yv.y) < """ ++ floatToGlsl epsilon ++ """ ? vec2(xv.x, yv.x) : vec2(0,0);
    }

    bool near(vec2 o, vec2 corner, vec2 vector, float deltaX, float mx) {
        float angleCorner = arg(o - corner);
        float angleVector = arg(vector);
        float delta = mod(angleCorner - angleVector, radians(360.0));
        float l = length(vector) / mx;
        float maxLength = deltaX * VECTOR_SPACING * (l < """ ++ floatToGlsl epsilon ++ """ ? 0.0 : l / 2.0 + 0.5);
        float wantedLength = length(o - corner);
        float angularDistance = mix(180.0, 0.0, pow(wantedLength / maxLength, 0.3));
        return (delta < radians(angularDistance) || delta > radians(360.0 - angularDistance)) && wantedLength < maxLength;
    }

    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        vec2 o = vec2(x, y);

        float mx = 0.0;
        for(int xi = -X_POINTS; xi <= X_POINTS; xi++) {
            for(int yi = -Y_POINTS; yi <= Y_POINTS; yi++) {
                vec2 p = u_zoomCenter + vec2(deltaX * VECTOR_SPACING * float(xi), deltaX * VECTOR_SPACING * float(yi));
                vec2 v = vector""" ++ suffix ++ """(p.x, p.y);
                mx = max(mx, length(v));
            }
        }

        vec3 colorA = vec3(0.0, 1.0, 0.0);
        vec3 colorB = vec3(0.0, 0.0, 1.0);

        x = o.x - mod(o.x, deltaX * VECTOR_SPACING);
        y = o.y - mod(o.y, deltaX * VECTOR_SPACING);

        vec2 bl = vector""" ++ suffix ++ """(x, y);
        vec2 br = vector""" ++ suffix ++ """(x + deltaX * VECTOR_SPACING, y);
        vec2 ul = vector""" ++ suffix ++ """(x, y + deltaX * VECTOR_SPACING);
        vec2 ur = vector""" ++ suffix ++ """(x + deltaX * VECTOR_SPACING, y + deltaX * VECTOR_SPACING);

        float angleO;
        vec2 corner;
        float l;

        corner = vec2(x, y);
        l = length(bl) / mx;
        if(near(o, corner, bl, deltaX, mx))
            return mix(colorA, colorB, l);

        corner = vec2(x + deltaX * VECTOR_SPACING, y);
        l = length(br) / mx;
        if(near(o, corner, br, deltaX, mx))
            return mix(colorA, colorB, l);

        corner = vec2(x, y + deltaX * VECTOR_SPACING);
        l = length(ul) / mx;
        if(near(o, corner, ul, deltaX, mx))
            return mix(colorA, colorB, l);

        corner = vec2(x + deltaX * VECTOR_SPACING, y + deltaX * VECTOR_SPACING);
        l = length(ur) / mx;
        if(near(o, corner, ur, deltaX, mx))
            return mix(colorA, colorB, l);

        return vec3(0,0,0);
    }
    """


toSrcContour : String -> Expression.Expression -> String
toSrcContour suffix e =
    """
    vec3 pixel""" ++ suffix ++ """_o(float deltaX, float deltaY, float x, float y) {
        vec2 z = """ ++ Generator.expressionToGlsl (expressionToGlsl [ ( "x", unknown "x" ), ( "y", unknown "y" ) ] e) ++ """;

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


toSrcRelation : String -> Expression.Expression -> String
toSrcRelation suffix e =
    """
    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        vec2 complex = """ ++ Generator.expressionToGlsl (expressionToGlsl [ ( "x", unknown "x" ), ( "y", unknown "y" ) ] e) ++ """;
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


expressionToIntervalGlsl : Bool -> Expression.Expression -> String
expressionToIntervalGlsl expandIntervals =
    toPrintExpression
        >> expressionToIntervalGlslPrec expandIntervals 0
        >> wordWrap


expressionToGlsl : List ( String, Expression Float ) -> Expression.Expression -> Expression Vec2
expressionToGlsl context =
    let
        ctx =
            Dict.fromList context

        go expr =
            case expr of
                PVariable "i" ->
                    vec2 zero one

                PVariable "pi" ->
                    vec2 (radians_ <| float 180) zero

                PVariable "e" ->
                    vec2 (exp one) zero

                PVariable v ->
                    case Dict.get v ctx of
                        Just w ->
                            vec2 w zero

                        Nothing ->
                            unknown <| "Variable " ++ v ++ " is undefined"

                PInteger v ->
                    vec2 (float <| toFloat v) zero

                PFloat f ->
                    vec2 (float f) zero

                PNegate expression ->
                    negate_ <| go expression

                PAdd l (PNegate r) ->
                    subtract (go l) (go r)

                PAdd l r ->
                    add (go l) (go r)

                PRel op l r ->
                    if op == "<=" || op == "<" then
                        subtract (go r) (go l)

                    else if op == "=" then
                        abs_ <| subtract (go r) (go l)

                    else
                        subtract (go l) (go r)

                PBy l r ->
                    by2 (go l) (go r)

                PDiv l r ->
                    div2 (go l) (go r)

                PPower (PVariable v) (PInteger 2) ->
                    case Dict.get v ctx of
                        Just w ->
                            vec2 (by w w) zero

                        Nothing ->
                            unknown <| "Variable " ++ v ++ " is undefined"

                PPower l r ->
                    call2 (unknownTypedName "cpow") (go l) (go r)

                PApply (KnownFunction Simplify) [ e ] ->
                    go e

                PApply name ex ->
                    if List.any (\( _, v ) -> name == KnownFunction v) Expression.variadicFunctions then
                        case List.map go ex of
                            [] ->
                                vec2Zero

                            head :: tail ->
                                List.foldl (\e a -> call2 (unknownTypedName <| "c" ++ Expression.functionNameToString name) a e) head tail

                    else
                        unsafeCall ("c" ++ Expression.functionNameToString name) (List.map go ex)

                PList es ->
                    unsafeCall ("vec" ++ String.fromInt (List.length es)) (List.map go es)

                PReplace var e ->
                    go (Expression.pfullSubstitute var e)

                -- If this happens, it's too late
                PLambda _ _ ->
                    vec2Zero
    in
    toPrintExpression >> go


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

        PPower l (PFloat f) ->
            if f == 2.0 then
                expand <| "isquare(" ++ expressionToIntervalGlslPrec expandIntervals 0 l ++ ")"

            else
                expand <| apply "ipow" [ l, PFloat f ]

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


expressionToNormalGlsl : { x : Expression Float, y : Expression Float, z : Expression Float } -> Expression.Expression -> Expression Vec4
expressionToNormalGlsl { x, y, z } =
    let
        gnum : Expression Float -> Expression Vec4
        gnum =
            call1 (unknownTypedName "gnum")

        go expr =
            case expr of
                PVariable "pi" ->
                    gnum (radians_ <| float 180)

                PVariable "e" ->
                    gnum <| exp one

                PVariable "x" ->
                    vec4 x one zero zero

                PVariable "y" ->
                    vec4 y zero one zero

                PVariable "z" ->
                    vec4 z zero zero one

                PInteger v ->
                    gnum <| float <| toFloat v

                PFloat f ->
                    gnum <| float f

                PNegate expression ->
                    call1 (unknownTypedName "gneg") (go expression)

                PAdd l r ->
                    add (go l) (go r)

                PRel op l r ->
                    if op == "<=" || op == "<" then
                        subtract (go r) (go l)

                    else if op == "=" then
                        abs_ <| subtract (go r) (go l)

                    else
                        subtract (go l) (go r)

                PBy l r ->
                    call2 (unknownTypedName "gby") (go l) (go r)

                PDiv l r ->
                    call2 (unknownTypedName "gdiv") (go l) (go r)

                PPower l (PInteger 2) ->
                    call1 (unknownTypedName "gsquare") (go l)

                PPower l (PInteger ri) ->
                    call2 (unknownTypedName "gpow") (go l) (int ri)

                PPower l r ->
                    call2 (unknownTypedName "gpow") (go l) (go r)

                PApply name ex ->
                    if List.any (\( _, v ) -> name == KnownFunction v) Expression.variadicFunctions then
                        case List.map go ex of
                            [] ->
                                vec4Zero

                            head :: tail ->
                                List.foldl (\e a -> call2 (unknownTypedName <| "g" ++ Expression.functionNameToString name) a e) head tail

                    else
                        unsafeCall ("g" ++ Expression.functionNameToString name) (List.map go ex)

                PList es ->
                    unsafeCall ("vec" ++ String.fromInt (List.length es)) (List.map go es)

                PReplace var e ->
                    go (Expression.pfullSubstitute var e)

                PVariable _ ->
                    vec4Zero

                -- If this happens, it's too late
                PLambda _ _ ->
                    vec4Zero
    in
    toPrintExpression >> go


mainGlsl :
    Uniforms
    -> Bool
    ->
        List
            { name : TypedName (Float -> Float -> Float -> Float -> Vec3)
            , color : Bool
            }
    -> List String
    -> String
mainGlsl uniforms rayDifferentials pixel2 pixel3 =
    case ( pixel2, pixel3 ) of
        ( _, [] ) ->
            Generator.fileToGlsl <|
                main2D uniforms pixel2
                    ++ [ fun0 voidT "main" <|
                            [ assign gl_FragColor <| call0 (unknownTypedName "pixel2") ]
                       ]

        ( [], _ ) ->
            main3D rayDifferentials pixel3
                ++ "\n\n"
                ++ Generator.fileToGlsl
                    [ fun0 voidT "main" <|
                        [ assign gl_FragColor <| call0 (unknownTypedName "pixel3") ]
                    ]

        _ ->
            Generator.fileToGlsl (main2D uniforms pixel2)
                ++ "\n\n"
                ++ main3D rayDifferentials pixel3
                ++ Generator.fileToGlsl
                    [ fun0 voidT "main" <|
                        [ assign gl_FragColor <| max_ (call0 (unknownTypedName "pixel2")) (call0 (unknownTypedName "pixel3")) ]
                    ]


main2D :
    Uniforms
    ->
        List
            { name : TypedName (Float -> Float -> Float -> Float -> Vec3)
            , color : Bool
            }
    -> File
main2D uniforms pixels =
    [ log10Decl
    , axisDecl
    , pixel2Decl uniforms pixels
    ]


pixel2Decl :
    Uniforms
    ->
        List
            { name : TypedName (Float -> Float -> Float -> Float -> Vec3)
            , color : Bool
            }
    -> FunDecl
pixel2Decl uniforms pixels =
    let
        addPixel :
            Expression Float
            -> Expression Float
            -> Expression Float
            -> Expression Float
            -> Expression Vec3
            -> Expression Vec3
            -> Int
            ->
                { name : TypedName (Float -> Float -> Float -> Float -> Vec3)
                , color : Bool
                }
            -> List (Statement f)
        addPixel deltaX deltaY x y px curr i { name, color } =
            let
                k =
                    if color then
                        let
                            h =
                                float <| (toFloat (i + 2) / pi)
                        in
                        by (call2 (unknownTypedName "hl2rgb") h (float 0.5)) (call4 name deltaX deltaY x y)

                    else
                        call4 name deltaX deltaY x y
            in
            [ assign curr k
            , assign px <| ternary (eq curr vec3Zero) px curr
            ]

        inner deltaX deltaY x y px curr =
            pixels
                |> List.indexedMap (addPixel deltaX deltaY x y px curr)
                |> List.concat
    in
    fun0 vec4T "pixel2" <|
        def vec2T "canvasSize" (vec2 uniforms.u_canvasWidth uniforms.u_canvasHeight) <|
            \canvasSize ->
                def vec2T "uv_centered" (subtract (dotted4 gl_FragCoord).xy (byF (float 0.5) canvasSize)) <|
                    \uv_centered ->
                        def vec2T "viewportSize" (byF (div uniforms.u_viewportWidth uniforms.u_canvasWidth) canvasSize) <|
                            \viewportSize ->
                                def vec2T "uv" (by (div uv_centered canvasSize) viewportSize) <|
                                    \uv ->
                                        def vec2T "c" (add uniforms.u_zoomCenter uv) <|
                                            \c ->
                                                def floatT "x" (dotted2 c).x <|
                                                    \x ->
                                                        def floatT "y" (dotted2 c).y <|
                                                            \y ->
                                                                def floatT "deltaX" (div uniforms.u_viewportWidth uniforms.u_canvasWidth) <|
                                                                    \deltaX ->
                                                                        def floatT "deltaY" (div uniforms.u_viewportWidth uniforms.u_canvasHeight) <|
                                                                            \deltaY ->
                                                                                def vec3T "px" vec3Zero <|
                                                                                    \px ->
                                                                                        decl vec3T "curr" <|
                                                                                            \curr ->
                                                                                                inner deltaX deltaY x y px curr
                                                                                                    ++ (def floatT "maxDelta" (max_ deltaX deltaY) <|
                                                                                                            \maxDelta ->
                                                                                                                def vec3T
                                                                                                                    "yax"
                                                                                                                    (ternary
                                                                                                                        (eq uniforms.u_drawAxes one)
                                                                                                                        (byF (axis x y maxDelta) (vec3 zero one zero))
                                                                                                                        vec3Zero
                                                                                                                    )
                                                                                                                <|
                                                                                                                    \yax ->
                                                                                                                        def vec3T
                                                                                                                            "xax"
                                                                                                                            (ternary
                                                                                                                                (eq uniforms.u_drawAxes one)
                                                                                                                                (byF (axis y x maxDelta) (vec3 one zero zero))
                                                                                                                                vec3Zero
                                                                                                                            )
                                                                                                                        <|
                                                                                                                            \xax ->
                                                                                                                                [ return <| vec4_3_1 (max_ px (max_ xax yax)) one
                                                                                                                                ]
                                                                                                       )


axisTuple : ( FunDecl, Expression Float -> Expression Float -> Expression Float -> Expression Float )
axisTuple =
    fun3 floatT "axis" (floatT "coord") (floatT "otherCoord") (floatT "maxDelta") <|
        \coord otherCoord maxDelta ->
            def floatT "across" (subtract one <| abs_ <| div coord maxDelta) <|
                \across ->
                    if_ (lt across (float -12))
                        (return zero)
                        :: (def floatT "smallUnit" (pow (float 10) (ceil_ (log10 maxDelta))) <|
                                \smallUnit ->
                                    if_ (ands [ lt across zero, lt (abs_ otherCoord) (by maxDelta (float 2)) ])
                                        (return zero)
                                        :: (def floatT
                                                "unit"
                                                (ternary
                                                    (lt across (float -6))
                                                    (by smallUnit (float 100))
                                                    (ternary
                                                        (lt across (float -0.1))
                                                        (by smallUnit (float 10))
                                                        (by smallUnit (float 5))
                                                    )
                                                )
                                            <|
                                                \unit ->
                                                    def floatT "parallel" (ternary (lt (mod (abs_ otherCoord) unit) maxDelta) one zero) <|
                                                        \parallel ->
                                                            [ return <| max_ zero (max_ across parallel) ]
                                           )
                           )


axis : Expression Float -> Expression Float -> Expression Float -> Expression Float
axis =
    Tuple.second axisTuple


axisDecl : FunDecl
axisDecl =
    Tuple.first axisTuple


toSrc3D : Bool -> String -> Expression.Expression -> File
toSrc3D expandIntervals suffix e =
    [ Tuple.first <|
        fun1 vec3T ("normal" ++ suffix) (vec3T "p") <|
            \p ->
                let
                    dp =
                        dotted3 p
                in
                def floatT "x" dp.x <|
                    \x ->
                        def floatT "y" dp.y <|
                            \y ->
                                def floatT "z" dp.z <|
                                    \z ->
                                        def vec4T "gradient" (expressionToNormalGlsl { x = x, y = y, z = z } e) <|
                                            \gradient ->
                                                [ return <| normalize (dotted4 gradient).yzw ]
    , Tuple.first <|
        fun2 vec2T ("interval" ++ suffix) (mat3T "f") (mat3T "t") <|
            \f t ->
                def vec3T "mn" (min_ (arr f <| int 0) (arr t <| int 0)) <|
                    \mn ->
                        def vec3T "mx" (max_ (arr f <| int 1) (arr t <| int 1)) <|
                            \mx ->
                                let
                                    dmn =
                                        dotted3 mn

                                    dmx =
                                        dotted3 mx
                                in
                                def vec2T "x" (vec2 dmn.x dmx.x) <|
                                    \x ->
                                        def vec2T "y" (vec2 dmn.y dmx.y) <|
                                            \y ->
                                                def vec2T "z" (vec2 dmn.z dmx.z) <|
                                                    \z ->
                                                        [ return <| unknown <| expressionToIntervalGlsl expandIntervals e ]
    ]


main3D : Bool -> List String -> String
main3D rayDifferentials suffixes =
    let
        k =
            if rayDifferentials then
                "0.001"

            else
                "0.0"

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
                float k = """ ++ k ++ """;
                mat3 d = mat3(ray_direction - k * diffs, ray_direction + k * diffs, vec3(0));

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
            vec4 raytrace(vec3 o, mat3 d, float max_distance) {
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
                    mat3 light_direction = mat3(ld - 0.5 * diffs, ld + 0.5 * diffs, vec3(0));
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
            bool bisect""" ++ suffix ++ """(vec3 o, mat3 d, float max_distance, out vec3 found) {
                mat3 from = mat3(o, o, vec3(0));
                mat3 to = from + max_distance * d;
                float ithreshold = """ ++ threshold ++ """;
                int depth = 0;
                int choices = 0;
                for(int it = 0; it < MAX_ITERATIONS; it++) {
                    mat3 midpoint = 0.5 * (from + to);
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
