module UI.Glsl.Code exposing (cexpFunction, constantToGlsl, deindent, dupDecl, expressionToGlsl, gnumDecl, intervalFunctionToGlsl, intervalOperationToGlsl, mainGlsl, straightFunctionToGlsl, straightOperationToGlsl, suffixToBisect, thetaDeltaDecl, threshold, toSrc3D, toSrcContour, toSrcImplicit, toSrcParametric, toSrcPolar, toSrcRelation, toSrcVectorField2D)

import Dict
import Expression exposing (FunctionName(..), KnownFunction(..), PrintExpression(..), toPrintExpression)
import UI.Glsl.Generator as Generator exposing (Expression1, Expression2, Expression3, Expression33, Expression4, ExpressionX, File, FunDecl, Mat3, Statement, Vec2, Vec3, Vec4, abs2, abs4, abs_, add, add2, add4, ands, arr, assign, atan2_, by, by2, by3, byF, ceil_, cos_, cosh, cross, decl, def, div, div2, divF, dot, dotted1, dotted2, dotted3, dotted33, dotted4, eq, exp, fileToGlsl, float, floatCast, floatT, floatToGlsl, fract, fun0, fun1, fun2, fun3, fun4, fwidth, geq, gl_FragColor, gl_FragCoord, gt, hl2rgb, if_, int, intCast, intT, length, leq, log, lt, mat3T, mat3_3_3_3, max3, max4, max_, min_, minusOne, mod, negate2, negate_, nop, normalize, normalize3, one, pow, radians_, return, round_, sign, sin_, sinh, subtract, subtract2, subtract3, subtract4, ternary, ternary3, uniform, unknown, unknownFunDecl, unsafeCall, vec2, vec2T, vec2Zero, vec3, vec3T, vec3Zero, vec4, vec4T, vec4Zero, vec4_1_3, vec4_3_1, voidT, zero)
import UI.Glsl.Model exposing (GlslConstant(..), GlslFunction(..), GlslOperation(..))


type alias Uniforms =
    { u_canvasHeight : Expression1 Float
    , u_canvasWidth : Expression1 Float
    , u_drawAxes : Expression1 Float
    , u_viewportHeight : Expression1 Float
    , u_viewportWidth : Expression1 Float
    , u_whiteLines : Expression1 Float
    , u_zoomCenter : Expression2
    , u_phi : Expression1 Float
    , u_theta : Expression1 Float
    }


constantToGlsl : GlslConstant -> ( FunDecl, Expression2 )
constantToGlsl c =
    case c of
        I ->
            fun0 vec2T "i" <|
                return (vec2 zero one)

        Pi ->
            fun0 vec2T "pi" <|
                return (vec2 (radians_ <| float 180) zero)

        E ->
            fun0 vec2T "e" <|
                return (vec2 (exp one) zero)


cbyTuple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
cbyTuple =
    fun2 vec2T "cby" (vec2T "a") (vec2T "b") <|
        \a b ->
            return <|
                vec2
                    (subtract
                        (by a.x b.x)
                        (by a.y b.y)
                    )
                    (add
                        (by a.x b.y)
                        (by a.y b.x)
                    )


cbyDecl : FunDecl
cbyDecl =
    Tuple.first cbyTuple


gnumTuple : ( FunDecl, ExpressionX xa Float -> Expression4 )
gnumTuple =
    fun1 vec4T "gnum" (floatT "f") <| \f -> return <| vec4 f zero zero zero


gnumDecl : FunDecl
gnumDecl =
    Tuple.first gnumTuple


gnum : ExpressionX xa Float -> Expression4
gnum =
    Tuple.second gnumTuple


dupTuple : ( FunDecl, ExpressionX xa Float -> Expression2 )
dupTuple =
    fun1 vec2T "dup" (floatT "x") <| \x -> return <| vec2 x x


dupDecl : FunDecl
dupDecl =
    Tuple.first dupTuple


dup : ExpressionX xa Float -> Expression2
dup =
    Tuple.second dupTuple


cby : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
cby =
    Tuple.second cbyTuple


cby3Tuple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> ExpressionX xc Vec2 -> Expression2 )
cby3Tuple =
    fun3 vec2T "cby" (vec2T "a") (vec2T "b") (vec2T "c") <|
        \a b c ->
            return <| cby (cby a b) c


cby3Decl : FunDecl
cby3Decl =
    Tuple.first cby3Tuple


cdivTuple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
cdivTuple =
    fun2 vec2T "cdiv" (vec2T "a") (vec2T "b") <|
        \a b ->
            def floatT "k" (div one <| dot b b) <|
                \k ->
                    def floatT "r" (by k <| dot a b) <|
                        \r ->
                            def floatT
                                "i"
                                (by k
                                    (subtract (by a.y b.x) (by a.x b.y))
                                )
                            <|
                                \i ->
                                    return <| vec2 r i


cdivDecl : FunDecl
cdivDecl =
    Tuple.first cdivTuple


cdiv : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
cdiv =
    Tuple.second cdivTuple


log10Tuple : ( FunDecl, ExpressionX xa Float -> Expression1 Float )
log10Tuple =
    fun1 floatT "log10" (floatT "x") <|
        \x -> return <| div (log x) (log <| float 10)


log10Decl : FunDecl
log10Decl =
    Tuple.first log10Tuple


log10 : ExpressionX xa Float -> Expression1 Float
log10 =
    Tuple.second log10Tuple


straightOperationToGlsl : GlslOperation -> List FunDecl
straightOperationToGlsl op =
    case op of
        GlslAddition ->
            []

        GlslMultiplication ->
            [ cbyDecl
            , cby3Decl
            ]

        GlslNegation ->
            []

        GlslDivision ->
            [ cdivDecl ]

        GlslPower ->
            [ cpowDecl ]

        GlslRelations ->
            []


cpowTuple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
cpowTuple =
    fun2 vec2T "cpow" (vec2T "w") (vec2T "z") <|
        \w z ->
            return <|
                ternary
                    (ands
                        [ geq w.x zero
                        , eq w.y zero
                        , eq z.y zero
                        ]
                    )
                    (vec2 (pow w.x z.x) zero)
                    (cexp <| cby (cln w) z)


cpowDecl : FunDecl
cpowDecl =
    Tuple.first cpowTuple


cpow : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
cpow =
    Tuple.second cpowTuple


inegCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
inegCouple =
    fun1 vec2T "ineg" (vec2T "v") <|
        \v ->
            return <| vec2 (negate_ v.y) (negate_ v.x)


inegDecl : FunDecl
inegDecl =
    Tuple.first inegCouple


ineg : ExpressionX xa Vec2 -> Expression2
ineg =
    Tuple.second inegCouple


gnegCouple : ( FunDecl, ExpressionX xa Vec4 -> Expression4 )
gnegCouple =
    fun1 vec4T "gneg" (vec4T "v") <| \v -> return <| negate_ v


gnegDecl : FunDecl
gnegDecl =
    Tuple.first gnegCouple


gneg : ExpressionX xa Vec4 -> Expression4
gneg =
    Tuple.second gnegCouple


isquareCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
isquareCouple =
    fun1 vec2T "isquare" (vec2T "z") <|
        \z ->
            def vec2T "s" (by z z) <|
                \s ->
                    def floatT "mx" (max_ s.x s.y) <|
                        \mx ->
                            if_
                                (ands [ leq z.x zero, geq z.y zero ])
                                (return <| vec2 zero mx)
                                (def floatT "mn" (min_ s.x s.y) <|
                                    \mn ->
                                        return <| vec2 mn mx
                                )


isquareDecl : FunDecl
isquareDecl =
    Tuple.first isquareCouple


isquare : ExpressionX xa Vec2 -> Expression2
isquare =
    Tuple.second isquareCouple


gsquareCouple : ( FunDecl, ExpressionX xa Vec4 -> Expression4 )
gsquareCouple =
    fun1 vec4T "gsquare" (vec4T "z") <| \z -> return <| vec4_1_3 (by z.x z.x) z.yzw


gsquareDecl : FunDecl
gsquareDecl =
    Tuple.first gsquareCouple


gsquare : ExpressionX xa Vec4 -> Expression4
gsquare =
    Tuple.second gsquareCouple


gbyCouple : ( FunDecl, ExpressionX xa Vec4 -> ExpressionX xb Vec4 -> Expression4 )
gbyCouple =
    fun2 vec4T "gby" (vec4T "l") (vec4T "r") <| \l r -> return <| vec4_1_3 (by l.x r.x) (add (byF l.x r.yzw) (byF r.x l.yzw))


gbyDecl : FunDecl
gbyDecl =
    Tuple.first gbyCouple


gby : ExpressionX xa Vec4 -> ExpressionX xb Vec4 -> Expression4
gby =
    Tuple.second gbyCouple


ibyCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
ibyCouple =
    fun2 vec2T "iby" (vec2T "l") (vec2T "r") <|
        \l r ->
            def floatT "a" (by l.x r.x) <|
                \a ->
                    def floatT "b" (by l.x r.y) <|
                        \b ->
                            def floatT "c" (by l.y r.x) <|
                                \c ->
                                    def floatT "d" (by l.y r.y) <|
                                        \d ->
                                            def floatT "mn" (min_ (min_ a b) (min_ c d)) <|
                                                \mn ->
                                                    def floatT "mx" (max_ (max_ a b) (max_ c d)) <|
                                                        \mx ->
                                                            return <| vec2 mn mx


ibyDecl : FunDecl
ibyDecl =
    Tuple.first ibyCouple


iby : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
iby =
    Tuple.second ibyCouple


iinverseCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
iinverseCouple =
    fun1 vec2T "iinverse" (vec2T "y") <|
        \y ->
            return <|
                ternary
                    (ands [ leq y.x zero, geq y.y zero ])
                    (vec2 (div minusOne zero) (div one zero))
                    (ternary
                        (eq y.y zero)
                        (vec2 (div minusOne zero) (div one y.x))
                        (ternary
                            (eq y.x zero)
                            (vec2 (div one y.y) (div one zero))
                            (vec2 (div one y.y) (div one y.x))
                        )
                    )


iinverseDecl : FunDecl
iinverseDecl =
    Tuple.first iinverseCouple


iinverse : ExpressionX xa Vec2 -> Expression2
iinverse =
    Tuple.second iinverseCouple


idivCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
idivCouple =
    fun2 vec2T "idiv" (vec2T "l") (vec2T "r") <|
        \l r ->
            return <| iby l (iinverse r)


idivDecl : FunDecl
idivDecl =
    Tuple.first idivCouple


idiv : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
idiv =
    Tuple.second idivCouple


gdivCouple : ( FunDecl, ExpressionX xa Vec4 -> ExpressionX xb Vec4 -> Expression4 )
gdivCouple =
    fun2 vec4T "gdiv" (vec4T "l") (vec4T "r") <|
        \l r ->
            return <|
                vec4_1_3 (div l.x r.x)
                    (divF
                        (subtract (byF r.x l.yzw) (byF l.x r.yzw))
                        (pow r.x <| float 2)
                    )


gdivDecl : FunDecl
gdivDecl =
    Tuple.first gdivCouple


gdiv : ExpressionX xa Vec4 -> ExpressionX xb Vec4 -> Expression4
gdiv =
    Tuple.second gdivCouple


ipowFICouple : ( FunDecl, ExpressionX xa Float -> ExpressionX xb Int -> Expression1 Float )
ipowFICouple =
    fun2 floatT "ipow" (floatT "b") (intT "e") <|
        \b e ->
            def floatT "fe" (floatCast e) <|
                \fe ->
                    return <|
                        ternary
                            (eq (mod fe (float 2.0)) zero)
                            (pow (abs_ b) fe)
                            (by b <| pow (abs_ b) (subtract fe one))


ipowFIDecl : FunDecl
ipowFIDecl =
    Tuple.first ipowFICouple


ipowFI : ExpressionX xa Float -> ExpressionX xb Int -> Expression1 Float
ipowFI =
    Tuple.second ipowFICouple


ipowICouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Int -> Expression2 )
ipowICouple =
    fun2 vec2T "ipow" (vec2T "b") (intT "e") <|
        \b e ->
            if_
                (eq e (int 0))
                (return <| vec2 one one)
                (if_
                    (eq e (int 1))
                    (return b)
                    (def floatT "xe" (ipowFI b.x e) <|
                        \xe ->
                            def floatT "ye" (ipowFI b.y e) <|
                                \ye ->
                                    def floatT "mn" (min_ xe ye) <|
                                        \mn ->
                                            def floatT "mx" (max_ xe ye) <|
                                                \mx ->
                                                    return <|
                                                        ternary
                                                            (ands
                                                                [ eq (mod (floatCast e) (float 2)) zero
                                                                , leq b.x zero
                                                                , geq b.y zero
                                                                ]
                                                            )
                                                            (vec2 (min_ zero mn) (max_ zero mx))
                                                            (vec2 mn mx)
                    )
                )


ipowIDecl : FunDecl
ipowIDecl =
    Tuple.first ipowICouple


ipowI : ExpressionX xa Vec2 -> ExpressionX xb Int -> Expression2
ipowI =
    Tuple.second ipowICouple


ipowCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
ipowCouple =
    fun2 vec2T "ipow" (vec2T "b") (vec2T "e") <|
        \b e ->
            return <|
                ternary
                    (ands
                        [ lt (subtract e.y e.x) (float 0.000001)
                        , lt (abs_ <| subtract e.x (round_ e.x)) (float 0.000001)
                        ]
                    )
                    (ipowI b <| intCast e.x)
                    (iexp <| iby (iln b) e)


ipowDecl : FunDecl
ipowDecl =
    Tuple.first ipowCouple


ipow : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
ipow =
    Tuple.second ipowCouple


gpowICouple : ( FunDecl, ExpressionX xa Vec4 -> ExpressionX xb Int -> Expression4 )
gpowICouple =
    fun2 vec4T "gpow" (vec4T "b") (intT "e") <|
        \b e ->
            return <|
                vec4_1_3
                    (pow b.x <| floatCast e)
                    (byF (by b.x <| floatCast <| subtract e (int 1)) b.yzw)


gpowIDecl : FunDecl
gpowIDecl =
    Tuple.first gpowICouple


gpowI : ExpressionX xa Vec4 -> ExpressionX xb Int -> Expression4
gpowI =
    Tuple.second gpowICouple


gpowCouple : ( FunDecl, ExpressionX xa Vec4 -> ExpressionX xb Vec4 -> Expression4 )
gpowCouple =
    fun2 vec4T "gpow" (vec4T "b") (vec4T "e") <|
        \b e ->
            def intT "ie" (intCast e.x) <|
                \ie ->
                    if_ (ands [ eq (floatCast ie) e.x, eq e.y zero, eq e.z zero, eq e.w zero ])
                        (return <| gpowI b ie)
                        (return <| gexp <| gby (gln b) e)


gpowDecl : FunDecl
gpowDecl =
    Tuple.first gpowCouple


gpow : ExpressionX xa Vec4 -> ExpressionX xb Vec4 -> Expression4
gpow =
    Tuple.second gpowCouple


iexpCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
iexpCouple =
    fun1 vec2T "iexp" (vec2T "z") <| \z -> return <| vec2 (exp z.x) (exp z.y)


iexpDecl : FunDecl
iexpDecl =
    Tuple.first iexpCouple


iexp : ExpressionX xa Vec2 -> Expression2
iexp =
    Tuple.second iexpCouple


gexpCouple : ( FunDecl, ExpressionX xa Vec4 -> Expression4 )
gexpCouple =
    fun1 vec4T "gexp" (vec4T "z") <| \z -> return <| vec4_1_3 z.x (byF (exp z.x) z.yzw)


gexpDecl : FunDecl
gexpDecl =
    Tuple.first gexpCouple


gexp : ExpressionX xa Vec4 -> Expression4
gexp =
    Tuple.second gexpCouple


ilnCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
ilnCouple =
    fun1 vec2T "iln" (vec2T "z") <| \z -> return <| vec2 (log z.x) (log z.y)


ilnDecl : FunDecl
ilnDecl =
    Tuple.first ilnCouple


iln : ExpressionX xa Vec2 -> Expression2
iln =
    Tuple.second ilnCouple


glnCouple : ( FunDecl, ExpressionX xa Vec4 -> Expression4 )
glnCouple =
    fun1 vec4T "gln" (vec4T "z") <| \z -> return <| vec4_1_3 (log z.x) (divF z.yzw z.x)


glnDecl : FunDecl
glnDecl =
    Tuple.first glnCouple


gln : ExpressionX xa Vec4 -> Expression4
gln =
    Tuple.second glnCouple


csinCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
csinCouple =
    fun1 vec2T "csin" (vec2T "z") <|
        \z ->
            def floatT "s" (sin_ z.x) <|
                \s ->
                    return <|
                        ternary
                            (eq z.y zero)
                            (vec2 s zero)
                            (vec2
                                (by s (cosh z.y))
                                (by (cos_ z.x) (sinh z.y))
                            )


csinDecl : FunDecl
csinDecl =
    Tuple.first csinCouple


csin : ExpressionX xa Vec2 -> Expression2
csin =
    Tuple.second csinCouple


ccosCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
ccosCouple =
    fun1 vec2T "ccos" (vec2T "z") <|
        \z ->
            def floatT "c" (cos_ z.x) <|
                \c ->
                    return <|
                        ternary
                            (eq z.y zero)
                            (vec2 c zero)
                            (vec2
                                (by c (cosh z.y))
                                (by (sin_ z.x) (sinh z.y))
                            )


ccosDecl : FunDecl
ccosDecl =
    Tuple.first ccosCouple


ccos : ExpressionX xa Vec2 -> Expression2
ccos =
    Tuple.second ccosCouple


intervalOperationToGlsl : GlslOperation -> String
intervalOperationToGlsl op =
    case op of
        GlslAddition ->
            ""

        GlslNegation ->
            fileToGlsl [ inegDecl, gnegDecl ]

        GlslMultiplication ->
            fileToGlsl [ ibyDecl, gbyDecl ]

        GlslDivision ->
            fileToGlsl [ iinverseDecl, idivDecl, gdivDecl ]

        GlslPower ->
            fileToGlsl [ ipowFIDecl, ipowIDecl, ipowDecl, gpowIDecl, gpowDecl ]

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
                    \x -> return <| by (float 0.5) <| subtract (exp x) (exp <| negate_ x)
            ]

        Cosh11 ->
            [ Tuple.first <|
                fun1 floatT "cosh" (floatT "x") <|
                    \x -> return <| by (float 0.5) <| add (exp x) (exp <| negate_ x)
            ]

        Tanh11 ->
            [ Tuple.first <|
                fun1 floatT "tanh" (floatT "x") <|
                    \x ->
                        if_ (gt (abs_ x) (float 10))
                            (return <| sign x)
                            (def floatT "p" (exp x) <|
                                \p ->
                                    def floatT "m" (exp <| negate_ x) <|
                                        \m ->
                                            return <| div (subtract p m) (add p m)
                            )
            ]

        Sin22 ->
            [ csinDecl ]

        Cos22 ->
            [ ccosDecl ]

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
                return cdiv(csin(z), ccos(z));
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
                vec2 s = csqrt(vec2(1, 0) - cby(z, z));
                vec2 arg = s - cby(vec2(0, 1), z);
                return cby(vec2(0, 1), cln(arg));
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
                vec2 iz = cby(vec2(0, 1), z);
                vec2 l = cdiv(o + iz, o - iz);
                return -0.5 * cby(vec2(0, 1), cln(l));
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
                return cdiv(p - m, p + m);
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
                return cby(z, z);
            }
            """ } ]

        Ln22 ->
            [ clnDecl ]

        Log1022 ->
            [ unknownFunDecl { name = "clog10", type_ = "TODO", body = """
            vec2 clog10(vec2 z) {
                return cdiv(cln(z), vec2(log(10.0), 0));
            }
            """ } ]

        Exp22 ->
            [ cexpDecl ]

        Re22 ->
            [ creDecl ]

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


cexpTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
cexpTuple =
    fun1 vec2T "cexp" (vec2T "z") cexpFunction


cexpFunction : Expression2 -> Statement Vec2
cexpFunction z =
    if_ (eq z.y zero)
        (return <| vec2 (exp z.x) zero)
        (return <| vec2 (by (cos_ z.y) (exp z.x)) (by (sin_ z.y) (exp z.x)))


cexpDecl : FunDecl
cexpDecl =
    Tuple.first cexpTuple


cexp : ExpressionX xa Vec2 -> Expression2
cexp =
    Tuple.second cexpTuple


clnTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
clnTuple =
    fun1 vec2T "cln" (vec2T "z") <|
        \z ->
            if_
                (ands [ eq z.y zero, geq z.x zero ])
                (return <| vec2 (log z.x) zero)
                (def floatT "px" (length z) <|
                    \px ->
                        def floatT "py" (atan2_ z.y z.x) <|
                            \py ->
                                return <| vec2 (log px) py
                )


creTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
creTuple =
    fun1 vec2T "cre" (vec2T "z") <| \z -> return <| vec2 z.x zero


creDecl : FunDecl
creDecl =
    Tuple.first creTuple


cre : ExpressionX xa Vec2 -> Expression2
cre =
    Tuple.second creTuple


clnDecl : FunDecl
clnDecl =
    Tuple.first clnTuple


cln : ExpressionX xa Vec2 -> Expression2
cln =
    Tuple.second clnTuple


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
            fileToGlsl [ iexpDecl, gexpDecl ]

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
            fileToGlsl [ ilnDecl, glnDecl ]

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
            fileToGlsl [ isquareDecl, gsquareDecl ]

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
        vec2 complex = """ ++ Generator.expressionToGlsl (expressionToGlsl [ ( "x", dotted1 <| unknown "x" ), ( "y", dotted1 <| unknown "y" ) ] e).base ++ """;
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
        vec2 complex = """ ++ Generator.expressionToGlsl (expressionToGlsl [ ( "x", dotted1 <| unknown "x" ), ( "y", dotted1 <| unknown "y" ), ( "r", dotted1 <| unknown "r" ), ( "t", dotted1 <| unknown "t" ) ] e).base ++ """;
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


toSrcParametric : String -> Expression.Expression -> String
toSrcParametric suffix e =
    """
    vec2 interval""" ++ suffix ++ """(vec2 p, float from, float to) {
        vec2 x = vec2(p.x,p.x);
        vec2 y = vec2(p.y,p.y);
        vec2 t = from < to ? vec2(from, to) : vec2(to, from);
        return """ ++ Generator.expressionToGlsl (expressionToIntervalGlsl (toPrintExpression e)).base ++ """;
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


uniforms : Uniforms
uniforms =
    { u_canvasHeight = uniform floatT "u_canvasHeight"
    , u_canvasWidth = uniform floatT "u_canvasWidth"
    , u_drawAxes = uniform floatT "u_drawAxes"
    , u_viewportHeight = uniform floatT "u_viewportHeight"
    , u_viewportWidth = uniform floatT "u_viewportWidth"
    , u_whiteLines = uniform floatT "u_whiteLines"
    , u_zoomCenter = uniform vec2T "u_zoomCenter"
    , u_phi = uniform floatT "u_phi"
    , u_theta = uniform floatT "u_theta"
    }


thetaDeltaCouple : ( FunDecl, ExpressionX xa Float -> Expression1 Float )
thetaDeltaCouple =
    fun1 floatT "thetaDelta" (floatT "theta") <|
        \theta ->
            if_ (lt uniforms.u_whiteLines one)
                (return <| float 100)
                (def floatT "thetaSix" (add (by theta uniforms.u_whiteLines) (float 0.5)) <|
                    \thetaSix ->
                        def floatT "thetaNeigh" (float 0.05) <|
                            \thetaNeigh ->
                                return <| divF (abs_ (subtract (fract thetaSix) (float 0.5))) thetaNeigh
                )


thetaDeltaDecl : FunDecl
thetaDeltaDecl =
    Tuple.first thetaDeltaCouple


thetaDelta : ExpressionX xa Float -> Expression1 Float
thetaDelta =
    Tuple.second thetaDeltaCouple


toSrcVectorField2D : String -> Expression.Expression -> Expression.Expression -> String
toSrcVectorField2D suffix x y =
    """
    vec2 vector""" ++ suffix ++ """(float x, float y) {
        vec2 xv = """ ++ Generator.expressionToGlsl (expressionToGlsl [ ( "x", dotted1 <| unknown "x" ), ( "y", dotted1 <| unknown "y" ) ] x).base ++ """;
        vec2 yv = """ ++ Generator.expressionToGlsl (expressionToGlsl [ ( "x", dotted1 <| unknown "x" ), ( "y", dotted1 <| unknown "y" ) ] y).base ++ """;
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
        vec2 z = """ ++ Generator.expressionToGlsl (expressionToGlsl [ ( "x", dotted1 <| unknown "x" ), ( "y", dotted1 <| unknown "y" ) ] e).base ++ """;

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
    fileToGlsl
        [ Tuple.first <|
            fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <|
                \_ _ x y ->
                    def vec2T "complex" (expressionToGlsl [ ( "x", x ), ( "y", y ) ] e) <|
                        \complex ->
                            return <|
                                ternary
                                    (ands
                                        [ gt complex.x zero
                                        , lt (abs_ complex.y) (float epsilon)
                                        ]
                                    )
                                    (vec3 (float 0.8) (float 0.5) (float 0.5))
                                    vec3Zero
        ]


epsilon : Float
epsilon =
    0.00001


expressionToGlsl : List ( String, Expression1 Float ) -> Expression.Expression -> Expression2
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
                            dotted2 <| unknown <| "Variable " ++ v ++ " is undefined"

                PInteger v ->
                    vec2 (float <| toFloat v) zero

                PFloat f ->
                    vec2 (float f) zero

                PNegate expression ->
                    negate2 <| go expression

                PAdd l (PNegate r) ->
                    subtract2 (go l) (go r)

                PAdd l r ->
                    add2 (go l) (go r)

                PRel op l r ->
                    if op == "<=" || op == "<" then
                        subtract2 (go r) (go l)

                    else if op == "=" then
                        abs2 <| subtract (go r) (go l)

                    else
                        subtract2 (go l) (go r)

                PBy l r ->
                    cby (go l) (go r)

                PDiv l r ->
                    cdiv (go l) (go r)

                PPower (PVariable v) (PInteger 2) ->
                    case Dict.get v ctx of
                        Just w ->
                            vec2 (by w w) zero

                        Nothing ->
                            dotted2 <| unknown <| "Variable " ++ v ++ " is undefined"

                PPower l r ->
                    cpow (go l) (go r)

                PApply (KnownFunction Simplify) [ e ] ->
                    go e

                PApply name ex ->
                    if List.any (\( _, v ) -> name == KnownFunction v) Expression.variadicFunctions then
                        case List.map (go >> .base) ex of
                            [] ->
                                vec2Zero

                            head :: tail ->
                                dotted2 (List.foldl (\e a -> unsafeCall ("c" ++ Expression.functionNameToString name) [ a, e ]) head tail)

                    else
                        dotted2 <| unsafeCall ("c" ++ Expression.functionNameToString name) (List.map (go >> .base) ex)

                PList es ->
                    dotted2 <| unsafeCall ("vec" ++ String.fromInt (List.length es)) (List.map (go >> .base) es)

                PReplace var e ->
                    go (Expression.pfullSubstitute var e)

                -- If this happens, it's too late
                PLambda _ _ ->
                    vec2Zero
    in
    toPrintExpression >> go


expressionToIntervalGlsl : PrintExpression -> Expression2
expressionToIntervalGlsl expr =
    let
        unsafeApply name ex =
            let
                _ =
                    Debug.todo
            in
            dotted2 <| unsafeCall name (List.map (go >> .base) ex)

        go =
            expressionToIntervalGlsl
    in
    case expr of
        PVariable "pi" ->
            dup <| radians_ (float 180)

        PVariable "e" ->
            dup <| exp one

        PVariable v ->
            dotted2 <| unknown v

        PInteger v ->
            dup <| float <| toFloat v

        PFloat f ->
            dup <| float f

        PNegate expression ->
            ineg <| go expression

        PAdd l r ->
            add2 (go l) (go r)

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
            unsafeApply name [ l, r ]

        PBy l r ->
            iby (go l) (go r)

        PDiv l r ->
            idiv (go l) (go r)

        PPower (PVariable "i") (PInteger 2) ->
            dup <| float -1

        PPower l (PInteger 2) ->
            isquare (go l)

        PPower l (PFloat f) ->
            if f == 2.0 then
                isquare (go l)

            else
                ipow (go l) (go <| PFloat f)

        PPower l (PInteger ri) ->
            ipow (go l) (go <| PInteger ri)

        PPower l r ->
            ipow (go l) (go r)

        PApply name ex ->
            if List.any (\( _, v ) -> name == KnownFunction v) Expression.variadicFunctions then
                case List.map go ex of
                    [] ->
                        vec2 zero zero

                    head :: tail ->
                        dotted2 <| List.foldl (\e a -> unsafeCall ("i" ++ Expression.functionNameToString name) [ a, e.base ]) head.base tail

            else if name == KnownFunction Sin || name == KnownFunction Cos then
                unsafeApply ("i" ++ Expression.functionNameToString name) ex

            else
                unsafeApply ("i" ++ Expression.functionNameToString name) ex

        PList es ->
            unsafeApply ("vec" ++ String.fromInt (List.length es)) es

        PReplace var e ->
            go (Expression.pfullSubstitute var e)

        -- If this happens, it's too late
        PLambda _ _ ->
            vec2 zero zero


expressionToNormalGlsl : { x : Expression1 Float, y : Expression1 Float, z : Expression1 Float } -> Expression.Expression -> Expression4
expressionToNormalGlsl { x, y, z } =
    let
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
                    gneg (go expression)

                PAdd l r ->
                    add4 (go l) (go r)

                PRel op l r ->
                    if op == "<=" || op == "<" then
                        subtract4 (go r) (go l)

                    else if op == "=" then
                        abs4 <| subtract (go r) (go l)

                    else
                        subtract4 (go l) (go r)

                PBy l r ->
                    gby (go l) (go r)

                PDiv l r ->
                    dotted4 (gdiv (go l) (go r)).base

                PPower l (PInteger 2) ->
                    gsquare (go l)

                PPower l (PInteger ri) ->
                    gpowI (go l) (int ri)

                PPower l r ->
                    gpow (go l) (go r)

                PApply name ex ->
                    if List.any (\( _, v ) -> name == KnownFunction v) Expression.variadicFunctions then
                        case List.map (go >> .base) ex of
                            [] ->
                                vec4Zero

                            head :: tail ->
                                dotted4 <| List.foldl (\e a -> unsafeCall ("g" ++ Expression.functionNameToString name) [ a, e ]) head tail

                    else
                        dotted4 (unsafeCall ("g" ++ Expression.functionNameToString name) (List.map (go >> .base) ex))

                PList es ->
                    dotted4 (unsafeCall ("vec" ++ String.fromInt (List.length es)) (List.map (go >> .base) es))

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
    Bool
    ->
        List
            { call : Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression3
            , color : Bool
            }
    -> List String
    -> String
mainGlsl rayDifferentials pixel2Def pixel3Def =
    case ( pixel2Def, pixel3Def ) of
        ( _, [] ) ->
            let
                ( pixel2Decl, pixel2 ) =
                    main2D pixel2Def
            in
            fileToGlsl <|
                pixel2Decl
                    ++ [ Tuple.first <|
                            fun0 voidT "main" <|
                                assign gl_FragColor pixel2 nop
                       ]

        ( [], _ ) ->
            let
                ( pixel3Decl, pixel3 ) =
                    main3D rayDifferentials pixel3Def
            in
            pixel3Decl
                ++ "\n\n"
                ++ fileToGlsl
                    [ Tuple.first <|
                        fun0 voidT "main" <|
                            assign gl_FragColor pixel3 nop
                    ]

        _ ->
            let
                ( pixel2Decl, pixel2 ) =
                    main2D pixel2Def

                ( pixel3Decl, pixel3 ) =
                    main3D rayDifferentials pixel3Def
            in
            fileToGlsl pixel2Decl
                ++ "\n\n"
                ++ pixel3Decl
                ++ fileToGlsl
                    [ Tuple.first <|
                        fun0 voidT "main" <|
                            assign gl_FragColor (max4 pixel2 pixel3) nop
                    ]


main2D :
    List
        { call : Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression3
        , color : Bool
        }
    -> ( File, Expression4 )
main2D pixels =
    let
        ( pixel2Decl, pixel2 ) =
            pixel2Tuple pixels
    in
    ( [ log10Decl
      , axisDecl
      , pixel2Decl
      ]
    , pixel2
    )


pixel2Tuple :
    List
        { call : Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression3
        , color : Bool
        }
    -> ( FunDecl, Expression4 )
pixel2Tuple pixels =
    let
        inner deltaX deltaY x y px curr cont =
            List.foldl (\( i, p ) -> addPixel deltaX deltaY x y px curr i p) cont (List.indexedMap Tuple.pair pixels)
    in
    fun0 vec4T "pixel2" <|
        def vec2T "canvasSize" (vec2 uniforms.u_canvasWidth uniforms.u_canvasHeight) <|
            \canvasSize ->
                def vec2T "uv_centered" (subtract2 gl_FragCoord.xy (byF (float 0.5) canvasSize)) <|
                    \uv_centered ->
                        def vec2T "viewportSize" (byF (div uniforms.u_viewportWidth uniforms.u_canvasWidth) canvasSize) <|
                            \viewportSize ->
                                def vec2T "uv" (by2 (div2 uv_centered canvasSize) viewportSize) <|
                                    \uv ->
                                        def vec2T "c" (add uniforms.u_zoomCenter uv) <|
                                            \c ->
                                                def floatT "x" c.x <|
                                                    \x ->
                                                        def floatT "y" c.y <|
                                                            \y ->
                                                                def floatT "deltaX" (div uniforms.u_viewportWidth uniforms.u_canvasWidth) <|
                                                                    \deltaX ->
                                                                        def floatT "deltaY" (div uniforms.u_viewportWidth uniforms.u_canvasHeight) <|
                                                                            \deltaY ->
                                                                                def vec3T "px" vec3Zero <|
                                                                                    \px ->
                                                                                        decl vec3T "curr" <|
                                                                                            \curr ->
                                                                                                inner deltaX deltaY x y px curr <|
                                                                                                    def floatT "maxDelta" (max_ deltaX deltaY) <|
                                                                                                        \maxDelta ->
                                                                                                            def vec3T "yax" (ternary3 (eq uniforms.u_drawAxes one) (byF (axis x y maxDelta) (vec3 zero one zero)) vec3Zero) <|
                                                                                                                \yax ->
                                                                                                                    def vec3T "xax" (ternary3 (eq uniforms.u_drawAxes one) (byF (axis y x maxDelta) (vec3 one zero zero)) vec3Zero) <|
                                                                                                                        \xax ->
                                                                                                                            return <| vec4_3_1 (max3 px (max3 xax yax)) one


addPixel :
    Expression1 Float
    -> Expression1 Float
    -> Expression1 Float
    -> Expression1 Float
    -> Expression3
    -> Expression3
    -> Int
    ->
        { call : Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression3
        , color : Bool
        }
    -> (Statement f -> Statement f)
addPixel deltaX deltaY x y px curr i { call, color } next =
    let
        k =
            if color then
                let
                    h =
                        float <| (toFloat (i + 2) / pi)
                in
                by3 (hl2rgb h (float 0.5)) (call deltaX deltaY x y)

            else
                call deltaX deltaY x y
    in
    assign curr k <|
        assign px (ternary3 (eq curr vec3Zero) px curr) next


axisTuple : ( FunDecl, Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression1 Float )
axisTuple =
    fun3 floatT "axis" (floatT "coord") (floatT "otherCoord") (floatT "maxDelta") <|
        \coord otherCoord maxDelta ->
            def floatT "across" (subtract one <| abs_ <| div coord maxDelta) <|
                \across ->
                    if_ (lt across (float -12))
                        (return zero)
                        (def floatT "smallUnit" (pow (float 10) (ceil_ (log10 maxDelta))) <|
                            \smallUnit ->
                                if_ (ands [ lt across zero, lt (abs_ otherCoord) (by maxDelta (float 2)) ])
                                    (return zero)
                                    (def floatT
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
                                                    return <| max_ zero (max_ across parallel)
                                    )
                        )


axis : Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression1 Float
axis =
    Tuple.second axisTuple


axisDecl : FunDecl
axisDecl =
    Tuple.first axisTuple


toSrc3D : String -> Expression.Expression -> File
toSrc3D suffix e =
    [ Tuple.first <|
        fun1 vec3T ("normal" ++ suffix) (vec3T "p") <|
            \p ->
                def floatT "x" p.x <|
                    \x ->
                        def floatT "y" p.y <|
                            \y ->
                                def floatT "z" p.z <|
                                    \z ->
                                        def vec4T "gradient" (expressionToNormalGlsl { x = x, y = y, z = z } e) <|
                                            \gradient ->
                                                return <| normalize gradient.yzw
    , Tuple.first <|
        fun2 vec2T ("interval" ++ suffix) (mat3T "f") (mat3T "t") <|
            \f t ->
                def vec3T "mn" (min_ (arr f <| int 0) (arr t <| int 0)) <|
                    \mn ->
                        def vec3T "mx" (max_ (arr f <| int 1) (arr t <| int 1)) <|
                            \mx ->
                                def vec2T "x" (vec2 mn.x mx.x) <|
                                    \_ ->
                                        def vec2T "y" (vec2 mn.y mx.y) <|
                                            \_ ->
                                                def vec2T "z" (vec2 mn.z mx.z) <|
                                                    \_ ->
                                                        return <| expressionToIntervalGlsl <| toPrintExpression e
    ]


main3D : Bool -> List String -> ( String, Expression4 )
main3D rayDifferentials suffixes =
    let
        kValue =
            if rayDifferentials then
                0.001

            else
                0.0

        ( block, pixel3 ) =
            fun0 vec4T "pixel3" <|
                (def floatT "eye_dist" (byF (float 2) uniforms.u_viewportWidth) <|
                    \eyeDist ->
                        def vec2T "canvas_size" (vec2 uniforms.u_canvasWidth uniforms.u_canvasHeight) <|
                            \canvasSize ->
                                def vec2T "uv_centered" (subtract gl_FragCoord.xy <| byF (float 0.5) canvasSize) <|
                                    \uvCentered ->
                                        def vec2T "uv_normalized" (byF (div one uniforms.u_canvasHeight) uvCentered) <|
                                            \uvNormalized ->
                                                def floatT "t" (add uniforms.u_theta <| float 0.58) <|
                                                    \t ->
                                                        def floatT "p" (by (float -2.0) uniforms.u_phi) <|
                                                            \p ->
                                                                def vec3T "eye" (eyePosition t p) <|
                                                                    \eye ->
                                                                        def vec3T "target" vec3Zero <|
                                                                            \target ->
                                                                                def vec3T "to_target" (normalize <| subtract target eye) <|
                                                                                    \toTarget ->
                                                                                        def vec3T "across" (normalize <| cross toTarget <| vec3 zero zero one) <|
                                                                                            \across ->
                                                                                                def vec3T "up" (normalize <| cross across toTarget) <|
                                                                                                    \up ->
                                                                                                        def vec3T "canvas_center" (add eye toTarget) <|
                                                                                                            \canvasCenter ->
                                                                                                                def vec3T "canvas_point" (add (add canvasCenter <| byF uvNormalized.x across) <| byF uvNormalized.y up) <|
                                                                                                                    \canvasPoint ->
                                                                                                                        def vec3T "ray_direction" (normalize3 <| subtract canvasPoint eye) <|
                                                                                                                            \rayDirection ->
                                                                                                                                def vec3T "diffs" (abs_ <| fwidth rayDirection) <|
                                                                                                                                    \diffs ->
                                                                                                                                        def floatT "k" (float kValue) <|
                                                                                                                                            \k ->
                                                                                                                                                def mat3T "d" (dValue rayDirection k diffs) <|
                                                                                                                                                    \d ->
                                                                                                                                                        def floatT "max_distance" (by (float 100) eyeDist) <|
                                                                                                                                                            \maxDistance ->
                                                                                                                                                                return <| raytraceF canvasPoint d maxDistance
                )

        dValue : ExpressionX a Vec3 -> ExpressionX b Float -> ExpressionX c Vec3 -> Expression33
        dValue rayDirection k diffs =
            mat3_3_3_3
                (subtract3 rayDirection <| byF k diffs)
                (add rayDirection <| byF k diffs)
                vec3Zero

        ( raytraceSrc, raytraceF ) =
            raytrace suffixes

        eyePosition t p =
            byF (dotted1 <| unknown "eye_dist") <|
                normalize <|
                    vec3
                        (by (cos_ t) (sin_ p))
                        (by (cos_ t) (negate_ <| cos_ p))
                        (sin_ t)
    in
    ( deindent 12 <| raytraceSrc ++ fileToGlsl [ block ], pixel3 )


raytrace : List String -> ( String, ExpressionX a Vec3 -> ExpressionX b Mat3 -> Expression1 Float -> Expression4 )
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

        src =
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
    in
    ( src
    , \o d maxDistance ->
        dotted4 <|
            unsafeCall "raytrace"
                [ Generator.uNsAfEtYpEcAsT o.base
                , Generator.uNsAfEtYpEcAsT d.base
                , Generator.uNsAfEtYpEcAsT maxDistance.base
                ]
    )


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
