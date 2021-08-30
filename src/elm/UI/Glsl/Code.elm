module UI.Glsl.Code exposing (atanPlusDecl, cexpFunction, constantToGlsl, deindent, dupDecl, expressionToGlsl, gnumDecl, intervalFunctionToGlsl, intervalOperationToGlsl, mainGlsl, straightFunctionToGlsl, straightOperationToGlsl, suffixToBisect, thetaDeltaDecl, threshold, toSrc3D, toSrcContour, toSrcImplicit, toSrcParametric, toSrcPolar, toSrcRelation, toSrcVectorField2D)

import Dict
import Expression exposing (FunctionName(..), KnownFunction(..), PrintExpression(..), RelationOperation(..), functionNameToString, toPrintExpression)
import UI.Glsl.Generator as Generator exposing (Constant, Expression1, Expression2, Expression3, Expression33, Expression4, ExpressionX, File, FunDecl, Mat3, Statement, Vec2, Vec3, Vec4, abs2, abs4, abs_, add, add2, add4, adds3, and, ands, arr, assign, assignAdd, assignBy, atan2_, atan_, boolT, by, by2, by3, byF, ceil_, constant, cos_, cosh, cross, decl, def, def2, def3, def4, def5, def6, div, div2, divConst, divF, dot, dotted1, dotted2, dotted4, eq, exp, expr, fileToGlsl, float, floatCast, floatT, floatToGlsl, floor_, for, forLeq, fract, fun0, fun1, fun2, fun3, fun4, fun5, fwidth, geq, gl_FragColor, gl_FragCoord, gt, hl2rgb, ifElse, if_, int, intCast, intT, length, leq, log, log2, lt, mat3T, mat3_3_3_3, max3, max4, max_, min_, minusOne, mix, mod, negate2, negate_, neq, normalize, normalize3, one, or, ors, postfixPlus, pow, radians_, return, round_, sign, sin_, sinh, smoothstep, sqrt_, subtract, subtract2, subtract3, subtract4, tan_, ternary, ternary3, uniform, unknown, unknownFunDecl, unknownStatement, unsafeBreak, unsafeCall, unsafeNop, vec2, vec2T, vec2Zero, vec3, vec3T, vec3Zero, vec4, vec4T, vec4Zero, vec4_1_3, vec4_3_1, voidT, zero)
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
    , u_completelyReal : Expression1 Float
    }


constantToGlsl : GlslConstant -> ( FunDecl, Expression2 )
constantToGlsl c =
    case c of
        I ->
            fun0 vec2T "i" <|
                return (vec2 zero one)

        Pi ->
            fun0 vec2T "pi" <|
                return (vec2 constants.pi zero)

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


atanPlusTuple : ( FunDecl, ExpressionX xa Float -> ExpressionX xb Float -> Expression1 Float )
atanPlusTuple =
    fun2 floatT "atanPlus" (floatT "y") (floatT "x") <|
        \x y ->
            return <| mod (add constants.twopi (dotted1 <| unsafeCall "atan" [ y.base, x.base ])) constants.twopi


atanPlusDecl : FunDecl
atanPlusDecl =
    Tuple.first atanPlusTuple


atanPlus : ExpressionX xa Float -> ExpressionX xb Float -> Expression1 Float
atanPlus =
    Tuple.second atanPlusTuple


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


iltCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
iltCouple =
    fun2 vec2T "ilt" (vec2T "l") (vec2T "r") <| \l r -> return <| vec2 (subtract r.x l.y) (subtract r.y l.x)


iltDecl : FunDecl
iltDecl =
    Tuple.first iltCouple


ilt : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
ilt =
    Tuple.second iltCouple


ileqCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
ileqCouple =
    fun2 vec2T "ileq" (vec2T "l") (vec2T "r") <| \l r -> return <| vec2 (subtract r.x l.y) (subtract r.y l.x)


ileqDecl : FunDecl
ileqDecl =
    Tuple.first ileqCouple


ileq : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
ileq =
    Tuple.second ileqCouple


ineqCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
ineqCouple =
    fun2 vec2T "ineq" (vec2T "l") (vec2T "r") <| \l r -> return <| vec2 (subtract r.x l.y) (subtract r.y l.x)


ineqDecl : FunDecl
ineqDecl =
    Tuple.first ineqCouple


ineq : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
ineq =
    Tuple.second ineqCouple


ieqCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
ieqCouple =
    fun2 vec2T "ieq" (vec2T "l") (vec2T "r") <| \l r -> return <| vec2 (subtract l.x r.y) (subtract l.y r.x)


ieqDecl : FunDecl
ieqDecl =
    Tuple.first ieqCouple


ieq : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
ieq =
    Tuple.second ieqCouple


igeqCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
igeqCouple =
    fun2 vec2T "igeq" (vec2T "l") (vec2T "r") <| \l r -> return <| vec2 (subtract l.x r.y) (subtract l.y r.x)


igeqDecl : FunDecl
igeqDecl =
    Tuple.first igeqCouple


igeq : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
igeq =
    Tuple.second igeqCouple


igtCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
igtCouple =
    fun2 vec2T "igt" (vec2T "l") (vec2T "r") <| \l r -> return <| vec2 (subtract l.x r.y) (subtract l.y r.x)


igtDecl : FunDecl
igtDecl =
    Tuple.first igtCouple


igt : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
igt =
    Tuple.second igtCouple


iabsCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
iabsCouple =
    fun1 vec2T "iabs" (vec2T "z") <|
        \z ->
            return <|
                ternary
                    (ands [ leq z.x zero, geq z.y zero ])
                    (vec2 zero <| max_ z.y <| abs_ z.x)
                    (ternary
                        (leq z.x zero)
                        (vec2 (negate_ z.y) (negate_ z.x))
                        z
                    )


iabsDecl : FunDecl
iabsDecl =
    Tuple.first iabsCouple


iabs : ExpressionX xa Vec2 -> Expression2
iabs =
    Tuple.second iabsCouple


gabsCouple : ( FunDecl, ExpressionX xa Vec4 -> Expression4 )
gabsCouple =
    fun1 vec4T "gabs" (vec4T "v") <| \v -> return <| vec4_1_3 (abs_ v.x) (byF (sign v.x) v.yzw)


gabsDecl : FunDecl
gabsDecl =
    Tuple.first gabsCouple


gabs : ExpressionX xa Vec4 -> Expression4
gabs =
    Tuple.second gabsCouple


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


ctanCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
ctanCouple =
    fun1 vec2T "ctan" (vec2T "z") <|
        \z ->
            return <| ternary (eq z.y zero) (vec2 (tan_ z.x) zero) (cdiv (csin z) (ccos z))


ctanDecl : FunDecl
ctanDecl =
    Tuple.first ctanCouple


ctan : ExpressionX xa Vec2 -> Expression2
ctan =
    Tuple.second ctanCouple


casinTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
casinTuple =
    fun1 vec2T "casin" (vec2T "z") <|
        \z ->
            def vec2T "s" (csqrt <| subtract (vec2 one zero) (cby z z)) <|
                \s ->
                    def vec2T "arg" (subtract s <| cby (vec2 zero one) z) <|
                        \arg_ ->
                            return <| cby (vec2 zero one) (cln arg_)


casinDecl : FunDecl
casinDecl =
    Tuple.first casinTuple


casin : ExpressionX xa Vec2 -> Expression2
casin =
    Tuple.second casinTuple


cacosTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
cacosTuple =
    fun1 vec2T "cacos" (vec2T "z") <| \z -> return <| subtract (vec2 constants.pihalf zero) (casin z)


cacosDecl : FunDecl
cacosDecl =
    Tuple.first cacosTuple


cacos : ExpressionX xa Vec2 -> Expression2
cacos =
    Tuple.second cacosTuple


catanTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
catanTuple =
    fun1 vec2T "catan" (vec2T "z") <|
        \z ->
            if_ (eq z.y zero)
                (return <| vec2 (atan_ z.x) zero)
                (def2
                    ( vec2T "o", vec2 one zero )
                    ( vec2T "iz", cby (vec2 zero one) z )
                 <|
                    \o iz ->
                        def vec2T "l" (cdiv (add o iz) (subtract o iz)) <|
                            \l ->
                                return <| byF (float -0.5) <| cby (vec2 zero one) (cln l)
                )


catanDecl : FunDecl
catanDecl =
    Tuple.first catanTuple


catan : ExpressionX xa Vec2 -> Expression2
catan =
    Tuple.second catanTuple


catan2Tuple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
catan2Tuple =
    fun2 vec2T "catan2" (vec2T "y") (vec2T "x") <|
        \y x ->
            def vec2T "z" (vec2 (subtract x.x y.y) (add x.y y.x)) <|
                \z ->
                    return <| vec2 (atan2_ z.y z.x) zero


catan2Decl : FunDecl
catan2Decl =
    Tuple.first catan2Tuple


catan2 : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
catan2 =
    Tuple.second catan2Tuple


csinhTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
csinhTuple =
    fun1 vec2T "csinh" (vec2T "z") <|
        \z ->
            return <| byF (float 0.5) (subtract (cexp z) (cexp <| negate_ z))


csinhDecl : FunDecl
csinhDecl =
    Tuple.first csinhTuple


csinh : ExpressionX xa Vec2 -> Expression2
csinh =
    Tuple.second csinhTuple


ccoshTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
ccoshTuple =
    fun1 vec2T "ccosh" (vec2T "z") <|
        \z ->
            return <| byF (float 0.5) (add (cexp z) (cexp <| negate_ z))


ccoshDecl : FunDecl
ccoshDecl =
    Tuple.first ccoshTuple


ccosh : ExpressionX xa Vec2 -> Expression2
ccosh =
    Tuple.second ccoshTuple


ctanhCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
ctanhCouple =
    fun1 vec2T "ctanh" (vec2T "z") <|
        \z ->
            def2
                ( vec2T "p", cexp z )
                ( vec2T "m", cexp (negate_ z) )
            <|
                \p m ->
                    return <| cdiv (subtract p m) (add p m)


ctanhDecl : FunDecl
ctanhDecl =
    Tuple.first ctanhCouple


ctanh : ExpressionX xa Vec2 -> Expression2
ctanh =
    Tuple.second ctanhCouple


cabsCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
cabsCouple =
    fun1 vec2T "cabs" (vec2T "z") <| \z -> return <| vec2 (length z) zero


cabsDecl : FunDecl
cabsDecl =
    Tuple.first cabsCouple


cabs : ExpressionX xa Vec2 -> Expression2
cabs =
    Tuple.second cabsCouple


csignCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
csignCouple =
    fun1 vec2T "csign" (vec2T "z") <|
        \z ->
            return <| vec2 (sign z.x) (sign z.y)


csignDecl : FunDecl
csignDecl =
    Tuple.first csignCouple


csign : ExpressionX xa Vec2 -> Expression2
csign =
    Tuple.second csignCouple


intervalOperationToGlsl : GlslOperation -> List FunDecl
intervalOperationToGlsl op =
    case op of
        GlslAddition ->
            []

        GlslNegation ->
            [ inegDecl, gnegDecl ]

        GlslMultiplication ->
            [ ibyDecl, gbyDecl ]

        GlslDivision ->
            [ iinverseDecl, idivDecl, gdivDecl ]

        GlslPower ->
            [ ipowFIDecl, ipowIDecl, ipowDecl, gpowIDecl, gpowDecl ]

        GlslRelations ->
            [ iltDecl, ileqDecl, ineqDecl, ieqDecl, igeqDecl, igtDecl ]


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
            [ ctanDecl ]

        Asin22 ->
            [ casinDecl ]

        Acos22 ->
            [ cacosDecl ]

        Atan22 ->
            [ catanDecl ]

        Atan222 ->
            [ catan2Decl ]

        Sinh22 ->
            [ csinhDecl ]

        Cosh22 ->
            [ ccoshDecl ]

        Tanh22 ->
            [ ctanhDecl ]

        Abs22 ->
            [ cabsDecl ]

        Sign22 ->
            [ csignDecl ]

        Sqrt22 ->
            [ csqrtDecl ]

        Cbrt22 ->
            [ ccbrtDecl ]

        Square22 ->
            [ csquareDecl ]

        Ln22 ->
            [ clnDecl ]

        Log1022 ->
            [ clog10Decl ]

        Exp22 ->
            [ cexpDecl ]

        Re22 ->
            [ creDecl ]

        Im22 ->
            [ cimDecl ]

        Arg22 ->
            [ argDecl, cargDecl ]

        Pw22 ->
            [ cpwDecl ]

        Ceiling22 ->
            [ cceilingDecl ]

        Floor22 ->
            [ cfloorDecl ]

        Round22 ->
            [ croundDecl ]

        Min222 ->
            [ cminDecl ]

        Max222 ->
            [ cmaxDecl ]

        Mod22 ->
            [ cmodDecl ]

        Mbrot22 ->
            [ cmbrotDecl ]


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


creTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
creTuple =
    fun1 vec2T "cre" (vec2T "z") <| \z -> return <| vec2 z.x zero


creDecl : FunDecl
creDecl =
    Tuple.first creTuple


cre : ExpressionX xa Vec2 -> Expression2
cre =
    Tuple.second creTuple


cimCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
cimCouple =
    fun1 vec2T "cim" (vec2T "z") <| \z -> return <| vec2 z.y zero


cimDecl : FunDecl
cimDecl =
    Tuple.first cimCouple


cim : ExpressionX xa Vec2 -> Expression2
cim =
    Tuple.second cimCouple


argTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression1 Float )
argTuple =
    fun1 floatT "arg" (vec2T "v") <| \v -> return <| atan2_ v.y v.x


argDecl : FunDecl
argDecl =
    Tuple.first argTuple


arg : ExpressionX xa Vec2 -> Expression1 Float
arg =
    Tuple.second argTuple


cargTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
cargTuple =
    fun1 vec2T "carg" (vec2T "v") <| \v -> return <| vec2 (atan2_ v.y v.x) zero


cargDecl : FunDecl
cargDecl =
    Tuple.first cargTuple


carg : ExpressionX xa Vec2 -> Expression2
carg =
    Tuple.second cargTuple


cpwCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> ExpressionX xc Vec2 -> Expression2 )
cpwCouple =
    fun3 vec2T "cpw" (vec2T "c") (vec2T "t") (vec2T "f") <|
        \c t f -> return <| ternary (gt c.x zero) t f


cpwDecl : FunDecl
cpwDecl =
    Tuple.first cpwCouple


cpw : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> ExpressionX xc Vec2 -> Expression2
cpw =
    Tuple.second cpwCouple


cceilingCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
cceilingCouple =
    fun1 vec2T "cceiling" (vec2T "z") <| \z -> return <| ceil_ z


cceilingDecl : FunDecl
cceilingDecl =
    Tuple.first cceilingCouple


cceiling : ExpressionX xa Vec2 -> Expression2
cceiling =
    Tuple.second cceilingCouple


cfloorCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
cfloorCouple =
    fun1 vec2T "cfloor" (vec2T "z") <| \z -> return <| floor_ z


cfloorDecl : FunDecl
cfloorDecl =
    Tuple.first cfloorCouple


cfloor : ExpressionX xa Vec2 -> Expression2
cfloor =
    Tuple.second cfloorCouple


croundCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
croundCouple =
    fun1 vec2T "cround" (vec2T "z") <|
        \z ->
            return <| floor_ (add z (vec2 (float 0.5) (float 0.5)))


croundDecl : FunDecl
croundDecl =
    Tuple.first croundCouple


cround : ExpressionX xa Vec2 -> Expression2
cround =
    Tuple.second croundCouple


cminTuple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
cminTuple =
    fun2 vec2T "cmin" (vec2T "l") (vec2T "r") <| \l r -> return <| ternary (lt l.x r.x) l r


cminDecl : FunDecl
cminDecl =
    Tuple.first cminTuple


cmin : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
cmin =
    Tuple.second cminTuple


cmaxTuple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
cmaxTuple =
    fun2 vec2T "cmax" (vec2T "l") (vec2T "r") <| \l r -> return <| ternary (gt l.x r.x) l r


cmaxDecl : FunDecl
cmaxDecl =
    Tuple.first cmaxTuple


cmax : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
cmax =
    Tuple.second cmaxTuple


cmodCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
cmodCouple =
    fun2 vec2T "cmod" (vec2T "l") (vec2T "r") <|
        \l r ->
            return <| vec2 (mod l.x r.x) zero


cmodDecl : FunDecl
cmodDecl =
    Tuple.first cmodCouple


cmod : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
cmod =
    Tuple.second cmodCouple


cmbrotCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
cmbrotCouple =
    fun2 vec2T "cmbrot" (vec2T "x") (vec2T "y") <|
        \x y ->
            def vec2T "c" (add x (vec2 (negate_ y.y) y.x)) <|
                \c ->
                    def floatT "p" (length (subtract c (vec2 (float 0.25) zero))) <|
                        \p ->
                            if_ (or (leq c.x (add (subtract p (by (by (float 2) p) p)) (float 0.25))) (leq (length (add c (vec2 one zero))) (float 0.25)))
                                (return vec2Zero)
                                (def vec2T "z" c <|
                                    \z ->
                                        for ( "i", int 0, int 4000 )
                                            (\i ->
                                                expr (assign z (add (vec2 (subtract (by z.x z.x) (by z.y z.y)) (by (by (float 2) z.x) z.y)) c))
                                                    (if_ (gt (length z) (float 1000000))
                                                        (def floatT "logLength" (log (length z)) <|
                                                            \logLength ->
                                                                def floatT "nu" (div (log (div logLength (log (float 2)))) (log (float 2))) <|
                                                                    \nu ->
                                                                        def floatT "fi" (subtract (floatCast i) nu) <|
                                                                            \fi ->
                                                                                return <| vec2 (sin_ fi) (cos_ fi)
                                                        )
                                                        unsafeNop
                                                    )
                                            )
                                            (return vec2Zero)
                                )


cmbrotDecl : FunDecl
cmbrotDecl =
    Tuple.first cmbrotCouple


cmbrot : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
cmbrot =
    Tuple.second cmbrotCouple


csqrtTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
csqrtTuple =
    fun1 vec2T "csqrt" (vec2T "z") <|
        \z ->
            if_ (ands [ eq z.y zero, geq z.x zero ])
                (return <| vec2 (sqrt_ z.x) zero)
                (def2
                    ( floatT "r", pow (dot z z) (float 0.25) )
                    ( floatT "t", byF (float 0.5) (atan2_ z.y z.x) )
                 <|
                    \r t ->
                        return <| byF r <| vec2 (cos_ t) (sin_ t)
                )


csqrtDecl : FunDecl
csqrtDecl =
    Tuple.first csqrtTuple


csqrt : ExpressionX xa Vec2 -> Expression2
csqrt =
    Tuple.second csqrtTuple


ccbrtCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
ccbrtCouple =
    fun1 vec2T "ccbrt" (vec2T "z") <|
        \z ->
            if_ (eq z.y zero)
                (return <| vec2 (by (sign z.x) (pow z.x (div one (float 3)))) zero)
                (def floatT "r" (pow (dot z z) (div one (float 6))) <|
                    \r ->
                        def floatT
                            "t"
                            (add (div (atan2_ z.y z.x) (float 3))
                                (ternary
                                    (gt z.x zero)
                                    zero
                                    (radians_ (float 120))
                                )
                            )
                        <|
                            \t ->
                                return <| byF r (vec2 (cos_ t) (sin_ t))
                )


ccbrtDecl : FunDecl
ccbrtDecl =
    Tuple.first ccbrtCouple


ccbrt : ExpressionX xa Vec2 -> Expression2
ccbrt =
    Tuple.second ccbrtCouple


csquareTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
csquareTuple =
    fun1 vec2T "csquare" (vec2T "z") <| \z -> return <| cby z z


csquareDecl : FunDecl
csquareDecl =
    Tuple.first csquareTuple


csquare : ExpressionX xa Vec2 -> Expression2
csquare =
    Tuple.second csquareTuple


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


clnDecl : FunDecl
clnDecl =
    Tuple.first clnTuple


cln : ExpressionX xa Vec2 -> Expression2
cln =
    Tuple.second clnTuple


clog10Couple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
clog10Couple =
    fun1 vec2T "clog10" (vec2T "z") <| \z -> return <| cdiv (cln z) (vec2 (log (float 10)) zero)


clog10Decl : FunDecl
clog10Decl =
    Tuple.first clog10Couple


clog10 : ExpressionX xa Vec2 -> Expression2
clog10 =
    Tuple.second clog10Couple


intervalFunctionToGlsl : GlslFunction -> String
intervalFunctionToGlsl name =
    case name of
        Abs22 ->
            fileToGlsl [ iabsDecl, gabsDecl ]

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
                    return dup(PI);
                if(z.x >= 0.0)
                    return vec2(0);
                return vec2(0.0, PI);
            }

            vec4 garg(vec4 z) {
                return gnum(z.x >= 0.0 ? 0.0 : PI);
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
                if(v.y - v.x > TWOPI) {
                    return vec2(-1.0, 1.0);
                }
                float from = mod(v.x, TWOPI); // [0, 360°]
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


toSrcImplicit :
    String
    -> Expression.Expression
    -> ( List FunDecl, ExpressionX xa Float -> ExpressionX xb Float -> ExpressionX xc Float -> ExpressionX xd Float -> Expression3 )
toSrcImplicit suffix e =
    let
        antialiasingSamples =
            7

        ( fDecl, f ) =
            fun2 floatT ("f" ++ suffix) (floatT "x") (floatT "y") <|
                \x y ->
                    def vec2T "complex" (expressionToGlsl [ ( "x", x ), ( "y", y ) ] e) <|
                        \complex ->
                            return <|
                                ternary (gt (abs_ complex.y) (float epsilon))
                                    zero
                                    (ternary (gt complex.x zero) one minusOne)

        ( pixelDecl, pixel ) =
            fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <|
                \deltaX _ x y ->
                    def2
                        ( floatT "sum", zero )
                        ( floatT "samples", float <| antialiasingSamples * 2 + 1 )
                    <|
                        \sum samples ->
                            assignBy samples samples <|
                                def floatT
                                    "coeff"
                                    (float 0.0875)
                                    (\coeff ->
                                        forLeq ( "w", int <| -antialiasingSamples, int antialiasingSamples )
                                            (\w ->
                                                forLeq ( "h", int <| -antialiasingSamples, int antialiasingSamples )
                                                    (\h ->
                                                        def floatT
                                                            "piece"
                                                            (f
                                                                (add x <| by (by deltaX coeff) <| floatCast w)
                                                                (add y <| by (by deltaX coeff) <| floatCast h)
                                                            )
                                                        <|
                                                            \piece ->
                                                                if_
                                                                    (eq piece zero)
                                                                    (return vec3Zero)
                                                                    (assignAdd sum piece unsafeNop)
                                                    )
                                                    unsafeNop
                                            )
                                            (def floatT "perc" (div (subtract samples <| abs_ sum) samples) <|
                                                \perc ->
                                                    expr
                                                        (assign perc (pow perc <| float 0.2))
                                                        (return <| byF perc (vec3 one one one))
                                            )
                                    )
    in
    ( [ fDecl, pixelDecl ], pixel )


toSrcPolar :
    String
    -> Expression.Expression
    -> ( List FunDecl, ExpressionX xa Float -> ExpressionX xb Float -> ExpressionX xc Float -> ExpressionX xd Float -> Expression3 )
toSrcPolar suffix e =
    let
        ( fDecl, f ) =
            fun4 floatT ("f" ++ suffix) (floatT "x") (floatT "y") (floatT "deltaT") (floatT "ot") <|
                \x y deltaT ot ->
                    def floatT "r" (length <| vec2 x y) <|
                        \r ->
                            def floatT "t" (add (atanPlus y x) deltaT) <|
                                \t ->
                                    -- Avoid the branch cut at {x > 0, y = 0}
                                    if_ (gt (abs_ <| subtract t ot) constants.pi)
                                        (assignAdd t (ternary (lt t ot) constants.twopi (negate_ constants.twopi)) unsafeNop)
                                        (def vec2T "complex" (expressionToGlsl [ ( "x", x ), ( "y", y ), ( "r", r ), ( "t", t ) ] e) <|
                                            \complex ->
                                                return <|
                                                    ternary (gt (abs_ complex.y) (float epsilon))
                                                        minusOne
                                                        (ternary (gt complex.x zero)
                                                            one
                                                            zero
                                                        )
                                        )

        ( pixelDecl, pixel ) =
            fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <|
                \deltaX deltaY x y ->
                    assignAdd x (div deltaX <| float 2) <|
                        assignAdd y (div deltaY <| float 2) <|
                            def2
                                ( floatT "t", zero )
                                ( floatT "ot", atanPlus y x )
                            <|
                                \t ot ->
                                    for ( "i", int 0, divConst constants.maxIterations (int 10) )
                                        (\_ ->
                                            def4
                                                ( floatT "h", f x y t ot )
                                                ( floatT "l", f (subtract x deltaX) y t ot )
                                                ( floatT "u", f x (subtract y deltaY) t ot )
                                                ( floatT "ul", f (subtract x deltaX) (subtract y deltaY) t ot )
                                            <|
                                                \h l u ul ->
                                                    if_ (ors [ lt h zero, lt l zero, lt u zero, lt ul zero ])
                                                        unsafeBreak
                                                        (if_ (ors [ neq h l, neq h u, neq h ul ])
                                                            (return <| vec3 one one one)
                                                            (assignAdd t constants.twopi <|
                                                                assignAdd ot constants.twopi <|
                                                                    unsafeNop
                                                            )
                                                        )
                                        )
                                        (return vec3Zero)
    in
    ( [ fDecl, pixelDecl ], pixel )


constants :
    { maxIterations : ExpressionX { isConstant : Constant } Int
    , maxDepth : ExpressionX { isConstant : Constant } Int
    , pihalf : ExpressionX { isConstant : Constant } Float
    , pi : ExpressionX { isConstant : Constant } Float
    , twopi : ExpressionX { isConstant : Constant } Float
    , vectorSpacing : ExpressionX { isConstant : Constant } Float
    }
constants =
    { maxIterations = constant intT "MAX_ITERATIONS"
    , maxDepth = constant intT "MAX_DEPTH"
    , pihalf = constant floatT "PIHALF"
    , pi = constant floatT "PI"
    , twopi = constant floatT "TWOPI"
    , vectorSpacing = constant floatT "VECTOR_SPACING"
    }


toSrcParametric : String -> Expression.Expression -> ( List FunDecl, ExpressionX xa Float -> ExpressionX xb Float -> ExpressionX xc Float -> ExpressionX xd Float -> Expression3 )
toSrcParametric suffix e =
    let
        ( intervalDecl, interval ) =
            fun3 vec2T ("interval" ++ suffix) (vec2T "p") (floatT "from") (floatT "to") <|
                \p from to ->
                    def3
                        ( vec2T "x", vec2 p.x p.x )
                        ( vec2T "y", vec2 p.y p.y )
                        ( vec2T "t", ternary (lt from to) (vec2 from to) (vec2 to from) )
                    <|
                        \x y t ->
                            return <| expressionToIntervalGlsl (toPrintExpression e)

        ( pixelDecl, pixel ) =
            fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <|
                \deltaX deltaY x y ->
                    def floatT "max_distance" (float <| 2 ^ 10) <|
                        \max_distance ->
                            def6
                                ( floatT "from", div (negate_ max_distance) (float 2) )
                                ( floatT "to", div max_distance (float 2) )
                                ( vec2T "p", vec2 x y )
                                ( intT "depth", int 0 )
                                ( intT "choices", int 0 )
                                ( floatT "ithreshold", by (by (float 10) deltaX) deltaX )
                            <|
                                \from to p depth choices ithreshold ->
                                    for ( "it", int 0, constants.maxIterations )
                                        (\_ -> innerLoop from to p depth choices ithreshold)
                                        (return vec3Zero)

        innerLoop from to p depth choices ithreshold =
            def floatT "midpoint" (mix from to <| float 0.5) <|
                \midpoint ->
                    def2
                        ( vec2T "front", interval p from midpoint )
                        ( vec2T "back", interval p midpoint to )
                    <|
                        \front back ->
                            if_
                                (ors
                                    [ geq depth constants.maxDepth
                                    , ands
                                        [ lt (subtract front.y front.x) ithreshold
                                        , leq front.x zero
                                        , geq front.y zero
                                        ]
                                    , ands
                                        [ lt (subtract back.y back.x) ithreshold
                                        , leq back.x zero
                                        , geq back.y zero
                                        ]
                                    ]
                                )
                                (return <| vec3 one one one)
                                (ifElse
                                    (ands [ leq front.x zero, geq front.y zero ])
                                    (expr (assign to midpoint) <|
                                        expr (postfixPlus depth) <|
                                            assignBy choices (int 2) <|
                                                unsafeNop
                                    )
                                    (ifElse
                                        (ands [ leq back.x zero, geq back.y zero ])
                                        (expr (assign from midpoint) <|
                                            expr (postfixPlus depth) <|
                                                expr (assign choices (add (by choices (int 2)) (int 1))) <|
                                                    unsafeNop
                                        )
                                        (unknownStatement
                                            ("""
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
                                            """)
                                        )
                                        unsafeNop
                                    )
                                    unsafeNop
                                )
    in
    ( [ intervalDecl, pixelDecl ], pixel )


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
    , u_completelyReal = uniform floatT "u_completelyReal"
    }


thetaDeltaCouple : ( FunDecl, ExpressionX xa Float -> Expression1 Float )
thetaDeltaCouple =
    fun1 floatT "thetaDelta" (floatT "theta") <|
        \theta ->
            if_ (lt uniforms.u_whiteLines one)
                (return <| float 100)
                (def2
                    ( floatT "thetaSix", add (by theta uniforms.u_whiteLines) (float 0.5) )
                    ( floatT "thetaNeigh", float 0.05 )
                 <|
                    \thetaSix thetaNeigh ->
                        return <| divF (abs_ (subtract (fract thetaSix) (float 0.5))) thetaNeigh
                )


thetaDeltaDecl : FunDecl
thetaDeltaDecl =
    Tuple.first thetaDeltaCouple


thetaDelta : ExpressionX xa Float -> Expression1 Float
thetaDelta =
    Tuple.second thetaDeltaCouple


toSrcVectorField2D :
    String
    -> Expression.Expression
    -> Expression.Expression
    -> ( List FunDecl, ExpressionX xa Float -> ExpressionX xb Float -> ExpressionX xc Float -> ExpressionX xd Float -> Expression3 )
toSrcVectorField2D suffix xexpr yexpr =
    let
        ( vectorDecl, vector ) =
            fun2 vec2T ("vector" ++ suffix) (floatT "x") (floatT "y") <|
                \x y ->
                    def2
                        ( vec2T "xv", expressionToGlsl [ ( "x", x ), ( "y", y ) ] xexpr )
                        ( vec2T "yv", expressionToGlsl [ ( "x", x ), ( "y", y ) ] yexpr )
                    <|
                        \xv yv -> return <| ternary (lt (add (abs_ xv.y) (abs_ yv.y)) (float epsilon)) (vec2 xv.x yv.x) vec2Zero

        ( nearDecl, near ) =
            fun5 boolT "near" (vec2T "o") (vec2T "corner") (vec2T "vect") (floatT "deltaX") (floatT "mx") <|
                \o corner vect deltaX mx ->
                    def2
                        ( floatT "angleCorner", arg <| subtract o corner )
                        ( floatT "angleVector", arg vect )
                    <|
                        \angleCorner angleVector ->
                            def floatT "delta" (mod (subtract angleCorner angleVector) constants.twopi) <|
                                \delta ->
                                    def floatT "l" (div (length vect) mx) <|
                                        \l ->
                                            def floatT
                                                "maxLength"
                                                (by (by deltaX constants.vectorSpacing)
                                                    (ternary
                                                        (lt l (float epsilon))
                                                        zero
                                                        (add (div l (float 2)) (float 0.5))
                                                    )
                                                )
                                            <|
                                                \maxLength ->
                                                    def floatT "wantedLength" (length (subtract o corner)) <|
                                                        \wantedLength ->
                                                            def floatT "angularDistance" (mix (float 180) zero (pow (div wantedLength maxLength) (float 0.3))) <|
                                                                \angularDistance ->
                                                                    return <| and (or (lt delta (radians_ angularDistance)) (gt delta (radians_ (subtract (float 360) angularDistance)))) (lt wantedLength maxLength)

        ( pixelDecl, pixel ) =
            fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <|
                \deltaX deltaY x y ->
                    unknownStatement ("""
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
                
                """)
    in
    ( [ vectorDecl, nearDecl, pixelDecl ], pixel )


toSrcContour : String -> Expression.Expression -> ( List FunDecl, ExpressionX xa Float -> ExpressionX xb Float -> ExpressionX xc Float -> ExpressionX xd Float -> Expression3 )
toSrcContour suffix expr =
    let
        ( pixelODecl, pixelO ) =
            fun4 vec3T ("pixel" ++ suffix ++ "_o") (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <|
                \_ _ x y ->
                    def vec2T "z" (expressionToGlsl [ ( "x", x ), ( "y", y ) ] expr) <|
                        \z ->
                            def2
                                ( floatT "theta", div (atan2_ z.y z.x) constants.twopi )
                                ( floatT "logRadius", log2 <| length z )
                            <|
                                \theta logRadius ->
                                    def floatT "powerRemainder" (fract logRadius) <|
                                        \powerRemainder ->
                                            def floatT "squished" (subtract (float 0.7) (by powerRemainder <| float 0.4)) <|
                                                \squished ->
                                                    if_ (gt uniforms.u_completelyReal zero)
                                                        (def floatT "haf" (div powerRemainder <| float 6.0) <|
                                                            \haf ->
                                                                return <|
                                                                    ternary (gt z.x zero)
                                                                        (hl2rgb (add haf <| float 0.3) squished)
                                                                        (hl2rgb (subtract haf <| float 0.2) (subtract one squished))
                                                        )
                                                        (def floatT "td" (thetaDelta theta) <|
                                                            \td ->
                                                                def floatT "l" (mix one squished <| smoothstep zero one td) <|
                                                                    \l ->
                                                                        return <| hl2rgb theta l
                                                        )

        ( pixelDecl, pixel ) =
            fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <|
                \deltaX deltaY x y ->
                    -- Antialiasing
                    def floatT "dist" (float <| 1 / 3) <|
                        \dist ->
                            def4
                                ( vec3T "a", pixelO deltaX deltaY (add x <| by dist deltaX) (add y <| by dist deltaY) )
                                ( vec3T "b", pixelO deltaX deltaY (subtract x <| by dist deltaX) (subtract y <| by dist deltaY) )
                                ( vec3T "c", pixelO deltaX deltaY (add x <| by dist deltaX) (subtract y <| by dist deltaY) )
                                ( vec3T "d", pixelO deltaX deltaY (subtract x <| by dist deltaX) (add y <| by dist deltaY) )
                            <|
                                \a b c d ->
                                    def2
                                        ( vec3T "mn", min_ a <| min_ b <| min_ c d )
                                        ( vec3T "mx", max_ a <| max_ b <| max_ c d )
                                    <|
                                        \mn mx ->
                                            def vec3T "diff" (abs_ (subtract mx mn)) <|
                                                \diff ->
                                                    if_ (ands [ lt diff.x dist, lt diff.y dist, lt diff.z dist ])
                                                        (return <| divF (adds3 [ a, b, c, d ]) (float 4))
                                                        (def floatT "dist2" (by (float 2) dist) <|
                                                            \dist2 ->
                                                                def4
                                                                    ( vec3T "e", pixelO deltaX deltaY (add x <| byF dist2 deltaX) y )
                                                                    ( vec3T "f", pixelO deltaX deltaY (subtract x <| byF dist2 deltaX) y )
                                                                    ( vec3T "g", pixelO deltaX deltaY x (subtract y <| byF dist2 deltaY) )
                                                                    ( vec3T "h", pixelO deltaX deltaY x (add y <| byF dist2 deltaY) )
                                                                <|
                                                                    \e f g h ->
                                                                        return <| divF (adds3 [ a, b, c, d, e, f, g, h ]) (float 8)
                                                        )
    in
    ( [ pixelODecl, pixelDecl ], pixel )


toSrcRelation : String -> Expression.Expression -> ( List FunDecl, ExpressionX xa Float -> ExpressionX xb Float -> ExpressionX xc Float -> ExpressionX xd Float -> Expression3 )
toSrcRelation suffix e =
    Tuple.mapFirst (\d -> [ d ]) <|
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


epsilon : Float
epsilon =
    0.00001


expressionToGlsl : List ( String, Expression1 Float ) -> Expression.Expression -> Expression2
expressionToGlsl context =
    let
        ctx =
            Dict.fromList context

        variadic f es =
            case List.map go es of
                [] ->
                    vec2Zero

                head :: tail ->
                    List.foldl (\e a -> f a e) head tail

        go expr =
            case expr of
                PVariable "i" ->
                    vec2 zero one

                PVariable "pi" ->
                    vec2 constants.pi zero

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
                    case op of
                        LessThan ->
                            subtract2 (go r) (go l)

                        LessThanOrEquals ->
                            subtract2 (go r) (go l)

                        NotEquals ->
                            abs2 <| subtract (go r) (go l)

                        Equals ->
                            abs2 <| subtract (go r) (go l)

                        GreaterThanOrEquals ->
                            subtract2 (go l) (go r)

                        GreaterThan ->
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

                PPower l (PInteger 2) ->
                    csquare (go l)

                PPower l r ->
                    cpow (go l) (go r)

                PApply (KnownFunction Simplify) [ e ] ->
                    go e

                PApply (KnownFunction Sin) [ e ] ->
                    csin (go e)

                PApply (KnownFunction Cos) [ e ] ->
                    ccos (go e)

                PApply (KnownFunction Tan) [ e ] ->
                    ctan (go e)

                PApply (KnownFunction Asin) [ e ] ->
                    casin (go e)

                PApply (KnownFunction Acos) [ e ] ->
                    cacos (go e)

                PApply (KnownFunction Atan) [ e ] ->
                    catan (go e)

                PApply (KnownFunction Atan2) [ l, r ] ->
                    catan2 (go l) (go r)

                PApply (KnownFunction Sinh) [ e ] ->
                    csinh (go e)

                PApply (KnownFunction Cosh) [ e ] ->
                    ccosh (go e)

                PApply (KnownFunction Tanh) [ e ] ->
                    ctanh (go e)

                PApply (KnownFunction Abs) [ e ] ->
                    cabs (go e)

                -- PApply (KnownFunction (Root r)) [ e ] ->
                --     croot r (go e)
                PApply (KnownFunction Ln) [ e ] ->
                    cln (go e)

                PApply (KnownFunction Log10) [ e ] ->
                    clog10 (go e)

                PApply (KnownFunction Exp) [ e ] ->
                    cexp (go e)

                PApply (KnownFunction Sign) [ e ] ->
                    csign (go e)

                PApply (KnownFunction Re) [ e ] ->
                    cre (go e)

                PApply (KnownFunction Im) [ e ] ->
                    cim (go e)

                PApply (KnownFunction Arg) [ e ] ->
                    carg (go e)

                -- PApply (KnownFunction Gra) [ e ] ->
                --     cgra (go e)
                -- PApply (KnownFunction Det) [ e ] ->
                --     cdet (go e)
                -- PApply (KnownFunction Dd) [ e ] ->
                --     cdd (go e)
                -- PApply (KnownFunction Ii) [ e ] ->
                --     cii (go e)
                PApply (KnownFunction Round) [ e ] ->
                    cround (go e)

                PApply (KnownFunction Floor) [ e ] ->
                    cfloor (go e)

                PApply (KnownFunction Ceiling) [ e ] ->
                    cceiling (go e)

                PApply (KnownFunction Pw) [ c, t, f ] ->
                    cpw (go c) (go t) (go f)

                -- PApply (KnownFunction Plot) [ e ] ->
                --     cplot (go e)
                -- PApply (KnownFunction APlot) [ e ] ->
                --     caPlot (go e)
                -- PApply (KnownFunction StepSimplify) [ e ] ->
                --     cstepSimplify (go e)
                -- PApply (KnownFunction Solve) [ e ] ->
                --     csolve (go e)
                PApply (KnownFunction Mod) [ l, r ] ->
                    cmod (go l) (go r)

                PApply (KnownFunction Mbrot) [ x, y ] ->
                    cmbrot (go x) (go y)

                -- PApply (KnownFunction For) [ e ] ->
                --     cfor (go e)
                PApply (KnownFunction Min) es ->
                    variadic cmin es

                PApply (KnownFunction Max) es ->
                    variadic cmax es

                PApply (UserFunction name) ex ->
                    dotted2 <| unsafeCall ("c" ++ name) (List.map (go >> .base) ex)

                PApply name ex ->
                    dotted2 <| unsafeCall ("c" ++ functionNameToString name) (List.map (go >> .base) ex)

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
            dup constants.pi

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
            case op of
                LessThan ->
                    ilt (go l) (go r)

                LessThanOrEquals ->
                    ileq (go l) (go r)

                Equals ->
                    ieq (go l) (go r)

                NotEquals ->
                    ineq (go l) (go r)

                GreaterThanOrEquals ->
                    igeq (go l) (go r)

                GreaterThan ->
                    igt (go l) (go r)

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
                    gnum constants.pi

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
                    case op of
                        LessThan ->
                            subtract4 (go r) (go l)

                        LessThanOrEquals ->
                            subtract4 (go r) (go l)

                        NotEquals ->
                            abs4 <| subtract (go r) (go l)

                        Equals ->
                            abs4 <| subtract (go r) (go l)

                        GreaterThanOrEquals ->
                            subtract4 (go l) (go r)

                        GreaterThan ->
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
                                expr (assign gl_FragColor pixel2)
                                    unsafeNop
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
                            expr (assign gl_FragColor pixel3)
                                unsafeNop
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
                            expr (assign gl_FragColor (max4 pixel2 pixel3))
                                unsafeNop
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
    expr (assign curr k) <|
        expr (assign px (ternary3 (eq curr vec3Zero) px curr))
            next


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
                                                                def vec3T "eye" (eyePosition eyeDist t p) <|
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
                                                                                                                def vec3T "canvas_point" (add (add canvasCenter <| byF uvNormalized.x across) (byF uvNormalized.y up)) <|
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

        eyePosition eyeDist t p =
            byF eyeDist <|
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
                if(found_index < 0)
                    return vec4(0,0,0,1);

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


suffixToBisect : String -> FunDecl
suffixToBisect suffix =
    unknownFunDecl
        { name = "bisect" ++ suffix
        , type_ = "TODO"
        , body =
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
        }


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
