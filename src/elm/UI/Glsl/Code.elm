module UI.Glsl.Code exposing (atanPlusDecl, cexpFunction, constantToGlsl, dupDecl, expressionToGlsl, gnumDecl, intervalFunctionToGlsl, intervalOperationToGlsl, mainGlsl, straightFunctionToGlsl, straightOperationToGlsl, suffixToBisect, thetaDeltaDecl, threshold, toSrc3D, toSrcContour, toSrcImplicit, toSrcParametric, toSrcPolar, toSrcRelation, toSrcVectorField2D)

import Dict exposing (Dict)
import Expression exposing (FunctionName(..), KnownFunction(..), PrintExpression(..), RelationOperation(..), toPrintExpression)
import UI.Glsl.Generator exposing (Constant, Continue, Expression1, Expression2, Expression3, Expression33, Expression4, ExpressionX, File, FunDecl, Mat3, Statement, Vec2, Vec3, Vec4, abs2, abs4, abs_, acos2, acos_, add, add2, add33, add4, adds3, ands, arr, assign, assignAdd, assignBy, atan2_, atan_, bool, boolT, break, by, by2, by3, byF, ceil_, constant, continue, cos_, cosh, cross, decl, def, div, div2, divConst, divF, dot, dotted1, dotted2, dotted4, eq, exp, expr, false, fileToGlsl, float, floatCast, floatT, floor_, for, forDown, forLeq, fract, fun0, fun1, fun2, fun3, fun4, fun5, fwidth, geq, gl_FragColor, gl_FragCoord, gt, hl2rgb, ifElse, if_, int, intCast, intT, length, leq, log, log2, lt, mat3T, mat3_3_3_3, max3, max4, max_, min_, minusOne, mix, mod, negate2, negateConst, negate_, neq, normalize, normalize3, one, or, ors, out, postfixDecrement, postfixIncrement, pow, radians_, return, round_, sign, sin_, sinh, smoothstep, sqrt_, subtract, subtract2, subtract3, subtract4, subtractConst, tan_, ternary, ternary3, uniform, unsafeCall, vec2, vec2T, vec2Zero, vec3, vec3T, vec3Zero, vec4, vec4T, vec4Zero, vec4_1_3, vec4_3_1, voidT, zero)
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
    fun2 vec2T "cby" (vec2T "a") (vec2T "b") <| \a b ->
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
    fun2 floatT "atanPlus" (floatT "y") (floatT "x") <| \y x ->
    return <| mod (add constants.twopi (atan2_ y x)) constants.twopi


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
    fun3 vec2T "cby" (vec2T "a") (vec2T "b") (vec2T "c") <| \a b c ->
    return <| cby (cby a b) c


cby3Decl : FunDecl
cby3Decl =
    Tuple.first cby3Tuple


cdivTuple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
cdivTuple =
    fun2 vec2T "cdiv" (vec2T "a") (vec2T "b") <| \a b nop ->
    def floatT "k" (div one <| dot b b) <| \k ->
    def floatT "r" (by k <| dot a b) <| \r ->
    def floatT "i" (by k (subtract (by a.y b.x) (by a.x b.y))) <| \i ->
    return (vec2 r i) nop


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
    fun2 vec2T "cpow" (vec2T "w") (vec2T "z") <| \w z ->
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
    fun1 vec2T "ineg" (vec2T "v") <| \v ->
    return <| negate_ v.yx


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
    fun1 vec2T "isquare" (vec2T "z") <| \z nop ->
    def vec2T "s" (by z z) <| \s ->
    def floatT "mx" (max_ s.x s.y) <| \mx ->
    if_ (ands [ leq z.x zero, geq z.y zero ])
        (return <| vec2 zero mx)
    <| \_ ->
    def floatT "mn" (min_ s.x s.y) <| \mn ->
    return (vec2 mn mx) nop


isquareDecl : FunDecl
isquareDecl =
    Tuple.first isquareCouple


isquare : ExpressionX xa Vec2 -> Expression2
isquare =
    Tuple.second isquareCouple


gsquareCouple : ( FunDecl, ExpressionX xa Vec4 -> Expression4 )
gsquareCouple =
    fun1 vec4T "gsquare" (vec4T "z") <| \z ->
    return <| vec4_1_3 (by z.x z.x) z.yzw


gsquareDecl : FunDecl
gsquareDecl =
    Tuple.first gsquareCouple


gsquare : ExpressionX xa Vec4 -> Expression4
gsquare =
    Tuple.second gsquareCouple


gbyCouple : ( FunDecl, ExpressionX xa Vec4 -> ExpressionX xb Vec4 -> Expression4 )
gbyCouple =
    fun2 vec4T "gby" (vec4T "l") (vec4T "r") <| \l r ->
    return <| vec4_1_3 (by l.x r.x) (add (byF l.x r.yzw) (byF r.x l.yzw))


gbyDecl : FunDecl
gbyDecl =
    Tuple.first gbyCouple


gby : ExpressionX xa Vec4 -> ExpressionX xb Vec4 -> Expression4
gby =
    Tuple.second gbyCouple


ibyCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
ibyCouple =
    fun2 vec2T "iby" (vec2T "l") (vec2T "r") <| \l r nop ->
    def floatT "a" (by l.x r.x) <| \a ->
    def floatT "b" (by l.x r.y) <| \b ->
    def floatT "c" (by l.y r.x) <| \c ->
    def floatT "d" (by l.y r.y) <| \d ->
    def floatT "mn" (min_ (min_ a b) (min_ c d)) <| \mn ->
    def floatT "mx" (max_ (max_ a b) (max_ c d)) <| \mx ->
    return (vec2 mn mx) nop


ibyDecl : FunDecl
ibyDecl =
    Tuple.first ibyCouple


iby : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
iby =
    Tuple.second ibyCouple


iinverseCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
iinverseCouple =
    fun1 vec2T "iinverse" (vec2T "y") <| \y ->
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
    fun2 vec2T "idiv" (vec2T "l") (vec2T "r") <| \l r ->
    return <| iby l (iinverse r)


idivDecl : FunDecl
idivDecl =
    Tuple.first idivCouple


idiv : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
idiv =
    Tuple.second idivCouple


gdivCouple : ( FunDecl, ExpressionX xa Vec4 -> ExpressionX xb Vec4 -> Expression4 )
gdivCouple =
    fun2 vec4T "gdiv" (vec4T "l") (vec4T "r") <| \l r ->
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
    fun2 floatT "ipow" (floatT "b") (intT "e") <| \b e nop ->
    def floatT "fe" (floatCast e) <| \fe ->
    return
        (ternary
            (eq (mod fe (float 2.0)) zero)
            (pow (abs_ b) fe)
            (by b <| pow (abs_ b) (subtract fe one))
        )
        nop


ipowFIDecl : FunDecl
ipowFIDecl =
    Tuple.first ipowFICouple


ipowFI : ExpressionX xa Float -> ExpressionX xb Int -> Expression1 Float
ipowFI =
    Tuple.second ipowFICouple


ipowICouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Int -> Expression2 )
ipowICouple =
    fun2 vec2T "ipow" (vec2T "b") (intT "e") <| \b e nop ->
    if_ (eq e (int 0))
        (return <| vec2 one one)
    <| \_ ->
    if_ (eq e (int 1))
        (return b)
    <| \_ ->
    def floatT "xe" (ipowFI b.x e) <| \xe ->
    def floatT "ye" (ipowFI b.y e) <| \ye ->
    def floatT "mn" (min_ xe ye) <| \mn ->
    def floatT "mx" (max_ xe ye) <| \mx ->
    return
        (ternary
            (ands
                [ eq (mod (floatCast e) (float 2)) zero
                , leq b.x zero
                , geq b.y zero
                ]
            )
            (vec2 (min_ zero mn) (max_ zero mx))
            (vec2 mn mx)
        )
        nop


ipowIDecl : FunDecl
ipowIDecl =
    Tuple.first ipowICouple


ipowI : ExpressionX xa Vec2 -> ExpressionX xb Int -> Expression2
ipowI =
    Tuple.second ipowICouple


ipowCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
ipowCouple =
    fun2 vec2T "ipow" (vec2T "b") (vec2T "e") <| \b e ->
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
    fun2 vec4T "gpow" (vec4T "b") (intT "e") <| \b e ->
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
    fun2 vec4T "gpow" (vec4T "b") (vec4T "e") <| \b e nop ->
    def intT "ie" (intCast e.x) <| \ie ->
    if_ (ands [ eq (floatCast ie) e.x, eq e.y zero, eq e.z zero, eq e.w zero ])
        (return <| gpowI b ie)
    <| \_ ->
    return (gexp <| gby (gln b) e) nop


gpowDecl : FunDecl
gpowDecl =
    Tuple.first gpowCouple


gpow : ExpressionX xa Vec4 -> ExpressionX xb Vec4 -> Expression4
gpow =
    Tuple.second gpowCouple


iltCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
iltCouple =
    fun2 vec2T "ilt" (vec2T "l") (vec2T "r") <| \l r -> return <| subtract r l.yx


iltDecl : FunDecl
iltDecl =
    Tuple.first iltCouple


ilt : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
ilt =
    Tuple.second iltCouple


ileqCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
ileqCouple =
    fun2 vec2T "ileq" (vec2T "l") (vec2T "r") <| \l r -> return <| subtract r l.yx


ileqDecl : FunDecl
ileqDecl =
    Tuple.first ileqCouple


ileq : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
ileq =
    Tuple.second ileqCouple


ineqCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
ineqCouple =
    fun2 vec2T "ineq" (vec2T "l") (vec2T "r") <| \l r -> return <| subtract r l.yx


ineqDecl : FunDecl
ineqDecl =
    Tuple.first ineqCouple


ineq : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
ineq =
    Tuple.second ineqCouple


ieqCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
ieqCouple =
    fun2 vec2T "ieq" (vec2T "l") (vec2T "r") <| \l r -> return <| subtract l r.yx


ieqDecl : FunDecl
ieqDecl =
    Tuple.first ieqCouple


ieq : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
ieq =
    Tuple.second ieqCouple


igeqCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
igeqCouple =
    fun2 vec2T "igeq" (vec2T "l") (vec2T "r") <| \l r -> return <| subtract l r.yx


igeqDecl : FunDecl
igeqDecl =
    Tuple.first igeqCouple


igeq : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
igeq =
    Tuple.second igeqCouple


igtCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
igtCouple =
    fun2 vec2T "igt" (vec2T "l") (vec2T "r") <| \l r -> return <| subtract l r.yx


igtDecl : FunDecl
igtDecl =
    Tuple.first igtCouple


igt : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
igt =
    Tuple.second igtCouple


iabsCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
iabsCouple =
    fun1 vec2T "iabs" (vec2T "z") <| \z ->
    return <|
        ternary
            (ands [ leq z.x zero, geq z.y zero ])
            (vec2 zero <| max_ z.y <| abs_ z.x)
            (ternary
                (leq z.x zero)
                (negate_ z.yx)
                z
            )


iabsDecl : FunDecl
iabsDecl =
    Tuple.first iabsCouple


iabs : ExpressionX xa Vec2 -> Expression2
iabs =
    Tuple.second iabsCouple


iacosCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
iacosCouple =
    fun1 vec2T "iacos" (vec2T "z") <| \z nop ->
    -- Don't use clamp so if z is fully outside range the result is empty
    def vec2T "clamped" (vec2 (max_ z.x (negate_ one)) (min_ z.y one)) <| \clamped ->
    return (acos2 clamped).yx nop


iacosDecl : FunDecl
iacosDecl =
    Tuple.first iacosCouple


iacos : ExpressionX xa Vec2 -> Expression2
iacos =
    Tuple.second iacosCouple


gacosCouple : ( FunDecl, ExpressionX xa Vec4 -> Expression4 )
gacosCouple =
    fun1 vec4T "gacos" (vec4T "v") <| \v ->
    return <|
        vec4_1_3
            (acos_ v.x)
            (divF (negate_ v.yzw) (sqrt_ (subtract one (by v.x v.x))))


gacosDecl : FunDecl
gacosDecl =
    Tuple.first gacosCouple


gacos : ExpressionX xa Vec4 -> Expression4
gacos =
    Tuple.second gacosCouple


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
    fun1 vec2T "iexp" (vec2T "z") <| \z -> return <| exp z


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
    fun1 vec2T "iln" (vec2T "z") <| \z -> return <| log z


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
    fun1 vec2T "csin" (vec2T "z") <| \z nop ->
    def floatT "s" (sin_ z.x) <| \s ->
    return
        (ternary
            (eq z.y zero)
            (vec2 s zero)
            (vec2
                (by s (cosh z.y))
                (by (cos_ z.x) (sinh z.y))
            )
        )
        nop


csinDecl : FunDecl
csinDecl =
    Tuple.first csinCouple


csin : ExpressionX xa Vec2 -> Expression2
csin =
    Tuple.second csinCouple


ccosCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
ccosCouple =
    fun1 vec2T "ccos" (vec2T "z") <| \z nop ->
    def floatT "c" (cos_ z.x) <| \c ->
    return
        (ternary
            (eq z.y zero)
            (vec2 c zero)
            (vec2
                (by c (cosh z.y))
                (by (sin_ z.x) (sinh z.y))
            )
        )
        nop


ccosDecl : FunDecl
ccosDecl =
    Tuple.first ccosCouple


ccos : ExpressionX xa Vec2 -> Expression2
ccos =
    Tuple.second ccosCouple


ctanCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
ctanCouple =
    fun1 vec2T "ctan" (vec2T "z") <| \z ->
    return <|
        ternary (eq z.y zero)
            (vec2 (tan_ z.x) zero)
            (cdiv (csin z) (ccos z))


ctanDecl : FunDecl
ctanDecl =
    Tuple.first ctanCouple


ctan : ExpressionX xa Vec2 -> Expression2
ctan =
    Tuple.second ctanCouple


casinTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
casinTuple =
    fun1 vec2T "casin" (vec2T "z") <| \z nop ->
    def vec2T "s" (csqrt <| subtract (vec2 one zero) (cby z z)) <| \s ->
    def vec2T "arg" (subtract s <| cby (vec2 zero one) z) <| \arg_ ->
    return (cby (vec2 zero one) (cln arg_)) nop


casinDecl : FunDecl
casinDecl =
    Tuple.first casinTuple


casin : ExpressionX xa Vec2 -> Expression2
casin =
    Tuple.second casinTuple


cacosTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
cacosTuple =
    fun1 vec2T "cacos" (vec2T "z") <| \z ->
    return <| subtract (vec2 constants.pihalf zero) (casin z)


cacosDecl : FunDecl
cacosDecl =
    Tuple.first cacosTuple


cacos : ExpressionX xa Vec2 -> Expression2
cacos =
    Tuple.second cacosTuple


catanTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
catanTuple =
    fun1 vec2T "catan" (vec2T "z") <| \z nop ->
    if_ (eq z.y zero)
        (return <| vec2 (atan_ z.x) zero)
    <| \_ ->
    def vec2T "o" (vec2 one zero) <| \o ->
    def vec2T "iz" (cby (vec2 zero one) z) <| \iz ->
    def vec2T "l" (cdiv (add o iz) (subtract o iz)) <| \l ->
    return (byF (float -0.5) <| cby (vec2 zero one) (cln l)) nop


catanDecl : FunDecl
catanDecl =
    Tuple.first catanTuple


catan : ExpressionX xa Vec2 -> Expression2
catan =
    Tuple.second catanTuple


catan2Tuple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
catan2Tuple =
    fun2 vec2T "catan2" (vec2T "y") (vec2T "x") <| \y x nop ->
    def vec2T "z" (vec2 (subtract x.x y.y) (add x.y y.x)) <| \z ->
    return (vec2 (atan2_ z.y z.x) zero) nop


catan2Decl : FunDecl
catan2Decl =
    Tuple.first catan2Tuple


catan2 : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
catan2 =
    Tuple.second catan2Tuple


csinhTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
csinhTuple =
    fun1 vec2T "csinh" (vec2T "z") <| \z ->
    return <| byF (float 0.5) (subtract (cexp z) (cexp <| negate_ z))


csinhDecl : FunDecl
csinhDecl =
    Tuple.first csinhTuple


csinh : ExpressionX xa Vec2 -> Expression2
csinh =
    Tuple.second csinhTuple


ccoshTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
ccoshTuple =
    fun1 vec2T "ccosh" (vec2T "z") <| \z ->
    return <| byF (float 0.5) (add (cexp z) (cexp <| negate_ z))


ccoshDecl : FunDecl
ccoshDecl =
    Tuple.first ccoshTuple


ccosh : ExpressionX xa Vec2 -> Expression2
ccosh =
    Tuple.second ccoshTuple


ctanhCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
ctanhCouple =
    fun1 vec2T "ctanh" (vec2T "z") <| \z nop ->
    def vec2T "p" (cexp z) <| \p ->
    def vec2T "m" (cexp (negate_ z)) <| \m ->
    return (cdiv (subtract p m) (add p m)) nop


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
    fun1 vec2T "csign" (vec2T "z") <| \z -> return <| sign z


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
                fun1 floatT "tanh" (floatT "x") <| \x nop ->
                if_ (gt (abs_ x) (float 10))
                    (return <| sign x)
                <| \_ ->
                def floatT "p" (exp x) <| \p ->
                def floatT "m" (exp <| negate_ x) <| \m ->
                return (div (subtract p m) (add p m)) nop
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


cexpFunction : Expression2 -> Continue () -> Statement Vec2
cexpFunction z =
    return <|
        byF (exp z.x) <|
            ternary
                (eq z.y zero)
                (vec2 one zero)
                (vec2 (cos_ z.y) (sin_ z.y))


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
    fun1 vec2T "cround" (vec2T "z") <| \z ->
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
    fun2 vec2T "cmod" (vec2T "l") (vec2T "r") <| \l r ->
    return <| vec2 (mod l.x r.x) zero


cmodDecl : FunDecl
cmodDecl =
    Tuple.first cmodCouple


cmod : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
cmod =
    Tuple.second cmodCouple


cmbrotCouple : ( FunDecl, ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2 )
cmbrotCouple =
    fun2 vec2T "cmbrot" (vec2T "x") (vec2T "y") <| \x y nop ->
    def vec2T "c" (add x (vec2 (negate_ y.y) y.x)) <| \c ->
    def floatT "p" (length (subtract c (vec2 (float 0.25) zero))) <| \p ->
    if_ (or (leq c.x (add (subtract p (by (by (float 2) p) p)) (float 0.25))) (leq (length (add c (vec2 one zero))) (float 0.25)))
        (return vec2Zero)
    <| \_ ->
    def vec2T "z" c <| \z ->
    for ( "i", int 0, int 4000 )
        (\i cont ->
            expr (assign z (add (vec2 (subtract (by z.x z.x) (by z.y z.y)) (by (by (float 2) z.x) z.y)) c)) <| \_ ->
            if_ (gt (length z) (float 1000000))
                (\_ ->
                    def floatT "logLength" (log (length z)) <| \logLength ->
                    def floatT "nu" (div (log (div logLength (log (float 2)))) (log (float 2))) <| \nu ->
                    def floatT "fi" (subtract (floatCast i) nu) <| \fi ->
                    return (vec2 (sin_ fi) (cos_ fi)) nop
                )
            <|
                cont
        )
    <| \_ ->
    return vec2Zero nop


cmbrotDecl : FunDecl
cmbrotDecl =
    Tuple.first cmbrotCouple


cmbrot : ExpressionX xa Vec2 -> ExpressionX xb Vec2 -> Expression2
cmbrot =
    Tuple.second cmbrotCouple


csqrtTuple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
csqrtTuple =
    fun1 vec2T "csqrt" (vec2T "z") <| \z nop ->
    if_ (ands [ eq z.y zero, geq z.x zero ])
        (return <| vec2 (sqrt_ z.x) zero)
    <| \_ ->
    def floatT "r" (pow (dot z z) (float 0.25)) <| \r ->
    def floatT "t" (byF (float 0.5) (atan2_ z.y z.x)) <| \t ->
    return (byF r <| vec2 (cos_ t) (sin_ t)) nop


csqrtDecl : FunDecl
csqrtDecl =
    Tuple.first csqrtTuple


csqrt : ExpressionX xa Vec2 -> Expression2
csqrt =
    Tuple.second csqrtTuple


ccbrtCouple : ( FunDecl, ExpressionX xa Vec2 -> Expression2 )
ccbrtCouple =
    fun1 vec2T "ccbrt" (vec2T "z") <| \z nop ->
    if_ (eq z.y zero)
        (return <| vec2 (by (sign z.x) (pow z.x (div one (float 3)))) zero)
    <| \_ ->
    def floatT "r" (pow (dot z z) (div one (float 6))) <| \r ->
    def floatT
        "t"
        (add (div (atan2_ z.y z.x) (float 3))
            (ternary
                (gt z.x zero)
                zero
                (radians_ (float 120))
            )
        )
    <| \t ->
    return (byF r (vec2 (cos_ t) (sin_ t))) nop


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
    fun1 vec2T "cln" (vec2T "z") <| \z nop ->
    if_ (ands [ eq z.y zero, geq z.x zero ])
        (return <| vec2 (log z.x) zero)
    <| \_ ->
    def floatT "px" (length z) <| \px ->
    def floatT "py" (atan2_ z.y z.x) <| \py ->
    return (vec2 (log px) py) nop


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
            fileToGlsl [ iacosDecl, gacosDecl ]

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
                return sqrt(max(vec2(0,0), v));
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
                return min(l, r);
            }

            vec4 gmin(vec4 l, vec4 r) {
                return l.x < r.x ? l : r;
            }
            """

        Max222 ->
            """
            vec2 imax(vec2 l, vec2 r) {
                return max(l, r);
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
            fun2 floatT ("f" ++ suffix) (floatT "x") (floatT "y") <| \x y nop ->
            def vec2T "complex" (expressionToGlsl [ ( "x", x ), ( "y", y ) ] e) <| \complex ->
            return
                (ternary (gt (abs_ complex.y) (float epsilon))
                    zero
                    (ternary (gt complex.x zero) one minusOne)
                )
                nop

        ( pixelDecl, pixel ) =
            fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <| \deltaX _ x y cont ->
            def floatT "sum" zero <| \sum ->
            def floatT "samples" (float <| antialiasingSamples * 2 + 1) <| \samples ->
            assignBy samples samples <| \_ ->
            def floatT "coeff" (float 0.0875) <| \coeff ->
            forLeq ( "w", int <| -antialiasingSamples, int antialiasingSamples )
                (\w cont2 ->
                    forLeq ( "h", int <| -antialiasingSamples, int antialiasingSamples )
                        (\h _ ->
                            def floatT
                                "piece"
                                (f
                                    (add x <| by (by deltaX coeff) <| floatCast w)
                                    (add y <| by (by deltaX coeff) <| floatCast h)
                                )
                            <| \piece ->
                            if_ (eq piece zero)
                                (return vec3Zero)
                            <| \_ ->
                            assignAdd sum piece cont2
                        )
                        cont2
                )
            <| \_ ->
            def floatT "perc" (div (subtract samples <| abs_ sum) samples) <| \perc ->
            expr (assign perc (pow perc <| float 0.2)) <| \_ ->
            return (byF perc (vec3 one one one)) cont
    in
    ( [ fDecl, pixelDecl ], pixel )


toSrcPolar :
    String
    -> Expression.Expression
    -> ( List FunDecl, ExpressionX xa Float -> ExpressionX xb Float -> ExpressionX xc Float -> ExpressionX xd Float -> Expression3 )
toSrcPolar suffix e =
    let
        ( fDecl, f ) =
            fun4 floatT ("f" ++ suffix) (floatT "x") (floatT "y") (floatT "deltaT") (floatT "ot") <| \x y deltaT ot cont ->
            def floatT "r" (length <| vec2 x y) <| \r ->
            def floatT "t" (add (atanPlus y x) deltaT) <| \t ->
            -- Avoid the branch cut at {x > 0, y = 0}
            if_ (gt (abs_ <| subtract t ot) constants.pi)
                (assignAdd t (ternary (lt t ot) constants.twopi (negate_ constants.twopi)))
            <| \_ ->
            def vec2T "complex" (expressionToGlsl [ ( "x", x ), ( "y", y ), ( "r", r ), ( "t", t ) ] e) <| \complex ->
            return
                (ternary (gt (abs_ complex.y) (float epsilon))
                    minusOne
                    (ternary (gt complex.x zero)
                        one
                        zero
                    )
                )
                cont

        ( pixelDecl, pixel ) =
            fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <| \deltaX deltaY x y cont ->
            assignAdd x (div deltaX <| float 2) <| \_ ->
            assignAdd y (div deltaY <| float 2) <| \_ ->
            def floatT "t" zero <| \t ->
            def floatT "ot" (atanPlus y x) <| \ot ->
            for ( "i", int 0, divConst constants.maxIterations (int 10) )
                (\_ cont2 ->
                    def floatT "h" (f x y t ot) <| \h ->
                    def floatT "l" (f (subtract x deltaX) y t ot) <| \l ->
                    def floatT "u" (f x (subtract y deltaY) t ot) <| \u ->
                    def floatT "ul" (f (subtract x deltaX) (subtract y deltaY) t ot) <| \ul ->
                    if_ (ors [ lt h zero, lt l zero, lt u zero, lt ul zero ])
                        break
                    <| \_ ->
                    if_ (ors [ neq h l, neq h u, neq h ul ])
                        (return <| vec3 one one one)
                    <| \_ ->
                    assignAdd t constants.twopi <| \_ ->
                    assignAdd ot constants.twopi cont2
                )
            <| \_ ->
            return vec3Zero cont
    in
    ( [ fDecl, pixelDecl ], pixel )


constants :
    { maxIterations : ExpressionX { isConstant : Constant } Int
    , maxDepth : ExpressionX { isConstant : Constant } Int
    , pihalf : ExpressionX { isConstant : Constant } Float
    , pi : ExpressionX { isConstant : Constant } Float
    , twopi : ExpressionX { isConstant : Constant } Float
    , vectorSpacing : ExpressionX { isConstant : Constant } Float
    , xPoints : ExpressionX { isConstant : Constant } Int
    , yPoints : ExpressionX { isConstant : Constant } Int
    }
constants =
    { maxIterations = constant intT "MAX_ITERATIONS"
    , maxDepth = constant intT "MAX_DEPTH"
    , pihalf = constant floatT "PIHALF"
    , pi = constant floatT "PI"
    , twopi = constant floatT "TWOPI"
    , vectorSpacing = constant floatT "VECTOR_SPACING"
    , xPoints = constant intT "X_POINTS"
    , yPoints = constant intT "Y_POINTS"
    }


toSrcParametric : String -> Expression.Expression -> ( List FunDecl, ExpressionX xa Float -> ExpressionX xb Float -> ExpressionX xc Float -> ExpressionX xd Float -> Expression3 )
toSrcParametric suffix e =
    let
        ( intervalDecl, interval ) =
            fun3 vec2T ("interval" ++ suffix) (vec2T "p") (floatT "from") (floatT "to") <| \p from to cont ->
            def vec2T "x" (vec2 p.x p.x) <| \x ->
            def vec2T "y" (vec2 p.y p.y) <| \y ->
            def vec2T "t" (vec2 (min_ from to) (max_ from to)) <| \t ->
            return
                (expressionToIntervalGlsl
                    (Dict.fromList
                        [ ( "x", x )
                        , ( "y", y )
                        , ( "t", t )
                        ]
                    )
                    (toPrintExpression e)
                )
                cont

        ( pixelDecl, pixel ) =
            fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <| \deltaX deltaY x y cont ->
            def floatT "max_distance" (float <| 2 ^ 10) <| \max_distance ->
            def floatT "from" (div (negate_ max_distance) (float 2)) <| \from ->
            def floatT "to" (div max_distance (float 2)) <| \to ->
            def vec2T "p" (vec2 x y) <| \p ->
            def intT "depth" (int 0) <| \depth ->
            def intT "choices" (int 0) <| \choices ->
            def floatT "ithreshold" (by (by (float 10) deltaX) deltaX) <| \ithreshold ->
            for ( "it", int 0, constants.maxIterations )
                (innerLoop from to p depth choices ithreshold)
            <| \_ ->
            return vec3Zero cont

        innerLoop :
            Expression1 Float
            -> Expression1 Float
            -> Expression2
            -> Expression1 Int
            -> Expression1 Int
            -> Expression1 Float
            -> Expression1 Int
            -> Continue Vec3
            -> Statement Vec3
        innerLoop from to p depth choices ithreshold _ cont =
            def floatT "midpoint" (mix from to <| float 0.5) <| \midpoint ->
            def vec2T "front" (interval p from midpoint) <| \front ->
            def vec2T "back" (interval p midpoint to) <| \back ->
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
            <| \_ ->
            ifElse
                (ands [ leq front.x zero, geq front.y zero ])
                (\_ ->
                    expr (assign to midpoint) <| \_ ->
                    expr (postfixIncrement depth) <| \_ ->
                    assignBy choices (int 2) cont
                )
                (ifElse (ands [ leq back.x zero, geq back.y zero ])
                    (\_ ->
                        expr (assign from midpoint) <| \_ ->
                        expr (postfixIncrement depth) <| \_ ->
                        expr (assign choices (add (by choices (int 2)) (int 1))) cont
                    )
                    -- This could be possibly helped by https://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightBinSearch
                    (\_ ->
                        forDown ( "j", subtractConst constants.maxDepth (int 1), int 0 )
                            (\j _ ->
                                if_ (gt j depth)
                                    continue
                                <| \_ ->
                                expr (postfixDecrement depth) <| \_ ->
                                expr (assign choices (div choices (int 2))) <| \_ ->
                                ifElse (eq (by (div choices (int 2)) (int 2)) choices)
                                    (\_ ->
                                        expr (assign midpoint to) <| \_ ->
                                        expr (assign to (add to (subtract to from))) <| \_ ->
                                        expr (assign back (interval p midpoint to)) <| \_ ->
                                        if_ (ands [ leq back.x zero, geq back.y zero ])
                                            (\_ ->
                                                expr (assign from midpoint) <| \_ ->
                                                expr (postfixIncrement depth) <| \_ ->
                                                expr (assign choices (add (by choices (int 2)) (int 1))) <| \_ ->
                                                break cont
                                            )
                                            cont
                                    )
                                    (expr (assign from (subtract from (subtract to from))))
                                    cont
                            )
                        <| \_ ->
                        if_ (eq depth (int 0))
                            (return vec3Zero)
                            cont
                    )
                )
                cont
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
    fun1 floatT "thetaDelta" (floatT "theta") <| \theta cont ->
    if_ (lt uniforms.u_whiteLines one)
        (return <| float 100)
    <| \_ ->
    def floatT "thetaSix" (add (by theta uniforms.u_whiteLines) (float 0.5)) <| \thetaSix ->
    def floatT "thetaNeigh" (float 0.05) <| \thetaNeigh ->
    return (divF (abs_ (subtract (fract thetaSix) (float 0.5))) thetaNeigh) cont


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
            fun2 vec2T ("vector" ++ suffix) (floatT "x") (floatT "y") <| \x y cont ->
            def vec2T "xv" (expressionToGlsl [ ( "x", x ), ( "y", y ) ] xexpr) <| \xv ->
            def vec2T "yv" (expressionToGlsl [ ( "x", x ), ( "y", y ) ] yexpr) <| \yv ->
            return (ternary (lt (add (abs_ xv.y) (abs_ yv.y)) (float epsilon)) (vec2 xv.x yv.x) vec2Zero) cont

        ( nearDecl, near ) =
            fun5 boolT "near" (vec2T "o") (vec2T "corner") (vec2T "vect") (floatT "deltaX") (floatT "mx") <| \o corner vect deltaX mx cont ->
            def floatT "angleCorner" (arg <| subtract o corner) <| \angleCorner ->
            def floatT "angleVector" (arg vect) <| \angleVector ->
            def floatT "delta" (mod (subtract angleCorner angleVector) constants.twopi) <| \delta ->
            def floatT "l" (div (length vect) mx) <| \l ->
            def floatT
                "maxLength"
                (by (by deltaX constants.vectorSpacing)
                    (ternary
                        (lt l (float epsilon))
                        zero
                        (add (div l (float 2)) (float 0.5))
                    )
                )
            <| \maxLength ->
            def floatT "wantedLength" (length (subtract o corner)) <| \wantedLength ->
            def floatT "angularDistance" (mix (float 180) zero (pow (div wantedLength maxLength) (float 0.3))) <| \angularDistance ->
            return
                (ands
                    [ ors
                        [ lt delta (radians_ angularDistance)
                        , gt delta (radians_ (subtract (float 360) angularDistance))
                        ]
                    , lt wantedLength maxLength
                    ]
                )
                cont

        ( pixelDecl, pixel ) =
            fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <| \deltaX deltaY x y cont ->
            def vec2T "o" (vec2 x y) <| \o ->
            def floatT "mx" zero <| \mx ->
            def floatT "k" (by deltaX constants.vectorSpacing) <| \k ->
            forLeq ( "xi", negateConst constants.xPoints, constants.xPoints )
                (\xi cont2 ->
                    forLeq ( "yi", negateConst constants.yPoints, constants.yPoints )
                        (\yi _ ->
                            def vec2T
                                "p"
                                (add uniforms.u_zoomCenter
                                    (vec2
                                        (by k (floatCast xi))
                                        (by k (floatCast yi))
                                    )
                                )
                            <| \p ->
                            def vec2T "v" (vector p.x p.y) <| \v ->
                            expr (assign mx (max_ mx (length v))) cont2
                        )
                        cont2
                )
            <| \_ ->
            def vec3T "colorA" (vec3 zero one zero) <| \colorA ->
            def vec3T "colorB" (vec3 zero zero one) <| \colorB ->
            expr (assign x (subtract o.x (mod o.x k))) <| \_ ->
            expr (assign y (subtract o.y (mod o.y k))) <| \_ ->
            def vec2T "bl" (vector x y) <| \bl ->
            def vec2T "br" (vector (add x k) y) <| \br ->
            def vec2T "ul" (vector x (add y k)) <| \ul ->
            def vec2T "ur" (vector (add x k) (add y k)) <| \ur ->
            def vec2T "corner" (vec2 x y) <| \corner ->
            def floatT "l" (div (length bl) mx) <| \l ->
            if_ (near o corner bl deltaX mx)
                (return <| mix colorA colorB l)
            <| \_ ->
            expr (assign corner (vec2 (add x (by deltaX constants.vectorSpacing)) y)) <| \_ ->
            expr (assign l (div (length br) mx)) <| \_ ->
            if_ (near o corner br deltaX mx)
                (return <| mix colorA colorB l)
            <| \_ ->
            expr (assign corner (vec2 x (add y (by deltaX constants.vectorSpacing)))) <| \_ ->
            expr (assign l (div (length ul) mx)) <| \_ ->
            if_ (near o corner ul deltaX mx)
                (return <| mix colorA colorB l)
            <| \_ ->
            expr (assign corner (vec2 (add x (by deltaX constants.vectorSpacing)) (add y (by deltaX constants.vectorSpacing)))) <| \_ ->
            expr (assign l (div (length ur) mx)) <| \_ ->
            if_ (near o corner ur deltaX mx)
                (return <| mix colorA colorB l)
            <| \_ ->
            return vec3Zero cont
    in
    ( [ vectorDecl, nearDecl, pixelDecl ], pixel )


toSrcContour : String -> Expression.Expression -> ( List FunDecl, ExpressionX xa Float -> ExpressionX xb Float -> ExpressionX xc Float -> ExpressionX xd Float -> Expression3 )
toSrcContour suffix expr =
    let
        ( pixelODecl, pixelO ) =
            fun4 vec3T ("pixel" ++ suffix ++ "_o") (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <| \_ _ x y cont ->
            def vec2T "z" (expressionToGlsl [ ( "x", x ), ( "y", y ) ] expr) <| \z ->
            def floatT "theta" (div (atan2_ z.y z.x) constants.twopi) <| \theta ->
            def floatT "logRadius" (log2 <| length z) <| \logRadius ->
            def floatT "powerRemainder" (fract logRadius) <| \powerRemainder ->
            def floatT "squished" (subtract (float 0.7) (by powerRemainder <| float 0.4)) <| \squished ->
            if_ (gt uniforms.u_completelyReal zero)
                (\_ ->
                    def floatT "haf" (div powerRemainder <| float 6.0) <| \haf ->
                    return
                        (ternary (gt z.x zero)
                            (hl2rgb (add haf <| float 0.3) squished)
                            (hl2rgb (subtract haf <| float 0.2) (subtract one squished))
                        )
                        cont
                )
            <| \_ ->
            def floatT "td" (thetaDelta theta) <| \td ->
            def floatT "l" (mix one squished <| smoothstep zero one td) <| \l ->
            return (hl2rgb theta l) cont

        ( pixelDecl, pixel ) =
            let
                dist =
                    1 / 3
            in
            fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <| \deltaX deltaY x y cont ->
            def floatT "deltaXr" (by deltaX <| float dist) <| \deltaXr ->
            def floatT "deltaYr" (by deltaY <| float dist) <| \deltaYr ->
            -- Antialiasing
            def vec3T "a" (pixelO deltaX deltaY (add x deltaXr) (add y deltaYr)) <| \a ->
            def vec3T "b" (pixelO deltaX deltaY (subtract x deltaXr) (subtract y deltaYr)) <| \b ->
            def vec3T "c" (pixelO deltaX deltaY (add x deltaXr) (subtract y deltaYr)) <| \c ->
            def vec3T "d" (pixelO deltaX deltaY (subtract x deltaXr) (add y deltaYr)) <| \d ->
            def vec3T "mn" (min_ a <| min_ b <| min_ c d) <| \mn ->
            def vec3T "mx" (max_ a <| max_ b <| max_ c d) <| \mx ->
            def vec3T "diff" (abs_ (subtract mx mn)) <| \diff ->
            if_ (ands [ lt diff.x (float dist), lt diff.y (float dist), lt diff.z (float dist) ])
                (return <| divF (adds3 [ a, b, c, d ]) (float 4))
            <| \_ ->
            assignBy deltaXr (float 2) <| \_ ->
            assignBy deltaYr (float 2) <| \_ ->
            def vec3T "e" (pixelO deltaX deltaY (add x deltaXr) y) <| \e ->
            def vec3T "f" (pixelO deltaX deltaY (subtract x deltaXr) y) <| \f ->
            def vec3T "g" (pixelO deltaX deltaY x (subtract y deltaYr)) <| \g ->
            def vec3T "h" (pixelO deltaX deltaY x (add y deltaYr)) <| \h ->
            return (divF (adds3 [ a, b, c, d, e, f, g, h ]) (float 8)) cont
    in
    ( [ pixelODecl, pixelDecl ], pixel )


toSrcRelation : String -> Expression.Expression -> ( List FunDecl, ExpressionX xa Float -> ExpressionX xb Float -> ExpressionX xc Float -> ExpressionX xd Float -> Expression3 )
toSrcRelation suffix e =
    Tuple.mapFirst (\d -> [ d ]) <|
        fun4 vec3T ("pixel" ++ suffix) (floatT "deltaX") (floatT "deltaY") (floatT "x") (floatT "y") <| \_ _ x y cont ->
        def vec2T "complex" (expressionToGlsl [ ( "x", x ), ( "y", y ) ] e) <| \complex ->
        return
            (ternary
                (ands
                    [ gt complex.x zero
                    , lt (abs_ complex.y) (float epsilon)
                    ]
                )
                (vec3 (float 0.8) (float 0.5) (float 0.5))
                vec3Zero
            )
            cont


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
                            vec2Zero

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
                            vec2Zero

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

                PApply (KnownFunction Round) [ e ] ->
                    cround (go e)

                PApply (KnownFunction Floor) [ e ] ->
                    cfloor (go e)

                PApply (KnownFunction Ceiling) [ e ] ->
                    cceiling (go e)

                PApply (KnownFunction Pw) [ c, t, f ] ->
                    cpw (go c) (go t) (go f)

                PApply (KnownFunction Mod) [ l, r ] ->
                    cmod (go l) (go r)

                PApply (KnownFunction Mbrot) [ x, y ] ->
                    cmbrot (go x) (go y)

                PApply (KnownFunction Min) es ->
                    variadic cmin es

                PApply (KnownFunction Max) es ->
                    variadic cmax es

                PReplace var e ->
                    go (Expression.pfullSubstitute var e)

                -- If this happens, it's too late
                PApply _ _ ->
                    vec2Zero

                PList _ ->
                    vec2Zero

                PLambda _ _ ->
                    vec2Zero
    in
    toPrintExpression >> go


expressionToIntervalGlsl : Dict String Expression2 -> PrintExpression -> Expression2
expressionToIntervalGlsl vars expr =
    let
        unsafeApply name ex =
            let
                _ =
                    Debug.todo
            in
            dotted2 <| unsafeCall name (List.map (go >> .base) ex)

        go =
            expressionToIntervalGlsl vars
    in
    case expr of
        PVariable "pi" ->
            dup constants.pi

        PVariable "e" ->
            dup <| exp one

        PVariable v ->
            case Dict.get v vars of
                Just w ->
                    w

                Nothing ->
                    vec2Zero

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
                        vec2Zero

                    head :: tail ->
                        dotted2 <| List.foldl (\e a -> unsafeCall ("i" ++ Expression.functionNameToString name) [ a, e.base ]) head.base tail

            else
                unsafeApply ("i" ++ Expression.functionNameToString name) ex

        PReplace var e ->
            go (Expression.pfullSubstitute var e)

        -- If this happens, it's too late
        PLambda _ _ ->
            vec2Zero

        PList _ ->
            vec2Zero


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



-- mainGlsl :
--     Bool
--     ->
--         List
--             { call : Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression3
--             , color : Bool
--             }
--     -> List String
--     -> String


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
    fun0 vec4T "pixel2" <| \cont ->
    def vec2T "canvasSize" (vec2 uniforms.u_canvasWidth uniforms.u_canvasHeight) <| \canvasSize ->
    def vec2T "uv_centered" (subtract2 gl_FragCoord.xy (byF (float 0.5) canvasSize)) <| \uv_centered ->
    def vec2T "viewportSize" (byF (div uniforms.u_viewportWidth uniforms.u_canvasWidth) canvasSize) <| \viewportSize ->
    def vec2T "uv" (by2 (div2 uv_centered canvasSize) viewportSize) <| \uv ->
    def vec2T "c" (add uniforms.u_zoomCenter uv) <| \c ->
    def floatT "x" c.x <| \x ->
    def floatT "y" c.y <| \y ->
    def floatT "deltaX" (div uniforms.u_viewportWidth uniforms.u_canvasWidth) <| \deltaX ->
    def floatT "deltaY" (div uniforms.u_viewportWidth uniforms.u_canvasHeight) <| \deltaY ->
    def vec3T "px" vec3Zero <| \px ->
    decl vec3T "curr" <| \curr ->
    inner deltaX deltaY x y px curr <|
        def floatT "maxDelta" (max_ deltaX deltaY) <| \maxDelta ->
        def vec3T "yax" (ternary3 (eq uniforms.u_drawAxes one) (byF (axis x y maxDelta) (vec3 zero one zero)) vec3Zero) <| \yax ->
        def vec3T "xax" (ternary3 (eq uniforms.u_drawAxes one) (byF (axis y x maxDelta) (vec3 one zero zero)) vec3Zero) <| \xax ->
        return (vec4_3_1 (max3 px (max3 xax yax)) one) cont


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
    expr (assign curr k) <| \_ ->
    expr (assign px (ternary3 (eq curr vec3Zero) px curr)) <| \_ ->
    next


axisTuple : ( FunDecl, Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression1 Float )
axisTuple =
    fun3 floatT "axis" (floatT "coord") (floatT "otherCoord") (floatT "maxDelta") <| \coord otherCoord maxDelta cont ->
    def floatT "across" (subtract one <| abs_ <| div coord maxDelta) <| \across ->
    if_ (lt across (float -12))
        (return zero)
    <| \_ ->
    def floatT "smallUnit" (pow (float 10) (ceil_ (log10 maxDelta))) <| \smallUnit ->
    if_ (ands [ lt across zero, lt (abs_ otherCoord) (by maxDelta (float 2)) ])
        (return zero)
    <| \_ ->
    def floatT
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
    <| \unit ->
    def floatT "parallel" (ternary (lt (mod (abs_ otherCoord) unit) maxDelta) one zero) <| \parallel ->
    return (max_ zero (max_ across parallel)) cont


axis : Expression1 Float -> Expression1 Float -> Expression1 Float -> Expression1 Float
axis =
    Tuple.second axisTuple


axisDecl : FunDecl
axisDecl =
    Tuple.first axisTuple


toSrc3D :
    String
    -> Expression.Expression
    -> ( List FunDecl, ExpressionX xa Mat3 -> ExpressionX xb Mat3 -> Expression2 )
toSrc3D suffix e =
    let
        ( intervalDecl, interval ) =
            fun2 vec2T ("interval" ++ suffix) (mat3T "f") (mat3T "t") <| \f t cont ->
            def vec3T "mn" (min_ (arr f <| int 0) (arr t <| int 0)) <| \mn ->
            def vec3T "mx" (max_ (arr f <| int 1) (arr t <| int 1)) <| \mx ->
            def vec2T "x" (vec2 mn.x mx.x) <| \x ->
            def vec2T "y" (vec2 mn.y mx.y) <| \y ->
            def vec2T "z" (vec2 mn.z mx.z) <| \z ->
            return (expressionToIntervalGlsl (Dict.fromList [ ( "x", x ), ( "y", y ), ( "z", z ) ]) <| toPrintExpression e) cont
    in
    ( [ Tuple.first <|
            fun1 vec3T ("normal" ++ suffix) (vec3T "p") <| \p cont ->
            def floatT "x" p.x <| \x ->
            def floatT "y" p.y <| \y ->
            def floatT "z" p.z <| \z ->
            def vec4T "gradient" (expressionToNormalGlsl { x = x, y = y, z = z } e) <| \gradient ->
            return (normalize gradient.yzw) cont
      , intervalDecl
      ]
    , interval
    )



--main3D : Bool -> List String -> ( String, Expression4 )


main3D : Bool -> List (Expression3 -> Expression1 Mat3 -> Expression1 Float -> Expression3 -> Expression1 Bool) -> ( String, Expression4 )
main3D rayDifferentials bisects =
    let
        kValue =
            if rayDifferentials then
                0.001

            else
                0.0

        ( block, pixel3 ) =
            fun0 vec4T "pixel3" <| \cont ->
            def floatT "eye_dist" (byF (float 2) uniforms.u_viewportWidth) <| \eyeDist ->
            def vec2T "canvas_size" (vec2 uniforms.u_canvasWidth uniforms.u_canvasHeight) <| \canvasSize ->
            def vec2T "uv_centered" (subtract gl_FragCoord.xy <| byF (float 0.5) canvasSize) <| \uvCentered ->
            def vec2T "uv_normalized" (byF (div one uniforms.u_canvasHeight) uvCentered) <| \uvNormalized ->
            def floatT "t" (add uniforms.u_theta <| float 0.58) <| \t ->
            def floatT "p" (by (float -2.0) uniforms.u_phi) <| \p ->
            def vec3T "eye" (eyePosition eyeDist t p) <| \eye ->
            def vec3T "target" vec3Zero <| \target ->
            def vec3T "to_target" (normalize <| subtract target eye) <| \toTarget ->
            def vec3T "across" (normalize <| cross toTarget <| vec3 zero zero one) <| \across ->
            def vec3T "up" (normalize <| cross across toTarget) <| \up ->
            def vec3T "canvas_center" (add eye toTarget) <| \canvasCenter ->
            def vec3T "canvas_point" (add (add canvasCenter <| byF uvNormalized.x across) (byF uvNormalized.y up)) <| \canvasPoint ->
            def vec3T "ray_direction" (normalize3 <| subtract canvasPoint eye) <| \rayDirection ->
            def vec3T "diffs" (abs_ <| fwidth rayDirection) <| \diffs ->
            def floatT "k" (float kValue) <| \k ->
            def mat3T "d" (dValue rayDirection k diffs) <| \d ->
            def floatT "max_distance" (by (float 100) eyeDist) <| \maxDistance ->
            return (raytraceF canvasPoint d maxDistance) cont

        dValue : ExpressionX a Vec3 -> ExpressionX b Float -> ExpressionX c Vec3 -> Expression33
        dValue rayDirection k diffs =
            mat3_3_3_3
                (subtract3 rayDirection <| byF k diffs)
                (add rayDirection <| byF k diffs)
                vec3Zero

        ( raytraceDecl, raytraceF ) =
            raytrace bisects

        eyePosition eyeDist t p =
            byF eyeDist <|
                normalize <|
                    vec3
                        (by (cos_ t) (sin_ p))
                        (by (cos_ t) (negate_ <| cos_ p))
                        (sin_ t)
    in
    ( fileToGlsl [ raytraceDecl, block ], pixel3 )


raytrace :
    List
        (Expression3
         -> Expression1 Mat3
         -> Expression1 Float
         -> Expression3
         -> Expression1 Bool
        )
    -> ( FunDecl, ExpressionX xa Vec3 -> ExpressionX xb Mat3 -> ExpressionX xc Float -> Expression4 )
raytrace bisects =
    let
        colorCoeff =
            float (1.19 - 0.2 * toFloat (List.length bisects))
    in
    fun3 vec4T "ratytrace" (vec3T "o") (mat3T "d") (floatT "max_distance") <| \o d maxDistance cont ->
    def vec3T "found" vec3Zero <| \found ->
    def floatT "curr_distance" maxDistance <| \currDistance ->
    def intT "found_index" (int -1) <| \foundIndex ->
    def vec3T "f" vec3Zero <| \f ->
    let
        innerTrace : Statement q -> Statement q
        innerTrace next =
            List.foldl
                (\bisect ( i, s ) ->
                    ( i + 1
                    , bisectToRay i bisect s
                    )
                )
                ( 0, next )
                bisects
                |> Tuple.second

        bisectToRay :
            Int
            ->
                (Expression3
                 -> Expression1 Mat3
                 -> Expression1 Float
                 -> Expression3
                 -> Expression1 Bool
                )
            -> Statement q
            -> Statement q
        bisectToRay i bisect next =
            if_ (ands [ bisect o d maxDistance f, lt (length (subtract f o)) currDistance ])
                (\cont2 ->
                    expr (assign foundIndex <| int i) <| \_ ->
                    expr (assign found f) <| \_ ->
                    expr (assign currDistance <| length <| subtract found o) cont2
                )
            <| \_ ->
            next
    in
    innerTrace <|
        if_ (lt foundIndex <| int 0)
            (return <| vec4 zero zero zero one)
        <| \_ ->
        def vec3T "ld" (normalize <| vec3 (float -0.3) zero one) <| \ld ->
        def vec3T "diffs" (subtract (arr d (int 1)) (arr d (int 0))) <| \diffs ->
        def floatT "light_distance" maxDistance <| \lightDistance ->
        def vec3T "offseted" (add found <| byF (by (float 0.0001) maxDistance) <| normalize <| subtract o found) <| \offseted ->
        def floatT "hue_based_on_index" (by (floatCast foundIndex) <| float <| radians (360 / 1.1)) <| \hueBasedOnIndex ->
        def mat3T "light_direction" (mat3_3_3_3 (subtract ld <| byF (float 0.5) diffs) (add ld <| byF (float 0.5) diffs) vec3Zero) <| \lightDirection ->
        def floatT "light_coeff" (ternary (ors (List.map (\bisect -> bisect offseted lightDirection lightDistance f) bisects)) (float 0.2) (float 0.45)) <| \lightCoeff ->
        def vec3T "px" (mix (hl2rgb hueBasedOnIndex lightCoeff) (hl2rgb (by (float 0.5) found.z) lightCoeff) (max_ (float 0.2) colorCoeff)) <| \px ->
        return (vec4_3_1 px one) cont


threshold : ExpressionX x Float -> Expression1 Float
threshold max_distance =
    by (float 0.000001) max_distance


suffixToBisect : (Expression33 -> Expression33 -> Expression2) -> String -> ( FunDecl, ExpressionX xa Vec3 -> ExpressionX xb Mat3 -> ExpressionX xc Float -> ExpressionX xd Vec3 -> Expression1 Bool )
suffixToBisect interval suffix =
    let
        mainLoop found from to ithreshold depth choices _ cont =
            def mat3T "midpoint" (byF (float 0.5) (add33 from to)) <| \midpoint ->
            def vec2T "front" (interval from midpoint) <| \front ->
            def vec2T "back" (interval midpoint to) <| \back ->
            if_
                (ors
                    [ geq depth constants.maxDepth
                    , ands [ lt (subtract front.y front.x) ithreshold, leq front.x zero, geq front.y zero ]
                    , ands [ lt (subtract back.y back.x) ithreshold, leq back.x zero, geq back.y zero ]
                    ]
                )
                (\_ ->
                    expr (assign found (mix (arr midpoint (int 0)) (arr midpoint (int 1)) (float 0.5))) <| \_ ->
                    return (bool True) cont
                )
            <| \_ ->
            ifElse (ands [ leq front.x zero, geq front.y zero ])
                (\_ ->
                    expr (assign to midpoint) <| \_ ->
                    expr (postfixIncrement depth) <| \_ ->
                    expr (assign choices (dotted1 <| unsafeCall "left_shift" [ choices.base ])) cont
                )
                (ifElse (ands [ leq back.x zero, geq back.y zero ])
                    (backtrackLeft from to depth choices midpoint)
                    (backtrackRight from to depth choices midpoint)
                )
                cont

        backtrackLeft from _ depth choices midpoint cont =
            expr (assign from midpoint) <| \_ ->
            expr (postfixIncrement depth) <| \_ ->
            expr (assign choices (dotted1 <| unsafeCall "left_shift_increment" [ choices.base ])) cont

        backtrackRight from to depth choices midpoint cont =
            -- This could be possibly helped by https://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightBinSearch
            forDown ( "j", subtractConst constants.maxDepth (int 1), int 0 )
                (\j _ ->
                    if_ (gt j depth)
                        continue
                    <| \_ ->
                    expr (postfixDecrement depth) <| \_ ->
                    expr (assign choices <| dotted1 <| unsafeCall "right_shift" [ choices.base ]) <| \_ ->
                    ifElse (dotted1 <| unsafeCall "is_even" [ choices.base ])
                        (\_ ->
                            expr (assign midpoint to) <| \_ ->
                            expr (assign to <| add to (subtract to from)) <| \_ ->
                            def vec2T "back" (interval midpoint to) <| \back ->
                            if_ (ands [ leq back.x zero, geq back.y zero ])
                                (\_ ->
                                    expr (assign from midpoint) <| \_ ->
                                    expr (postfixIncrement depth) <| \_ ->
                                    expr (assign choices <| dotted1 <| unsafeCall "left_shift_increment" [ choices.base ]) <| \_ ->
                                    break cont
                                )
                                cont
                        )
                        (expr (assign from <| subtract from (subtract to from)))
                        cont
                )
            <| \_ ->
            if_ (eq depth (int 0))
                (return false)
                cont
    in
    fun4 boolT ("bisect" ++ suffix) (vec3T "o") (mat3T "d") (floatT "max_distance") (out vec3T "found") <| \o d maxDistance found cont ->
    def mat3T "from" (mat3_3_3_3 o o vec3Zero) <| \from ->
    def mat3T "to" (add from <| byF maxDistance d) <| \to ->
    def floatT "ithreshold" (threshold maxDistance) <| \ithreshold ->
    def intT "depth" (int 0) <| \depth ->
    def intT "choices" (int 0) <| \choices ->
    for ( "it", int 0, constants.maxIterations )
        (mainLoop found from to ithreshold depth choices)
    <| \_ ->
    return (bool False) cont
