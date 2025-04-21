module UI.Glsl.Code exposing (expressionToGlsl, expressionToGlslComplex, mainGlsl, threshold, toSrc3D, toSrcContour, toSrcImplicit, toSrcParametric, toSrcPolar, toSrcRelation, toSrcVectorField2D, toThreshold)

import Expression exposing (Function1(..), Function2(..), Function3(..), RelationOperation(..), TGApply(..), ToGlslExpression(..))
import Glsl exposing (Expr, Expression, Vec2, Vec3, Vec4, dot2X, dot2Y, dot4XY, float1, unsafeCall0, unsafeTypecast, var)
import Glsl.Functions exposing (abs2, abs3, abs4, axis111, cabs2, cacos2, carg2, casin2, catan2, catan222, cby22, ccbrt2, cceil2, ccos2, ccosh2, cdiv22, cexp2, cfloor2, cim2, cln2, clog102, cmax22, cmbrot22, cmin22, cmod22, cos1, cpow22, cpw222, cre2, cross33, cround2, csign2, csin2, csinh2, csqrt2, ctan2, ctanh2, dup1, exp1, fwidth3, gby44, gdiv44, gneg4, gnum1, gpow41, gpow44, gsquare4, hl2rgb11, iabs2, iacos2, iarg2, iasin2, iatan2, iby22, icbrt2, iceil2, icos2, icosh2, idiv22, ieq22, iexp2, iexpand2, ifloor2, igeq22, igt22, iim2, ileq22, iln2, ilog102, ilt22, ineg2, ineq22, ipow21, ipow22, ire2, iround2, isign2, isin2, isinh2, isqrt2, isquare2, itan2, itanh2, mat3333, max11, max33, max44, normalize3, pow11, radians1, sin1, u_canvasHeight, u_canvasWidth, u_drawAxes, u_phi, u_theta, u_viewportWidth, u_zoomCenter, vec211, vec3111, vec41111, vec431)
import Glsl.Generator as Generator exposing (adds3, assign, decl, def, expr, float, fun0, gl_FragColor, gl_FragCoord, main_, mat3, one, return, ternary3, vec2, vec2T, vec2Zero, vec3, vec3T, vec3Zero, vec4T, vec4Zero, zero)
import Glsl.Operations exposing (add11, add22, add33, add44, by11, by12, by13, by22, by31, by33, div11, div22, eq, lt, negate1, negate2, subtract11, subtract22, subtract33, subtract44)
import Glsl.PrettyPrinter


expressionToGlslComplex : ToGlslExpression -> Glsl.Expression Vec2
expressionToGlslComplex =
    visit
        { expr =
            \expr ->
                case expr of
                    VVariable "i" ->
                        vec211 zero one

                    VVariable "pi" ->
                        vec211 (float1 pi) zero

                    VVariable "e" ->
                        vec211 (exp1 one) zero

                    VVariable v ->
                        vec211 (var v) zero

                    VInteger v ->
                        vec211 (float1 <| toFloat v) zero

                    VFloat f ->
                        vec211 (float1 f) zero

                    VNegate expression ->
                        negate2 expression

                    VAdd l r ->
                        add22 l r

                    VRel op l r ->
                        case op of
                            LessThan ->
                                subtract22 r l

                            LessThanOrEquals ->
                                subtract22 r l

                            NotEquals ->
                                abs2 <| subtract22 r l

                            Equals ->
                                abs2 <| subtract22 r l

                            GreaterThanOrEquals ->
                                subtract22 l r

                            GreaterThan ->
                                subtract22 l r

                    VBy l r ->
                        cby22 l r

                    VDiv l r ->
                        cdiv22 l r

                    VPower l r ->
                        cpow22 l r

                    -- If this happens, it's too late
                    VList _ ->
                        vec2Zero

                    VSubtract _ _ ->
                        Debug.todo "branch 'VSubtract _ _' not implemented"

                    VSquare _ ->
                        Debug.todo "branch 'VSquare _' not implemented"

                    VPowerF _ _ ->
                        Debug.todo "branch 'VPowerF _ _' not implemented"
        , function1 = function1ToComplex
        , function2 = function2ToComplex
        , function3 =
            \f l m r ->
                case f of
                    PPiecewise ->
                        cpw222 l m r
        }


function2ToComplex : Function2 -> Expression Vec2 -> Expression Vec2 -> Expression Vec2
function2ToComplex f =
    case f of
        PAtan2 ->
            catan222

        PMod ->
            cmod22

        PMbrot ->
            cmbrot22

        PMin ->
            cmin22

        PMax ->
            cmax22


function1ToComplex : Function1 -> Expression Vec2 -> Expression Vec2
function1ToComplex f =
    case f of
        PSin ->
            csin2

        PCos ->
            ccos2

        PTan ->
            ctan2

        PAsin ->
            casin2

        PAcos ->
            cacos2

        PAtan ->
            catan2

        PSinh ->
            csinh2

        PCosh ->
            ccosh2

        PTanh ->
            ctanh2

        PAbs ->
            cabs2

        PLn ->
            cln2

        PLog10 ->
            clog102

        PExp ->
            cexp2

        PSign ->
            csign2

        PRe ->
            cre2

        PIm ->
            cim2

        PArg ->
            carg2

        PRound ->
            cround2

        PFloor ->
            cfloor2

        PCeil ->
            cceil2

        PSqrt ->
            csqrt2

        PCbrt ->
            ccbrt2

        PGra ->
            Debug.todo "branch 'PGra' not implemented"

        PDet ->
            Debug.todo "branch 'PDet' not implemented"


expressionToIntervalGlsl : Bool -> ToGlslExpression -> Expression Vec2
expressionToIntervalGlsl expandIntervals =
    let
        expand : Expression Vec2 -> Expression Vec2
        expand e =
            if expandIntervals then
                iexpand2 e

            else
                e

        visitExpr expr =
            case expr of
                VVariable "pi" ->
                    dup1 (radians1 (float1 180))

                VVariable "e" ->
                    dup1 (float1 e)

                VVariable v ->
                    var v

                VInteger v ->
                    dup1 (float1 <| toFloat v)

                VFloat f ->
                    dup1 (float1 f)

                VNegate expression ->
                    ineg2 expression

                VAdd l r ->
                    expand <| add22 l r

                VRel op l r ->
                    comparisonToInterval op l r

                VBy l r ->
                    expand <| iby22 l r

                VDiv l r ->
                    expand <| idiv22 l r

                VSquare l ->
                    expand <| isquare2 l

                VPowerF l f ->
                    expand <| ipow21 l (float1 f)

                VPower l r ->
                    expand <| ipow22 l r

                VList [ x, y ] ->
                    vec211 x y

                VList _ ->
                    -- TODO: handle this
                    vec2Zero

                VSubtract _ _ ->
                    Debug.todo "branch 'VSubtract _ _' not implemented"
    in
    visit
        { expr = visitExpr
        , function1 =
            \f l ->
                -- sin and cos should not be expanded
                if f == PSin || f == PCos then
                    function1ToInterval f l

                else
                    expand <| function1ToInterval f l
        , function2 = Debug.todo "function2"
        , function3 = Debug.todo "function3"
        }


function1ToInterval : Function1 -> Expression Vec2 -> Expression Vec2
function1ToInterval f =
    case f of
        PSin ->
            isin2

        PCos ->
            icos2

        PTan ->
            itan2

        PAsin ->
            iasin2

        PAcos ->
            iacos2

        PAtan ->
            iatan2

        PSinh ->
            isinh2

        PCosh ->
            icosh2

        PTanh ->
            itanh2

        PAbs ->
            iabs2

        PSqrt ->
            isqrt2

        PCbrt ->
            icbrt2

        PLn ->
            iln2

        PLog10 ->
            ilog102

        PExp ->
            iexp2

        PSign ->
            isign2

        PRe ->
            ire2

        PIm ->
            iim2

        PArg ->
            iarg2

        PRound ->
            iround2

        PFloor ->
            ifloor2

        PCeil ->
            iceil2

        PGra ->
            Debug.todo "branch 'PGra' not implemented"

        PDet ->
            Debug.todo "branch 'PDet' not implemented"


comparisonToInterval : RelationOperation -> Expression Vec2 -> Expression Vec2 -> Expression Vec2
comparisonToInterval op =
    case op of
        LessThan ->
            ilt22

        LessThanOrEquals ->
            ileq22

        Equals ->
            ieq22

        NotEquals ->
            ineq22

        GreaterThanOrEquals ->
            igeq22

        GreaterThan ->
            igt22


expressionToNormalGlsl : ToGlslExpression -> Expression Vec4
expressionToNormalGlsl =
    visit
        { expr =
            \expr ->
                case expr of
                    VVariable "pi" ->
                        gnum1 (radians1 (float1 180))

                    VVariable "e" ->
                        gnum1 (exp1 one)

                    VVariable "x" ->
                        vec41111 (var "x") one zero zero

                    VVariable "y" ->
                        vec41111 (var "y") zero one zero

                    VVariable "z" ->
                        vec41111 (var "z") zero zero one

                    VVariable v ->
                        var v

                    VInteger v ->
                        gnum1 (float1 (toFloat v))

                    VFloat f ->
                        gnum1 (float1 f)

                    VNegate expression ->
                        gneg4 expression

                    VAdd l r ->
                        add44 l r

                    VRel op l r ->
                        case op of
                            LessThan ->
                                subtract44 r l

                            LessThanOrEquals ->
                                subtract44 r l

                            Equals ->
                                abs4 (subtract44 r l)

                            NotEquals ->
                                abs4 (subtract44 r l)

                            GreaterThanOrEquals ->
                                subtract44 l r

                            GreaterThan ->
                                subtract44 l r

                    VBy l r ->
                        gby44 l r

                    VDiv l r ->
                        gdiv44 l r

                    VPower l r ->
                        gpow44 l r

                    VSquare l ->
                        gsquare4 l

                    VPowerF l r ->
                        gpow41 l (float1 r)

                    VList _ ->
                        Debug.todo "branch 'VList _' not implemented"

                    VSubtract _ _ ->
                        Debug.todo "branch 'VSubtract _ _' not implemented"
        , function1 = Debug.todo "function1"
        , function2 = Debug.todo "function2"
        , function3 = Debug.todo "function3"
        }


epsilon : Float
epsilon =
    0.00001


threshold : String
threshold =
    String.fromFloat epsilon ++ " * max_distance"


toThreshold : Expression Float -> Expression Float
toThreshold maxDistance =
    by11 (float1 epsilon) maxDistance


floatToGlsl : Float -> String
floatToGlsl f =
    Glsl.PrettyPrinter.float f


toSrcImplicit : String -> Expr -> String
toSrcImplicit suffix e =
    -- let
    --     antialiasingSamples =
    --         7
    -- in
    -- """
    -- float f""" ++ suffix ++ """(float x, float y) {
    --     vec2 complex = """ ++ expressionToGlslComplex e ++ """;
    --     if(abs(complex.y) > """ ++ floatToGlsl epsilon ++ """) {
    --         return 0.0;
    --     }
    --     return complex.x > 0.0 ? 1.0 : -1.0;
    -- }
    -- vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
    --     float sum = 0.0;
    --     float samples = """ ++ floatToGlsl antialiasingSamples ++ """ * 2.0 + 1.0;
    --     samples *= samples;
    --     float coeff = 0.0875;
    --     for(int w = -""" ++ String.fromInt antialiasingSamples ++ """; w <= """ ++ String.fromInt antialiasingSamples ++ """; w++)
    --         for(int h = -""" ++ String.fromInt antialiasingSamples ++ """; h <= """ ++ String.fromInt antialiasingSamples ++ """; h++) {
    --             float piece = f""" ++ suffix ++ """(x + deltaX * coeff * float(w), y + deltaX * coeff * float(h));
    --             if(piece == 0.0)
    --                 return vec3(0);
    --             sum += piece;
    --         }
    --     float perc = (samples - abs(sum)) / samples;
    --     perc = pow(perc, 0.2);
    --     return perc * vec3(1,1,1);
    -- }
    -- """
    let
        _ =
            Debug.todo
    in
    ""


toSrcPolar : String -> Expr -> String
toSrcPolar suffix e =
    -- """
    -- float f""" ++ suffix ++ """(float x, float y, float deltaT, float ot) {
    --     float r = sqrt(x*x + y*y);
    --     float t = atanPlus(y, x) + deltaT;
    --     // Avoid the branch cut at {x > 0, y = 0}
    --     if(abs(t - ot) > radians(180.0)) {
    --         if(t < ot) t += radians(360.0);
    --         else t -= radians(360.0);
    --     }
    --     vec2 complex = """ ++ expressionToGlsl e ++ """;
    --     if(abs(complex.y) > """ ++ floatToGlsl epsilon ++ """) {
    --         return -1.0;
    --     }
    --     return complex.x > 0.0 ? 1.0 : 0.0;
    -- }
    -- vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
    --     float t = 0.0;
    --     x += deltaX / 2.0;
    --     y += deltaY / 2.0;
    --     float ot = atanPlus(y, x);
    --     for(int i = 0; i < MAX_ITERATIONS / 10; i++) {
    --         float h = f""" ++ suffix ++ """(x, y, t, ot);
    --         float l = f""" ++ suffix ++ """(x - deltaX, y, t, ot);
    --         float u = f""" ++ suffix ++ """(x, y - deltaY, t, ot);
    --         float ul = f""" ++ suffix ++ """(x - deltaX, y - deltaY, t, ot);
    --         if(h < 0.0 || l < 0.0 || u < 0.0 || ul < 0.0)
    --             break;
    --         if(h != l || h != u || h != ul)
    --             return vec3(1,1,1);
    --         t += radians(360.0);
    --         ot += radians(360.0);
    --     }
    --     return vec3(0,0,0);
    -- }
    -- """
    let
        _ =
            Debug.todo
    in
    ""


toSrcParametric : Bool -> String -> Expr -> String
toSrcParametric expandIntervals suffix e =
    -- """
    -- vec2 interval""" ++ suffix ++ """(vec2 p, float from, float to) {
    --     vec2 x = vec2(p.x,p.x);
    --     vec2 y = vec2(p.y,p.y);
    --     vec2 t = from < to ? vec2(from, to) : vec2(to, from);
    --     return """ ++ expressionToIntervalGlsl expandIntervals e ++ """;
    -- }
    -- vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
    --     float max_distance = pow(2.0, 10.0);
    --     float from = -max_distance / 2.0;
    --     float to = max_distance / 2.0;
    --     vec2 p = vec2(x, y);
    --     int depth = 0;
    --     int choices = 0;
    --     float ithreshold = 10.0 * deltaX * deltaX;
    --     for(int it = 0; it < MAX_ITERATIONS; it++) {
    --         float midpoint = mix(from, to, 0.5);
    --         vec2 front = interval""" ++ suffix ++ """(p, from, midpoint);
    --         vec2 back = interval""" ++ suffix ++ """(p, midpoint, to);
    --         if(depth >= MAX_DEPTH
    --             || (front.y - front.x < ithreshold && front.x <= 0.0 && front.y >= 0.0)
    --             || (back.y - back.x < ithreshold && back.x <= 0.0 && back.y >= 0.0)
    --             )
    --                 return vec3(1,1,1);
    --         if(front.x <= 0.0 && front.y >= 0.0) {
    --             to = midpoint;
    --             depth++;
    --             choices *= 2;
    --         } else if(back.x <= 0.0 && back.y >= 0.0) {
    --             from = midpoint;
    --             depth++;
    --             choices = choices * 2 + 1;
    --         } else {
    --             // This could be possibly helped by https://graphics.stanford.edu/~seander/bithacks.html#ZerosOnRightBinSearch
    --             for(int j = MAX_DEPTH - 1; j > 0; j--) {
    --                 if(j > depth)
    --                     continue;
    --                 depth--;
    --                 choices /= 2;
    --                 if(choices / 2 * 2 == choices) {
    --                     midpoint = to;
    --                     to = to + (to - from);
    --                     vec2 back = interval""" ++ suffix ++ """(p, midpoint, to);
    --                     if(back.x <= 0.0 && back.y >= 0.0) {
    --                         from = midpoint;
    --                         depth++;
    --                         choices = choices * 2 + 1;
    --                         break;
    --                     }
    --                 } else {
    --                     from = from - (to - from);
    --                 }
    --             }
    --             if(depth == 0)
    --                 return vec3(0,0,0);
    --         }
    --     }
    --     return vec3(0,0,0);
    -- }
    -- """
    let
        _ =
            Debug.todo
    in
    ""


toSrcVectorField2D : String -> Expression.Expression -> Expression.Expression -> String
toSrcVectorField2D suffix x y =
    -- """
    -- vec2 vector""" ++ suffix ++ """(float x, float y) {
    --     vec2 xv = """ ++ expressionToGlsl x ++ """;
    --     vec2 yv = """ ++ expressionToGlsl y ++ """;
    --     return abs(xv.y) + abs(yv.y) < """ ++ floatToGlsl epsilon ++ """ ? vec2(xv.x, yv.x) : vec2(0,0);
    -- }
    -- bool near(vec2 o, vec2 corner, vec2 vector, float deltaX, float mx) {
    --     float angleCorner = arg(o - corner);
    --     float angleVector = arg(vector);
    --     float delta = mod(angleCorner - angleVector, radians(360.0));
    --     float l = length(vector) / mx;
    --     float maxLength = deltaX * VECTOR_SPACING * (l < """ ++ floatToGlsl epsilon ++ """ ? 0.0 : l / 2.0 + 0.5);
    --     float wantedLength = length(o - corner);
    --     float angularDistance = mix(180.0, 0.0, pow(wantedLength / maxLength, 0.3));
    --     return (delta < radians(angularDistance) || delta > radians(360.0 - angularDistance)) && wantedLength < maxLength;
    -- }
    -- vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
    --     vec2 o = vec2(x, y);
    --     float mx = 0.0;
    --     for(int xi = -X_POINTS; xi <= X_POINTS; xi++) {
    --         for(int yi = -Y_POINTS; yi <= Y_POINTS; yi++) {
    --             vec2 p = u_zoomCenter + vec2(deltaX * VECTOR_SPACING * float(xi), deltaX * VECTOR_SPACING * float(yi));
    --             vec2 v = vector""" ++ suffix ++ """(p.x, p.y);
    --             mx = max(mx, length(v));
    --         }
    --     }
    --     vec3 colorA = vec3(0.0, 1.0, 0.0);
    --     vec3 colorB = vec3(0.0, 0.0, 1.0);
    --     x = o.x - mod(o.x, deltaX * VECTOR_SPACING);
    --     y = o.y - mod(o.y, deltaX * VECTOR_SPACING);
    --     vec2 bl = vector""" ++ suffix ++ """(x, y);
    --     vec2 br = vector""" ++ suffix ++ """(x + deltaX * VECTOR_SPACING, y);
    --     vec2 ul = vector""" ++ suffix ++ """(x, y + deltaX * VECTOR_SPACING);
    --     vec2 ur = vector""" ++ suffix ++ """(x + deltaX * VECTOR_SPACING, y + deltaX * VECTOR_SPACING);
    --     float angleO;
    --     vec2 corner;
    --     float l;
    --     corner = vec2(x, y);
    --     l = length(bl) / mx;
    --     if(near(o, corner, bl, deltaX, mx))
    --         return mix(colorA, colorB, l);
    --     corner = vec2(x + deltaX * VECTOR_SPACING, y);
    --     l = length(br) / mx;
    --     if(near(o, corner, br, deltaX, mx))
    --         return mix(colorA, colorB, l);
    --     corner = vec2(x, y + deltaX * VECTOR_SPACING);
    --     l = length(ul) / mx;
    --     if(near(o, corner, ul, deltaX, mx))
    --         return mix(colorA, colorB, l);
    --     corner = vec2(x + deltaX * VECTOR_SPACING, y + deltaX * VECTOR_SPACING);
    --     l = length(ur) / mx;
    --     if(near(o, corner, ur, deltaX, mx))
    --         return mix(colorA, colorB, l);
    --     return vec3(0,0,0);
    -- }
    -- """
    let
        _ =
            Debug.todo
    in
    ""


toSrcContour : String -> ToGlslExpression -> String
toSrcContour suffix e =
    -- """
    -- vec3 pixel""" ++ suffix ++ """_o(float deltaX, float deltaY, float x, float y) {
    --     vec2 z = """ ++ expressionToGlslComplex e ++ """;
    --     float theta = atan(z.y, z.x) / radians(360.0);
    --     float logRadius = log2(length(z));
    --     float powerRemainder = fract(logRadius);
    --     float squished = 0.7 - powerRemainder * 0.4;
    --     if(u_completelyReal > 0.0) {
    --         float haf = fract(logRadius) / 6.0;
    --         if(z.x > 0.0)
    --             return hl2rgb(haf + 0.3, squished);
    --         else
    --             return hl2rgb(haf - 0.1, 1.0 - squished);
    --     }
    --     float td = thetaDelta(theta);
    --     float l = mix(1.0, squished, smoothstep(0.0, 1.0, td));
    --     return hl2rgb(theta, l);
    -- }
    -- vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
    --     // Antialiasing
    --     float dist = 1.0 / 3.0;
    --     vec3 a = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x + dist * deltaX, y + dist * deltaY);
    --     vec3 b = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x - dist * deltaX, y - dist * deltaY);
    --     vec3 c = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x + dist * deltaX, y - dist * deltaY);
    --     vec3 d = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x - dist * deltaX, y + dist * deltaY);
    --     vec3 diff = abs(max(a, max(b, max(c, d))) - min(a, min(b, min(c, d))));
    --     if (diff.x < dist && diff.y < dist && diff.z < dist)
    --         return (a + b + c + d) / 4.0;
    --     vec3 e = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x + 2.0 * dist * deltaX, y);
    --     vec3 f = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x - 2.0 * dist * deltaX, y);
    --     vec3 g = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x, y - 2.0 * dist * deltaY);
    --     vec3 h = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x, y + 2.0 * dist * deltaY);
    --     return (a + b + c + d + e + f + g + h) / 8.0;
    -- }
    -- """
    let
        _ =
            Debug.todo
    in
    ""


toSrcRelation : String -> Expr -> String
toSrcRelation suffix e =
    -- """
    -- vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
    --     vec2 complex = """ ++ expressionToGlsl e ++ """;
    --     return complex.x > 0.0 && abs(complex.y) < """ ++ floatToGlsl epsilon ++ """ ? vec3(0.8,0.5,0.5) : vec3(0,0,0);
    -- }
    -- """
    let
        _ =
            Debug.todo
    in
    ""


mainGlsl :
    Bool
    ->
        List
            { call :
                Expression Float
                -> Expression Float
                -> Expression Float
                -> Expression Float
                -> Expression Vec3
            , color : Bool
            }
    -> List String
    -> String
mainGlsl rayDifferentials pixel2 pixel3 =
    (case ( pixel2, pixel3 ) of
        ( [], [] ) ->
            vec4Zero

        ( _ :: _, [] ) ->
            main2D pixel2

        ( [], _ :: _ ) ->
            main3D rayDifferentials pixel3

        ( _ :: _, _ :: _ ) ->
            max44
                (main2D pixel2)
                (main3D rayDifferentials pixel3)
    )
        |> assign gl_FragColor
        |> expr
        |> main_
        |> Generator.fileToGlsl


main2D :
    List
        { call :
            Expression Float
            -> Expression Float
            -> Expression Float
            -> Expression Float
            -> Expression Vec3
        , color : Bool
        }
    -> Expression Vec4
main2D pixels =
    let
        maybeColor : Int -> Bool -> Expression Vec3 -> Expression Vec3
        maybeColor i color innerCall =
            let
                h : Expression Float
                h =
                    (toFloat (i + 2) / pi)
                        |> float1
            in
            if color then
                by33 (hl2rgb11 h (float1 0.5)) innerCall

            else
                innerCall
    in
    fun0 vec4T "pixel2" <| \_ ->
    def vec2T "canvasSize" (vec211 u_canvasWidth u_canvasHeight) <| \canvasSize ->
    def vec2T "uv_centered" (subtract22 (gl_FragCoord |> dot4XY) (by12 (float1 0.5) canvasSize)) <| \uv_centered ->
    def vec2T "viewportSize" (by12 (div11 u_viewportWidth u_canvasWidth) canvasSize) <| \viewportSize ->
    def vec2T "uv" (by22 (div22 uv_centered canvasSize) viewportSize) <| \uv ->
    def vec2T "c" (add22 u_zoomCenter uv) <| \c ->
    float "x" (c |> dot2X) <| \x ->
    float "y" (c |> dot2Y) <| \y ->
    float "deltaX" (div11 u_viewportWidth u_canvasWidth) <| \deltaX ->
    float "deltaY" (div11 u_viewportWidth u_canvasHeight) <| \deltaY ->
    vec3 "px" vec3Zero <| \px ->
    decl vec3T "curr" <| \curr ->
    let
        addPixel :
            Int
            ->
                { call : Expression Float -> Expression Float -> Expression Float -> Expression Float -> Expression Vec3
                , color : Bool
                }
            -> Glsl.Statement r
            -> Glsl.Statement r
        addPixel i { call, color } cont =
            let
                inColor =
                    maybeColor i color (call deltaX deltaY x y)
            in
            expr (assign curr inColor) <| \_ ->
            expr (assign px (ternary3 (eq curr vec3Zero) px curr)) <| \_ ->
            cont

        inner : Glsl.Statement r -> Glsl.Statement r
        inner cont =
            pixels
                |> List.indexedMap addPixel
                |> List.foldr identity cont
    in
    inner <|
        float "maxDelta" (max11 deltaX deltaY) <| \maxDelta ->
        vec3
            "yax"
            (ternary3 (eq u_drawAxes one)
                (by13 (axis111 x y maxDelta) (vec3111 zero one zero))
                vec3Zero
            )
        <| \yax ->
        vec3
            "xax"
            (ternary3 (eq u_drawAxes one)
                (by13 (axis111 y x maxDelta) (vec3111 one zero zero))
                vec3Zero
            )
        <| \xax ->
        return (vec431 (max33 px (max33 xax yax)) one)


toSrc3D : Bool -> String -> Expr -> String
toSrc3D expandIntervals suffix e =
    -- """
    -- vec3 normal""" ++ suffix ++ """(vec3 p) {
    --     float x = p.x;
    --     float y = p.y;
    --     float z = p.z;
    --     vec4 gradient = """ ++ expressionToNormalGlsl e ++ """;
    --     return normalize(gradient.yzw);
    -- }
    -- vec2 interval""" ++ suffix ++ """(mat3 f, mat3 t) {
    --     vec3 mn = min(f[0], t[0]);
    --     vec3 mx = max(f[1], t[1]);
    --     vec2 x = vec2(mn.x, mx.x);
    --     vec2 y = vec2(mn.y, mx.y);
    --     vec2 z = vec2(mn.z, mx.z);
    --     return """ ++ expressionToIntervalGlsl expandIntervals e ++ """;
    -- }
    -- """
    let
        _ =
            Debug.todo
    in
    ""


main3D : Bool -> List String -> Expression Vec4
main3D rayDifferentials suffixes =
    let
        iif b t f =
            if b then
                t

            else
                f
    in
    fun0 vec4T "pixel3" <| \_ ->
    float "eye_dist" (by11 (float1 2) u_viewportWidth) <| \eye_dist ->
    vec2 "canvasSize" (vec211 u_canvasWidth u_canvasHeight) <| \canvasSize ->
    vec2 "uv_centered" (subtract22 (gl_FragCoord |> dot4XY) (by12 (float1 0.5) canvasSize)) <| \uv_centered ->
    vec2 "uv_normalized" (by12 (div11 one u_canvasHeight) uv_centered) <| \uv_normalized ->
    float "t" (add11 u_theta (float1 0.58)) <| \t ->
    float "p" (by11 (float1 -2) u_phi) <| \p ->
    vec3
        "eye"
        (by13 eye_dist
            (normalize3
                (vec3111
                    (by11 (cos1 t) (sin1 p))
                    (by11 (cos1 t) (negate1 (cos1 p)))
                    (sin1 t)
                )
            )
        )
    <| \eye ->
    vec3 "target" vec3Zero <| \target ->
    vec3 "to_target" (normalize3 (subtract33 target eye)) <| \to_target ->
    vec3 "across" (normalize3 (cross33 to_target (vec3111 zero zero one))) <| \across ->
    vec3 "up" (normalize3 (cross33 across to_target)) <| \up ->
    vec3 "canvas_center" (add33 eye to_target) <| \canvas_center ->
    vec3
        "canvas_point"
        (adds3
            [ canvas_center
            , by13 (uv_normalized |> dot2X) across
            , by13 (uv_normalized |> dot2Y) up
            ]
        )
    <| \canvas_point ->
    vec3 "ray_direction" (normalize3 (subtract33 canvas_point eye)) <| \ray_direction ->
    vec3 "diffs" (abs3 (fwidth3 ray_direction)) <| \diffs ->
    float "k" (iif rayDifferentials (float1 0.001) zero) <| \k ->
    mat3 "d"
        (mat3333
            (subtract33 ray_direction (by13 k diffs))
            (add33 ray_direction (by13 k diffs))
            vec3Zero
        )
    <| \d ->
    float "max_distance" (by11 (float1 100) eye_dist) <| \max_distance ->
    return (raytrace suffixes canvas_point d max_distance)


raytrace : List String -> a -> b -> c -> Expression Vec4
raytrace suffixes _ _ _ =
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


type VisitExpression a
    = VInteger Int
    | VFloat Float
    | VVariable String
    | VAdd a a
    | VSubtract a a
    | VNegate a
    | VBy a a
    | VDiv a a
    | VRel RelationOperation a a
    | VPower a a
    | VSquare a
    | VPowerF a Float
    | VList (List (Glsl.Expression Float))


visit :
    { expr : VisitExpression a -> a
    , function1 : Function1 -> a -> a
    , function2 : Function2 -> a -> a -> a
    , function3 : Function3 -> a -> a -> a -> a
    }
    -> ToGlslExpression
    -> a
visit ({ expr, function1, function2, function3 } as cfg) p =
    let
        expr1 f l =
            expr (f (visit cfg l))

        expr2 f l r =
            expr (f (visit cfg l) (visit cfg r))
    in
    case p of
        TGInteger i ->
            expr (VInteger i)

        TGFloat f ->
            expr (VFloat f)

        TGVariable v ->
            expr (VVariable v)

        TGNegate e ->
            expr1 VNegate e

        TGAdd l (TGNegate r) ->
            expr2 VSubtract l r

        TGAdd l r ->
            expr2 VAdd l r

        TGBy l r ->
            expr2 VBy l r

        TGDiv l r ->
            expr2 VDiv l r

        TGRel op l r ->
            expr2 (VRel op) l r

        TGPower l (TGInteger i) ->
            if i == 2 then
                expr1 VSquare l

            else
                expr1 (\le -> VPowerF le (toFloat i)) l

        TGPower l (TGFloat f) ->
            if f == 2 then
                expr1 VSquare l

            else
                expr1 (\le -> VPowerF le f) l

        TGPower l r ->
            expr2 VPower l r

        TGList es ->
            expr (VList (List.map expressionToGlsl es))

        TGApply (TGApply1 f1 l) ->
            function1 f1 (visit cfg l)

        TGApply (TGApply2 f2 l r) ->
            function2 f2 (visit cfg l) (visit cfg r)

        TGApply (TGApply3 f3 l m r) ->
            function3 f3 (visit cfg l) (visit cfg m) (visit cfg r)


expressionToGlsl : ToGlslExpression -> Expression Float
expressionToGlsl =
    visit
        { expr =
            \expr ->
                case expr of
                    VInteger i ->
                        float1 (toFloat i)

                    VFloat f ->
                        float1 f

                    VVariable v ->
                        var v

                    VAdd l r ->
                        add11 l r

                    VNegate l ->
                        negate1 l

                    VSubtract l r ->
                        subtract11 l r

                    VBy l r ->
                        by11 l r

                    VDiv l r ->
                        div11 l r

                    VRel op l r ->
                        unsafeTypecast <|
                            case op of
                                LessThan ->
                                    lt l r

                                LessThanOrEquals ->
                                    Debug.todo "branch 'LessThanOrEquals' not implemented"

                                Equals ->
                                    Debug.todo "branch 'Equals' not implemented"

                                NotEquals ->
                                    Debug.todo "branch 'NotEquals' not implemented"

                                GreaterThanOrEquals ->
                                    Debug.todo "branch 'GreaterThanOrEquals' not implemented"

                                GreaterThan ->
                                    Debug.todo "branch 'GreaterThan' not implemented"

                    VPower l r ->
                        pow11 l r

                    VList _ ->
                        Debug.todo "branch 'VList _' not implemented"

                    VSquare _ ->
                        Debug.todo "branch 'VSquare _' not implemented"

                    VPowerF _ _ ->
                        Debug.todo "branch 'VPowerF _ _' not implemented"
        , function1 =
            \f ->
                case f of
                    PSin ->
                        sin1

                    PCos ->
                        Debug.todo "branch 'PCos' not implemented"

                    PTan ->
                        Debug.todo "branch 'PTan' not implemented"

                    PAsin ->
                        Debug.todo "branch 'PAsin' not implemented"

                    PAcos ->
                        Debug.todo "branch 'PAcos' not implemented"

                    PAtan ->
                        Debug.todo "branch 'PAtan' not implemented"

                    PSinh ->
                        Debug.todo "branch 'PSinh' not implemented"

                    PCosh ->
                        Debug.todo "branch 'PCosh' not implemented"

                    PTanh ->
                        Debug.todo "branch 'PTanh' not implemented"

                    PAbs ->
                        Debug.todo "branch 'PAbs' not implemented"

                    PSqrt ->
                        Debug.todo "branch 'PSqrt' not implemented"

                    PCbrt ->
                        Debug.todo "branch 'PCbrt' not implemented"

                    PLn ->
                        Debug.todo "branch 'PLn' not implemented"

                    PLog10 ->
                        Debug.todo "branch 'PLog10' not implemented"

                    PExp ->
                        Debug.todo "branch 'PExp' not implemented"

                    PSign ->
                        Debug.todo "branch 'PSign' not implemented"

                    PRe ->
                        Debug.todo "branch 'PRe' not implemented"

                    PIm ->
                        Debug.todo "branch 'PIm' not implemented"

                    PArg ->
                        Debug.todo "branch 'PArg' not implemented"

                    PGra ->
                        Debug.todo "branch 'PGra' not implemented"

                    PDet ->
                        Debug.todo "branch 'PDet' not implemented"

                    PRound ->
                        Debug.todo "branch 'PRound' not implemented"

                    PFloor ->
                        Debug.todo "branch 'PFloor' not implemented"

                    PCeil ->
                        Debug.todo "branch 'PCeil' not implemented"
        , function2 =
            \f ->
                case f of
                    PAtan2 ->
                        Debug.todo "branch 'PAtan2' not implemented"

                    PMax ->
                        Debug.todo "branch 'PMax' not implemented"

                    PMod ->
                        Debug.todo "branch 'PMod' not implemented"

                    PMbrot ->
                        Debug.todo "branch 'PMbrot' not implemented"

                    PMin ->
                        Debug.todo "branch 'PMin' not implemented"
        , function3 =
            \f ->
                case f of
                    PPiecewise ->
                        Debug.todo "branch 'PPiecewise' not implemented"
        }
