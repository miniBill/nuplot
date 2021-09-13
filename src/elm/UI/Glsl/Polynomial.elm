module UI.Glsl.Polynomial exposing (asPolynomial, getDegree, getSolutions, glslFromPolySolutions, glslFromSolutions)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), RelationOperation(..), UnaryOperation(..))
import Expression.Polynomial exposing (Exponents)
import Expression.Utils
import Maybe.Extra as Maybe
import UI.Glsl.Code exposing (threshold)
import UI.Glsl.Generator as Generator exposing (Expression1, ExpressionX, FunDecl, Mat3, Vec3, add, ands, arr, assign, boolT, by, byF, def, dotted1, expr, float, floatT, fun4, gt, int, lt, mat3T, mix, out, return, unknown, vec3T, zero)


getDegree : Dict (List ( a, number )) b -> number
getDegree poly =
    poly
        |> Dict.toList
        |> List.map (Tuple.first >> List.map Tuple.second >> List.sum)
        |> List.maximum
        |> Maybe.withDefault 0


asPolynomial : List String -> Expression -> Maybe (Dict Exponents Float)
asPolynomial vars e =
    case e of
        RelationOperation Equals l r ->
            asPolynomial vars (Expression.Utils.minus l r)

        _ ->
            e
                |> Expression.Polynomial.asPolynomial vars
                |> Maybe.map Dict.toList
                |> Maybe.andThen (Maybe.traverse (\( k, v ) -> Maybe.map (Tuple.pair k) (toFloat v)))
                |> Maybe.map Dict.fromList


toFloat : Expression -> Maybe Float
toFloat e =
    case e of
        Integer i ->
            Just <| Basics.toFloat i

        Float f ->
            Just <| f

        AssociativeOperation Addition l m r ->
            (l :: m :: r)
                |> Maybe.traverse toFloat
                |> Maybe.map List.sum

        AssociativeOperation Multiplication l m r ->
            (l :: m :: r)
                |> Maybe.traverse toFloat
                |> Maybe.map List.product

        UnaryOperation Negate c ->
            Maybe.map negate <| toFloat c

        BinaryOperation Division n d ->
            Maybe.map2 (/) (toFloat n) (toFloat d)

        BinaryOperation Power b ex ->
            Maybe.map2 (^) (toFloat b) (toFloat ex)

        _ ->
            Nothing


getSolutions : ExpressionX x Float -> Dict Int Expression -> Maybe ( Int, List ( String, String ) )
getSolutions maxDistance poly =
    let
        deg =
            poly
                |> Dict.keys
                |> List.maximum
                |> Maybe.withDefault 0

        get d =
            Dict.get d poly
                |> Maybe.withDefault Expression.Utils.zero
                |> UI.Glsl.Code.expressionToGlsl
                    [ ( "dx", dotted1 <| unknown "dx" )
                    , ( "dy", dotted1 <| unknown "dy" )
                    , ( "dz", dotted1 <| unknown "dz" )
                    , ( "o.x", dotted1 <| unknown "o.x" )
                    , ( "o.y", dotted1 <| unknown "o.y" )
                    , ( "o.z", dotted1 <| unknown "o.z" )
                    ]
                |> .base
                |> Generator.expressionToGlsl

        tFrom v =
            ( "t", v ++ ".x < 0.0 || abs(" ++ v ++ ".y) > " ++ Generator.expressionToGlsl (threshold maxDistance).base ++ " ? t : min(" ++ v ++ ".x, t)" )

        sols =
            case deg of
                1 ->
                    -- ax + b = 0
                    -- x = -b / a
                    [ ( "!dx", "mix(d[0].x, d[1].x, 0.5)" )
                    , ( "!dy", "mix(d[0].y, d[1].y, 0.5)" )
                    , ( "!dz", "mix(d[0].z, d[1].z, 0.5)" )
                    , ( "a", get 1 )
                    , ( "b", get 0 )
                    , ( "x", "cdiv(-b, a)" )
                    , tFrom "x"
                    ]

                2 ->
                    -- ax² + bx + c = 0
                    [ ( "!dx", "mix(d[0].x, d[1].x, 0.5)" )
                    , ( "!dy", "mix(d[0].y, d[1].y, 0.5)" )
                    , ( "!dz", "mix(d[0].z, d[1].z, 0.5)" )
                    , ( "a", get 2 )
                    , ( "b", get 1 )
                    , ( "c", get 0 )
                    , ( "delta", "cby(b,b) - 4.0 * cby(a,c)" )
                    , ( "sqdelta", "csqrt(delta)" )
                    , ( "den", "2.0 * a" )
                    , ( "pos", "cdiv(- b + sqdelta, den)" )
                    , tFrom "pos"
                    , ( "neg", "cdiv(- b - sqdelta, den)" )
                    , tFrom "neg"
                    ]

                {- 3 ->
                   [ ( "!dx", "mix(d[0].x, d[1].x, 0.5)" )
                   , ( "!dy", "mix(d[0].y, d[1].y, 0.5)" )
                   , ( "!dz", "mix(d[0].z, d[1].z, 0.5)" )
                   , ( "a", get 3 )
                   , ( "b", get 2 )
                   , ( "c", get 1 )
                   , ( "dd", get 0 )
                   , ( "deltaZero", "cby(b,b) - 3.0 * cby(a,c)" )
                   , ( "deltaOne", "2.0 * cby(b,b,b) - 9.0 * cby(a,b,c) + 27.0 * cby(a,a, dd)" )
                   , ( "inner", "csqrt(cby(deltaOne,deltaOne) - 4.0 * cby(deltaZero,deltaZero,deltaZero))" )
                   , ( "CargPlus", "deltaOne + inner" )
                   , ( "CargMinus", "deltaOne - inner" )
                   , ( "Carg", "length(CargMinus) > length(CargPlus) ? CargMinus : CargPlus" )
                   , ( "C", "ccbrt(0.5 * Carg)" )
                   , ( "coeff", "-cdiv(vec2(1,0), 3.0 * a)" )
                   , ( "xi", "0.5 * vec2(-1,sqrt(3.0))" )
                   , ( "x", "cby(coeff, b + C + cdiv(deltaZero, C))" )
                   , tFrom "x"
                   , ( "C", "cby(xi,C)" )
                   , ( "x", "cby(coeff, b + C + cdiv(deltaZero, C))" )
                   , tFrom "x"
                   , ( "C", "cby(xi,C)" )
                   , ( "x", "cby(coeff, b + C + cdiv(deltaZero, C))" )
                   , tFrom "x"
                   ]
                -}
                _ ->
                    []
    in
    if List.isEmpty sols then
        Nothing

    else
        Just ( deg, sols )


glslFromSolutions :
    String
    -> List ( String, String )
    ->
        { funDecls : List FunDecl
        , bisect : ExpressionX xa Vec3 -> ExpressionX xb Mat3 -> ExpressionX xc Float -> ExpressionX xd Vec3 -> Expression1 Bool
        }
glslFromSolutions suffix sols =
    let
        checks =
            sols
                |> List.foldl
                    (\( k, v ) ( known, f ) ->
                        let
                            ( decl, drop ) =
                                if List.member k known then
                                    ( "", 0 )

                                else if String.startsWith "!" k then
                                    ( "float ", 1 )

                                else
                                    ( "vec2 ", 0 )

                            newLine =
                                \n -> f <| expr (dotted1 <| unknown (decl ++ String.dropLeft drop k ++ " = " ++ v ++ ";")) <| \_ -> n
                        in
                        ( k :: known, newLine )
                    )
                    ( [ "t" ], identity )
                |> Tuple.second

        ( funDecl, bisect ) =
            fun4 boolT ("bisect" ++ suffix) (vec3T "o") (mat3T "d") (floatT "max_distance") (out vec3T "found") <| \o d maxDistance found nop ->
            def floatT "t" (by maxDistance <| float 2) <| \t ->
            checks <|
                expr (assign found (add o (byF t (mix (arr d (int 0)) (arr d (int 1)) (float 0.5))))) <| \_ ->
                return (ands [ lt t maxDistance, gt t zero ]) nop
    in
    { funDecls = [ funDecl ]
    , bisect = bisect
    }


glslFromPolySolutions :
    String
    -> ExpressionX xm Float
    -> Dict Int Expression
    ->
        Maybe
            { funDecls : List FunDecl
            , bisect : ExpressionX xa Vec3 -> ExpressionX xb Mat3 -> ExpressionX xc Float -> ExpressionX xd Vec3 -> Expression1 Bool
            }
glslFromPolySolutions suffix maxDistance poly =
    getSolutions maxDistance poly |> Maybe.map (\( _, sols ) -> glslFromSolutions suffix sols)
