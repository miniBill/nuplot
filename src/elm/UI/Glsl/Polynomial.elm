module UI.Glsl.Polynomial exposing (asPolynomial, getDegree, getSolutions)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), RelationOperation(..), UnaryOperation(..))
import Expression.Polynomial exposing (Exponents)
import Expression.Utils exposing (minus, zero)
import Maybe.Extra as Maybe
import UI.Glsl.Code exposing (threshold)


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
            asPolynomial vars (minus l r)

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


getSolutions : Dict Int Expression -> Maybe ( Int, List ( String, String ) )
getSolutions poly =
    let
        deg =
            poly
                |> Dict.keys
                |> List.maximum
                |> Maybe.withDefault 0

        get d =
            Dict.get d poly
                |> Maybe.withDefault zero
                |> UI.Glsl.Code.expressionToGlsl

        tFrom v =
            ( "t", v ++ ".x < 0.0 || abs(" ++ v ++ ".y) > " ++ threshold ++ " ? t : min(" ++ v ++ ".x, t)" )

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
                    , ( "x", "div(-b, a)" )
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
                    , ( "delta", "by(b,b) - 4.0 * by(a,c)" )
                    , ( "sqdelta", "csqrt(delta)" )
                    , ( "den", "2.0 * a" )
                    , ( "pos", "div(- b + sqdelta, den)" )
                    , tFrom "pos"
                    , ( "neg", "div(- b - sqdelta, den)" )
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
                   , ( "deltaZero", "by(b,b) - 3.0 * by(a,c)" )
                   , ( "deltaOne", "2.0 * by(b,b,b) - 9.0 * by(a,b,c) + 27.0 * by(a,a, dd)" )
                   , ( "inner", "csqrt(by(deltaOne,deltaOne) - 4.0 * by(deltaZero,deltaZero,deltaZero))" )
                   , ( "CargPlus", "deltaOne + inner" )
                   , ( "CargMinus", "deltaOne - inner" )
                   , ( "Carg", "length(CargMinus) > length(CargPlus) ? CargMinus : CargPlus" )
                   , ( "C", "ccbrt(0.5 * Carg)" )
                   , ( "coeff", "-div(vec2(1,0), 3.0 * a)" )
                   , ( "xi", "0.5 * vec2(-1,sqrt(3.0))" )
                   , ( "x", "by(coeff, b + C + div(deltaZero, C))" )
                   , tFrom "x"
                   , ( "C", "by(xi,C)" )
                   , ( "x", "by(coeff, b + C + div(deltaZero, C))" )
                   , tFrom "x"
                   , ( "C", "by(xi,C)" )
                   , ( "x", "by(coeff, b + C + div(deltaZero, C))" )
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
