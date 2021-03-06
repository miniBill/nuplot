module UI.Glsl.Polynomial exposing (asPolynomial, getDegree)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), UnaryOperation(..))
import Expression.Polynomial exposing (Exponents)
import List
import Maybe.Extra as Maybe


getDegree : Dict (List ( a, number )) b -> number
getDegree poly =
    poly
        |> Dict.toList
        |> List.map (Tuple.first >> List.map Tuple.second >> List.sum)
        |> List.maximum
        |> Maybe.withDefault 0


asPolynomial : List String -> Expression -> Maybe (Dict Exponents Float)
asPolynomial vars e =
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
