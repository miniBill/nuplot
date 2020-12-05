module RangeTest exposing (suite)

import Complex exposing (Complex)
import Dict
import Expect
import Expression exposing (BinaryOperation(..), Expression(..))
import Expression.NumericRange exposing (NumericRange(..))
import ExpressionFuzzer exposing (expressionFuzzer)
import Fuzz
import Test exposing (Test, describe, fuzz3)


suite : Test
suite =
    describe "Ranges are correctly calculated"
        [ fuzz3 (expressionFuzzer 6) Fuzz.float Fuzz.float "random expression" <|
            \e x y ->
                let
                    expected =
                        Expression.NumericRange.get e

                    actual =
                        Expression.value
                            (Dict.fromList
                                [ ( "x", Complex.fromReal x )
                                , ( "y", Complex.fromReal y )
                                ]
                            )
                            e
                in
                inRange expected actual
                    |> Expect.true
                        (String.concat
                            [ "The value of "
                            , Expression.toString e
                            , ", for x = "
                            , String.fromFloat x
                            , " and y = "
                            , String.fromFloat y
                            , " was "
                            , Debug.toString actual
                            , " which is not in the expected range of "
                            , Debug.toString expected
                            ]
                        )
        ]


epsilon : Float
epsilon =
    1.0 ^ -10


inRange : NumericRange -> Complex -> Bool
inRange range (Complex.Complex r i) =
    if isNaN r || isInfinite r || isNaN i || isInfinite i then
        True

    else if range /= Complex && abs i > epsilon then
        False

    else
        case range of
            Positive ->
                r > -epsilon

            Nonnegative ->
                r >= -epsilon

            Zero ->
                abs r < epsilon

            Nonpositive ->
                r <= epsilon

            Negative ->
                r < epsilon

            _ ->
                True
