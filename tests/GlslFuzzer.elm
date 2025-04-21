module GlslFuzzer exposing (suite)

import Complex exposing (Complex(..))
import Dict
import Expect exposing (Expectation)
import Glsl exposing (Expression, Vec2, float1)
import Glsl.Eval exposing (Context, Error, Value)
import Glsl.Functions exposing (cexp2, vec211)
import Glsl.Generator exposing (one, zero)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Glsl"
        [ test1 "cexp"
            cexp2
            [ ( zeroC, oneC )
            , ( oneC, real e )
            , ( real 2, real <| e * e )
            , ( real -1, real <| 1 / e )
            , ( complex Complex.i, complex <| Complex.exp Complex.i )
            ]
        ]


complex : Complex -> Expression Vec2
complex (Complex r i) =
    vec211 (float1 r) (float1 i)


real : Float -> Expression Vec2
real r =
    vec211 (float1 r) zero


oneC : Expression Vec2
oneC =
    vec211 one zero


zeroC : Expression Vec2
zeroC =
    vec211 zero zero


test1 :
    String
    -> (Expression Vec2 -> Expression Vec2)
    -> List ( Expression Vec2, Expression Vec2 )
    -> Test
test1 name function cases =
    cases
        |> List.map
            (\( input, output ) ->
                Test.test (Debug.toString input) <| \_ ->
                check
                    (Glsl.Eval.value Dict.empty <| function input)
                    (Glsl.Eval.value Dict.empty output)
            )
        |> Test.describe name


check : Result Error ( Context, Value ) -> Result Error ( Context, Value ) -> Expectation
check actual expected =
    Expect.equal expected actual
