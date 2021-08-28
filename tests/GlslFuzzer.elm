module GlslFuzzer exposing (suite)

import Complex exposing (Complex(..))
import Dict
import Expect exposing (Expectation)
import Test exposing (Test)
import UI.Glsl.Code exposing (cexpFunction)
import UI.Glsl.Generator as Generator exposing (Context, ErrorValue, Expression2, GlslValue, Statement, Vec2, float, one, vec2, zero)


suite : Test
suite =
    Test.describe "Glsl"
        [ test1 "cexp"
            cexpFunction
            [ ( zeroC, oneC )
            , ( oneC, real e )
            , ( real 2, real <| e * e )
            , ( real -1, real <| 1 / e )
            , ( complex Complex.i, complex <| Complex.exp Complex.i )
            ]
        ]


complex : Complex -> Expression2
complex (Complex r i) =
    vec2 (float r) (float i)


real : Float -> Expression2
real r =
    vec2 (float r) zero


oneC : Expression2
oneC =
    vec2 one zero


zeroC : Expression2
zeroC =
    vec2 zero zero


test1 :
    String
    -> (Expression2 -> Statement Vec2)
    -> List ( Expression2, Expression2 )
    -> Test
test1 name function cases =
    cases
        |> List.map
            (\( input, output ) ->
                Test.test (Debug.toString input) <|
                    \_ ->
                        check
                            (Generator.interpret Dict.empty <| function input)
                            (Generator.value Dict.empty output.base)
            )
        |> Test.describe name


check : Result ErrorValue ( Context, GlslValue ) -> Result ErrorValue ( Context, GlslValue ) -> Expectation
check actual expected =
    Expect.equal expected actual
