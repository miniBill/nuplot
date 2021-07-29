module ExpressionFuzzer exposing (expressionFuzzer)

import Expression exposing (BinaryOperation(..), Expression(..), UnaryOperation(..))
import Fuzz exposing (Fuzzer)


expressionFuzzer : Int -> Fuzzer Expression
expressionFuzzer maxDepth =
    let
        plain =
            [ ( 3, Fuzz.map Integer Fuzz.int )
            , ( 3, Fuzz.map Float Fuzz.float )
            , ( 4
              , Fuzz.map Variable <|
                    Fuzz.frequency
                        [ ( 10, Fuzz.constant "x" )
                        , ( 10, Fuzz.constant "y" )
                        , ( 10, Fuzz.constant "i" )
                        , ( 10, Fuzz.constant "e" )
                        , ( 10, Fuzz.constant "pi" )
                        , ( 1, Fuzz.string )
                        ]
              )
            ]
    in
    Fuzz.frequency <|
        plain
            ++ (if maxDepth > 0 then
                    let
                        child =
                            expressionFuzzer (maxDepth - 1)
                    in
                    [ ( 2, Fuzz.map (UnaryOperation Negate) child )
                    , ( 2, Fuzz.map3 BinaryOperation binaryOperationFuzzer child child )

                    --RelationOperation
                    --AssociativeOperation
                    --Apply
                    --Replace
                    --List
                    ]

                else
                    []
               )


binaryOperationFuzzer : Fuzzer BinaryOperation
binaryOperationFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Division
        , Fuzz.constant Power
        ]
