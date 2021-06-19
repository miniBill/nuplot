module NumericRangeTest exposing (suite)

import Dict
import Expect
import Expression exposing (Expression(..), RelationOperation(..))
import Expression.NumericRange as NumericRange exposing (NumericRange(..))
import Expression.Parser exposing (Problem(..))
import Expression.Utils exposing (plus, sin_, square, x, y)
import Expression.Value as Value
import Test exposing (Test, describe)


suite : Test
suite =
    describe "The NumericRange module"
        [ Test.test "Real² = Nonegative" <|
            \() ->
                square x
                    |> expectValueRange Nonnegative
        , Test.test "sin(Real² + Real²) = Real" <|
            \() ->
                sin_ (plus [ square x, square y ])
                    |> expectValueRange Real
        ]


expectValueRange : NumericRange -> Expression -> Expect.Expectation
expectValueRange range expression =
    expression
        |> Value.value Dict.empty
        |> Value.toExpression
        |> NumericRange.get
        |> Expect.equal range
