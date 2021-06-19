module NumericRangeTest exposing (suite)

import Expect
import Expression exposing (Expression(..), RelationOperation(..))
import Expression.NumericRange as NumericRange exposing (NumericRange(..))
import Expression.Parser exposing (Problem(..))
import Expression.Utils exposing (square, x)
import Test exposing (Test, describe)


suite : Test
suite =
    describe "The NumericRange module"
        [ Test.test "Real² = Real" <|
            \() ->
                square x
                    |> NumericRange.get
                    |> Expect.equal Nonnegative
        ]
