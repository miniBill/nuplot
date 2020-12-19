module TrieTest exposing (suite)

import Dict
import Expect
import Expression exposing (Expression(..), RelationOperation(..))
import Expression.Parser as Parser exposing (Problem(..))
import Expression.Utils exposing (a, abs_, asin_, atan2_, b, by, c, complex, cos_, cosh_, d, dd, div, double, exp_, f, g, gra_, i, icomplex, ii, int, ipow, ln_, minus, n, negate_, one, plus, pow, sin_, sinh_, sqrt_, square, triple, two, vector, x, y, z)
import Fuzz
import Parser
import Test exposing (Test, describe, test)
import Trie


suite : Test
suite =
    describe "The Trie module"
        [ Test.fuzz Fuzz.string "insert >> get === Just" <|
            \s ->
                Trie.empty
                    |> Trie.insert s 0
                    |> Trie.get s
                    |> Expect.equal (Just 0)
        ]


tests : List ( String, Expression, String )
tests =
    let
        byself var =
            by [ var, var ]

        straight s v =
            ( s, v, s )
    in
    []
