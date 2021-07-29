module TrieTest exposing (suite)

import Expect
import Fuzz
import Test exposing (Test, describe)
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
