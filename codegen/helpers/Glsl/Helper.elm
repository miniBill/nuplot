module Glsl.Helper exposing (..)

import Set exposing (Set)


type alias Expression =
    { expr : String
    , deps : Set String
    }


call : String -> List Expression -> List String -> Expression
call name args deps =
    { expr =
        name
            ++ "("
            ++ String.join ", " (List.map .expr args)
            ++ ")"
    , deps =
        List.map .deps args
            |> List.foldr Set.union (Set.fromList deps)
    }
