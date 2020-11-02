module List.MyExtra exposing (mapAccuml1, mapAccumr1)

import List.Extra as List


mapAccuml1 : (a -> a -> b) -> List a -> List b
mapAccuml1 f =
    List.foldl
        (\e ( o, a ) ->
            case o of
                Nothing ->
                    ( Just e, a )

                Just old ->
                    ( Just e, f old e :: a )
        )
        ( Nothing, [] )
        >> Tuple.second


mapAccumr1 : (a -> a -> b) -> List a -> List b
mapAccumr1 f =
    List.foldr
        (\e ( o, a ) ->
            case o of
                Nothing ->
                    ( Just e, a )

                Just old ->
                    ( Just e, f old e :: a )
        )
        ( Nothing, [] )
        >> Tuple.second
