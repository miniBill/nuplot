module List.MyExtra exposing (LeftOrRight(..), categorize, groupOneWith, unzip3)


unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 =
    List.foldr (\( x, y, z ) ( xs, ys, zs ) -> ( x :: xs, y :: ys, z :: zs )) ( [], [], [] )


groupOneWith : (a -> a -> Maybe a) -> List a -> List a
groupOneWith step list =
    list
        |> List.foldr
            (\e ( last, acc ) ->
                case last of
                    Nothing ->
                        ( Just e, acc )

                    Just lst ->
                        case step e lst of
                            Just c ->
                                ( Nothing, c :: acc )

                            Nothing ->
                                ( Just e, lst :: acc )
            )
            ( Nothing, [] )
        |> (\( last, acc ) ->
                case last of
                    Just l ->
                        l :: acc

                    Nothing ->
                        acc
           )


type LeftOrRight a b
    = Left a
    | Right b


categorize : (x -> LeftOrRight a b) -> List x -> ( List a, List b )
categorize f =
    List.foldr
        (\e ( la, ra ) ->
            case f e of
                Left l ->
                    ( l :: la, ra )

                Right r ->
                    ( la, r :: ra )
        )
        ( [], [] )
