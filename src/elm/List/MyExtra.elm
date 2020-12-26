module List.MyExtra exposing (groupWith, unzip3)


unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 =
    List.foldr (\( x, y, z ) ( xs, ys, zs ) -> ( x :: xs, y :: ys, z :: zs )) ( [], [], [] )


groupWith : (a -> a -> Maybe a) -> List a -> List a
groupWith step list =
    case List.reverse list of
        [] ->
            []

        slast :: sinit ->
            List.foldl
                (\e ( last, acc ) ->
                    case step e last of
                        Just c ->
                            ( c, acc )

                        Nothing ->
                            ( e, last :: acc )
                )
                ( slast, [] )
                sinit
                |> (\( last, acc ) -> last :: acc)
