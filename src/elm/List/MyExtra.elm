module List.MyExtra exposing (groupOneWith, unzip3)


unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 =
    List.foldr (\( x, y, z ) ( xs, ys, zs ) -> ( x :: xs, y :: ys, z :: zs )) ( [], [], [] )


groupOneWith : (a -> a -> Maybe a) -> List a -> List a
groupOneWith step list =
    case List.reverse list of
        [] ->
            []

        slast :: sinit ->
            List.foldl
                (\e ( last, acc, done ) ->
                    if done then
                        ( e, last :: acc, True )

                    else
                        case step e last of
                            Just c ->
                                ( c, acc, True )

                            Nothing ->
                                ( e, last :: acc, False )
                )
                ( slast, [], False )
                sinit
                |> (\( last, acc, _ ) -> last :: acc)
