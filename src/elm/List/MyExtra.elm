module List.MyExtra exposing (unzip3)


unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 =
    List.foldr (\( x, y, z ) ( xs, ys, zs ) -> ( x :: xs, y :: ys, z :: zs )) ( [], [], [] )
