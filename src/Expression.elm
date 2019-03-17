module Expression exposing (BinaryOperation(..), Expression(..), equals)


type Expression
    = BinaryOperation BinaryOperation Expression Expression
    | Variable String
    | Integer Int
    | Replace (List ( String, Expression )) Expression
    | List (List Expression)


type BinaryOperation
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Power


equals : Expression -> Expression -> Bool
equals l r =
    case ( l, r ) of
        ( BinaryOperation lop ll lr, BinaryOperation rop rl rr ) ->
            lop == rop && equals ll rl && equals lr rr

        ( Variable lv, Variable rv ) ->
            lv == rv

        ( Integer li, Integer ri ) ->
            li == ri

        ( Replace ls le, Replace rs re ) ->
            List.length ls
                == List.length rs
                && equals le re
                && List.all identity
                    (List.map2
                        (\( lc, ln ) ( rc, rn ) ->
                            lc == rc && equals ln rn
                        )
                        ls
                        rs
                    )

        ( List ls, List rs ) ->
            List.length ls
                == List.length rs
                && List.all identity
                    (List.map2
                        equals
                        ls
                        rs
                    )

        _ ->
            False
