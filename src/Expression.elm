module Expression exposing (BinaryOperation(..), Expression(..), by, div, double, equals, int, ipow, minus, plus, pow, sqroot, square, triple)


type Expression
    = BinaryOperation BinaryOperation Expression Expression (List Expression)
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
        ( BinaryOperation lop ll lr lo, BinaryOperation rop rl rr ro ) ->
            lop
                == rop
                && equals ll rl
                && equals lr rr
                && List.length lo
                == List.length ro
                && List.all identity (List.map2 equals lo ro)

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


int: Int->Expression
int =
    Integer


operation default op xs =
    case xs of
        [] ->
            default

        [ y ] ->
            y

        y :: z :: zs ->
            BinaryOperation op y z zs


plus =
    operation (Integer 0) Addition


minus x =
    by [ Integer -1, x ]


by =
    operation (Integer 1) Multiplication


div =
    BinaryOperation Division


pow =
    BinaryOperation Power


double x =
    by [ Integer 2, x ]


triple x =
    by [ Integer 3, x ]


ipow x y =
    pow x <| Integer y


square x =
    ipow x 2


sqroot x =
    by [ Variable "sqrt", x ]
