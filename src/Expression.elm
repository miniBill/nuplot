module Expression exposing
    ( AssociativeOperation(..)
    , BinaryOperation(..)
    , Expression(..)
    , UnaryOperation(..)
    , equals
    , partialSubstitute
    )

import Dict exposing (Dict)


type Expression
    = UnaryOperation UnaryOperation Expression
    | BinaryOperation BinaryOperation Expression Expression
    | AssociativeOperation AssociativeOperation Expression Expression (List Expression)
    | Variable String
    | Integer Int
    | Float Float
    | Replace (Dict String Expression) Expression
    | List (List Expression)


type UnaryOperation
    = Negate


type BinaryOperation
    = Division
    | Power


type AssociativeOperation
    = Addition
    | Multiplication


partialSubstitute : Expression -> a
partialSubstitute _ =
    Debug.todo "partialSubstitute"


equals : Expression -> Expression -> Bool
equals l r =
    case ( l, r ) of
        ( UnaryOperation lop le, UnaryOperation rop re ) ->
            (lop == rop) && equals le re

        ( BinaryOperation lop ll lr, BinaryOperation rop rl rr ) ->
            (lop == rop)
                && equals ll rl
                && equals lr rr

        ( AssociativeOperation lop ll lr lo, AssociativeOperation rop rl rr ro ) ->
            (lop == rop)
                && equals ll rl
                && equals lr rr
                && listEquals lo ro

        ( Variable lv, Variable rv ) ->
            lv == rv

        ( Integer li, Integer ri ) ->
            li == ri

        ( Replace ls le, Replace rs re ) ->
            (Dict.size ls == Dict.size rs)
                && equals le re
                && List.all identity
                    (List.map2
                        (\( lc, ln ) ( rc, rn ) -> lc == rc && equals ln rn)
                        (Dict.toList ls)
                        (Dict.toList rs)
                    )

        ( List ls, List rs ) ->
            listEquals ls rs

        _ ->
            False


listEquals : List Expression -> List Expression -> Bool
listEquals ls rs =
    (List.length ls == List.length rs)
        && List.all identity (List.map2 equals ls rs)
