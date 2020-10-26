module Expression exposing
    ( BinaryOperation(..)
    , Expression(..)
    , by
    , div
    , double
    , equals
    , int
    , ipow
    , minus
    , negate
    , one
    , partialSubstitute
    , plus
    , pow
    , sqroot
    , square
    , squash
    , triple
    , two
    , zero
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


int : Int -> Expression
int =
    Integer


associativeOperation : AssociativeOperation -> Expression -> List Expression -> Expression
associativeOperation op default xs =
    case xs of
        [] ->
            default

        [ y ] ->
            y

        y :: z :: zs ->
            AssociativeOperation op y z zs


plus : List Expression -> Expression
plus =
    associativeOperation Addition (Integer 0)


minus : Expression -> Expression -> Expression
minus x y =
    plus [ x, negate y ]


negate : Expression -> Expression
negate =
    UnaryOperation Negate


by : List Expression -> Expression
by =
    associativeOperation Multiplication (Integer 1)


div : Expression -> Expression -> Expression
div x y =
    BinaryOperation Division x y


pow : Expression -> Expression -> Expression
pow x y =
    BinaryOperation Power x y


double : Expression -> Expression
double x =
    by [ Integer 2, x ]


triple : Expression -> Expression
triple x =
    by [ Integer 3, x ]


ipow : Expression -> Int -> Expression
ipow x y =
    pow x <| Integer y


square : Expression -> Expression
square x =
    ipow x 2


sqroot : Expression -> Expression
sqroot x =
    by [ Variable "sqrt", x ]


zero : Expression
zero =
    Integer 0


one : Expression
one =
    Integer 1


two : Expression
two =
    Integer 2


squash : Expression -> Expression
squash expr =
    case expr of
        AssociativeOperation oop ol or oo ->
            case squash ol of
                (AssociativeOperation iop il ir io) as sl ->
                    if oop == iop then
                        squash <| AssociativeOperation oop il ir (io ++ or :: oo)

                    else
                        AssociativeOperation oop sl (squash or) (List.map squash oo)

                sl ->
                    AssociativeOperation oop sl (squash or) (List.map squash oo)

        UnaryOperation uop e ->
            UnaryOperation uop <| squash e

        Replace vars e ->
            Replace (Dict.map (\_ -> squash) vars) <| squash e

        List es ->
            List <| List.map squash es

        BinaryOperation bop l r ->
            BinaryOperation bop (squash l) (squash r)

        _ ->
            expr
