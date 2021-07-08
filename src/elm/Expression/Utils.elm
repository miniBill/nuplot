module Expression.Utils exposing (a, abs_, asin_, atan2_, b, by, byShort, c, cbrt, complex, cos_, cosh_, d, dd, det, determinant, div, divShort, double, e, exp, f, factor, g, gra_, h, i, icomplex, ii, im, ipow, ipowShort, isOne, isZero, j, k, l, ln_, log10_, m, minus, minusOne, n, negateShort, negate_, o, one, p, plus, plusShort, pow, q, r, re, runForLoop, s, sign, sin_, sinh_, sqrt_, square, squash, t, tan_, triple, two, u, v, vector, w, x, y, z, zero)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), UnaryOperation(..), genericAsSquareMatrix, genericDeterminant, visit)
import List.Extra as List



-- Numbers


zero : Expression
zero =
    Integer 0


one : Expression
one =
    Integer 1


two : Expression
two =
    Integer 2


minusOne : Expression
minusOne =
    Integer -1



-- Variables


a : Expression
a =
    Variable "a"


b : Expression
b =
    Variable "b"


c : Expression
c =
    Variable "c"


d : Expression
d =
    Variable "d"


e : Expression
e =
    Variable "e"


f : Expression
f =
    Variable "f"


g : Expression
g =
    Variable "g"


h : Expression
h =
    Variable "h"


i : Expression
i =
    Variable "i"


j : Expression
j =
    Variable "j"


k : Expression
k =
    Variable "k"


l : Expression
l =
    Variable "l"


m : Expression
m =
    Variable "m"


n : Expression
n =
    Variable "n"


o : Expression
o =
    Variable "o"


p : Expression
p =
    Variable "p"


q : Expression
q =
    Variable "q"


r : Expression
r =
    Variable "r"


s : Expression
s =
    Variable "s"


t : Expression
t =
    Variable "t"


u : Expression
u =
    Variable "u"


v : Expression
v =
    Variable "v"


w : Expression
w =
    Variable "w"


x : Expression
x =
    Variable "x"


y : Expression
y =
    Variable "y"


z : Expression
z =
    Variable "z"



-- Functions


unaryFunc : KnownFunction -> Expression -> Expression
unaryFunc name arg =
    Apply (KnownFunction name) [ arg ]


binaryFunc : KnownFunction -> Expression -> Expression -> Expression
binaryFunc name arg1 arg2 =
    Apply (KnownFunction name) [ arg1, arg2 ]


quaternaryFunc : KnownFunction -> Expression -> Expression -> Expression -> Expression -> Expression
quaternaryFunc name arg1 arg2 arg3 arg4 =
    Apply (KnownFunction name) [ arg1, arg2, arg3, arg4 ]


plus : List Expression -> Expression
plus =
    associativeOperation Addition zero


negate_ : Expression -> Expression
negate_ =
    UnaryOperation Negate


minus : Expression -> Expression -> Expression
minus x_ y_ =
    plus [ x_, negate_ y_ ]


by : List Expression -> Expression
by =
    associativeOperation Multiplication one


det : Expression -> Expression
det =
    unaryFunc Det


div : Expression -> Expression -> Expression
div =
    BinaryOperation Division


pow : Expression -> Expression -> Expression
pow =
    BinaryOperation Power


ipow : Expression -> Int -> Expression
ipow base exponent =
    pow base <| Integer exponent


cbrt : Expression -> Expression
cbrt =
    unaryFunc (Root 3)


sqrt_ : Expression -> Expression
sqrt_ =
    unaryFunc (Root 2)


re : Expression -> Expression
re =
    unaryFunc Re


im : Expression -> Expression
im =
    unaryFunc Im


gra_ : Expression -> Expression
gra_ =
    unaryFunc Gra


square : Expression -> Expression
square base =
    ipow base 2


abs_ : Expression -> Expression
abs_ =
    unaryFunc Abs


sin_ : Expression -> Expression
sin_ =
    unaryFunc Sin


cos_ : Expression -> Expression
cos_ =
    unaryFunc Cos


tan_ : Expression -> Expression
tan_ =
    unaryFunc Tan


exp : Expression -> Expression
exp =
    unaryFunc Exp


atan2_ : Expression -> Expression -> Expression
atan2_ =
    binaryFunc Atan2


sinh_ : Expression -> Expression
sinh_ =
    unaryFunc Sinh


cosh_ : Expression -> Expression
cosh_ =
    unaryFunc Cosh


asin_ : Expression -> Expression
asin_ =
    unaryFunc Asin


ln_ : Expression -> Expression
ln_ =
    unaryFunc Ln


log10_ : Expression -> Expression
log10_ =
    unaryFunc Log10


sign : Expression -> Expression
sign =
    unaryFunc Sign


double : Expression -> Expression
double num =
    by [ two, num ]


triple : Expression -> Expression
triple num =
    by [ Integer 3, num ]


dd : Expression -> Expression -> Expression
dd =
    binaryFunc Dd


ii : Expression -> Expression -> Expression -> Expression -> Expression
ii =
    quaternaryFunc Ii


vector : List Expression -> Expression
vector =
    Expression.List << List.map (\el -> Expression.List [ el ])


associativeOperation : AssociativeOperation -> Expression -> List Expression -> Expression
associativeOperation op default xs =
    case xs of
        [] ->
            default

        [ single ] ->
            single

        y_ :: z_ :: zs ->
            AssociativeOperation op y_ z_ zs


squash : Expression -> Expression
squash =
    visit <|
        \expr ->
            case expr of
                AssociativeOperation Addition ol or oo ->
                    let
                        extract el =
                            case squash el of
                                AssociativeOperation Addition il ir io ->
                                    List.concatMap extract <| il :: ir :: io

                                ex ->
                                    [ ex ]
                    in
                    Just <| plus <| List.concatMap extract (ol :: or :: oo)

                AssociativeOperation Multiplication ol or oo ->
                    let
                        extract el =
                            case squash el of
                                AssociativeOperation Multiplication il ir io ->
                                    List.concatMap extract <| il :: ir :: io

                                ex ->
                                    [ ex ]
                    in
                    Just <| by <| List.concatMap extract (ol :: or :: oo)

                _ ->
                    Nothing


complex : Expression -> Expression -> Expression
complex real immaginary =
    case ( real, immaginary ) of
        ( _, Integer 0 ) ->
            real

        ( Integer 0, Integer 1 ) ->
            i

        ( Integer 0, _ ) ->
            by [ immaginary, i ]

        ( _, Integer 1 ) ->
            plus [ real, i ]

        _ ->
            plus [ real, by [ immaginary, i ] ]


icomplex : Int -> Int -> Expression
icomplex real immaginary =
    complex (Integer real) (Integer immaginary)


asList : Expression -> Maybe (List Expression)
asList ex =
    case ex of
        List ls ->
            Just ls

        _ ->
            Nothing


asSquareMatrix : Expression -> Maybe (List (List Expression))
asSquareMatrix =
    genericAsSquareMatrix asList


determinant : Expression -> Expression
determinant expr =
    case asSquareMatrix expr of
        Just rows ->
            genericDeterminant { plus = plus, negate = negate_, by = by } rows
                |> Maybe.withDefault (Apply (KnownFunction Det) [ expr ])

        _ ->
            Apply (KnownFunction Det) [ expr ]


factor : Int -> List ( Int, Int )
factor =
    let
        plusOne key dict =
            Dict.insert key (1 + Maybe.withDefault 0 (Dict.get key dict)) dict

        go acc num =
            if num <= 1 then
                acc

            else
                case List.find (\prime -> modBy prime num == 0) smallPrimes of
                    Just prime ->
                        go (plusOne prime acc) (num // prime)

                    Nothing ->
                        let
                            prime =
                                naive 1009 num
                        in
                        go (plusOne prime acc) (num // prime)

        naive prime num =
            if modBy prime num == 0 then
                prime

            else if prime * prime >= num then
                num

            else
                naive (prime + 2) num
    in
    go Dict.empty >> Dict.toList


smallPrimes : List number
smallPrimes =
    [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997 ]


ipowShort : Expression -> Int -> Expression
ipowShort base exponent =
    case exponent of
        0 ->
            one

        1 ->
            base

        _ ->
            ipow base exponent


plusShort : List Expression -> Expression
plusShort exprs =
    exprs
        |> List.filterNot isZero
        |> plus


byShort : List Expression -> Expression
byShort exprs =
    if List.any isZero exprs then
        zero

    else
        exprs
            |> List.filterNot isOne
            |> by


divShort : Expression -> Expression -> Expression
divShort num den =
    if isZero num then
        zero

    else if isOne den then
        num

    else
        div num den


negateShort : Expression -> Expression
negateShort expr =
    case expr of
        Integer int ->
            Integer -int

        Float float ->
            Float -float

        UnaryOperation Negate child ->
            child

        _ ->
            negate_ expr


isZero : Expression -> Bool
isZero expr =
    case expr of
        Integer int ->
            int == 0

        Float float ->
            float == 0

        _ ->
            False


isOne : Expression -> Bool
isOne expr =
    case expr of
        Integer int ->
            int == 1

        Float float ->
            float == 1

        _ ->
            False


runForLoop : List Expression -> Maybe (List Expression)
runForLoop args =
    let
        run expression values =
            List.map
                (\value -> by [ expression, value ])
                values
    in
    case args of
        [ expression, Integer from, Integer to ] ->
            Just <| run expression <| List.map Integer (List.range from (to - 1))

        [ expression, List values ] ->
            Just <| run expression values

        [ expression, Integer to ] ->
            Just <| run expression <| List.map Integer (List.range 0 (to - 1))

        _ ->
            Nothing
