module Expression.Utils exposing (a, abs_, asPoly, asin_, atan2_, b, by, byShort, c, complex, cos_, cosh_, d, dd, det, determinant, div, divShort, double, e, exp_, f, factor, g, gra_, h, i, icomplex, ii, im, ipow, ipowShort, isOne, isZero, j, k, l, ln_, m, minus, n, negateShort, negate_, o, one, p, plus, plusShort, pow, q, r, re, s, sin_, sinh_, sqrt_, square, squash, t, triple, two, u, v, vector, w, x, y, z, zero)

import Dict exposing (Dict)
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


sqrt_ : Expression -> Expression
sqrt_ =
    unaryFunc Sqrt


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


exp_ : Expression -> Expression
exp_ =
    unaryFunc Exp


cos_ : Expression -> Expression
cos_ =
    unaryFunc Cos


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


asPoly : String -> Expression -> Result String (Dict Int Expression)
asPoly var expr =
    case expr of
        AssociativeOperation Addition ll mm rr ->
            List.foldl
                (Result.map2 <|
                    \el acc ->
                        Dict.merge
                            Dict.insert
                            (\kk aa bb -> Dict.insert kk <| plusShort [ aa, bb ])
                            Dict.insert
                            el
                            acc
                            Dict.empty
                )
                (Ok <| Dict.empty)
                (List.map (asPoly var) (ll :: mm :: rr))

        AssociativeOperation Multiplication ll mm rr ->
            (ll :: mm :: rr)
                |> List.map (asPoly var >> Result.map Dict.toList)
                |> List.foldl
                    (Result.map2 <|
                        \el acc ->
                            acc
                                |> List.concatMap (\acc_e -> el |> List.map (Tuple.pair acc_e))
                                |> List.map (\( ( dl, cl ), ( dr, cr ) ) -> ( dl + dr, byShort [ cl, cr ] ))
                    )
                    (Ok [ ( 0, one ) ])
                |> Result.map
                    (\ls ->
                        ls
                            |> List.gatherEqualsBy Tuple.first
                            |> List.map (\( ( deg, ff ), ts ) -> ( deg, plusShort <| ff :: List.map Tuple.second ts ))
                            |> Dict.fromList
                    )

        Integer _ ->
            Ok <| Dict.singleton 0 expr

        Float _ ->
            Ok <| Dict.singleton 0 expr

        Variable evar ->
            Ok <|
                if var == evar then
                    Dict.singleton 1 one

                else
                    Dict.singleton 0 expr

        UnaryOperation Negate inner ->
            inner
                |> asPoly var
                |> Result.map (Dict.map (always negateShort))

        BinaryOperation Division num (Integer _) ->
            num
                |> asPoly var
                |> Result.map (Dict.map (\_ coeff -> divShort coeff d))

        BinaryOperation Power base (Integer ex) ->
            if ex < 0 then
                Err ("Cannot extract coefficients from " ++ Expression.toString expr)

            else if ex == 0 then
                Ok <| Dict.singleton 0 one

            else
                asPoly var <| byShort [ base, ipowShort base (ex - 1) ]

        _ ->
            Err ("Cannot extract coefficients from " ++ Expression.toString expr)


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
