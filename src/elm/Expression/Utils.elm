module Expression.Utils exposing (a, abs_, asin_, atan2_, b, by, c, complex, cos_, cosh_, d, dd, determinant, div, double, e, f, g, genericDeterminant, gra_, i, icomplex, ii, int, ipow, ln_, minus, n, negate_, one, plus, pow, sin_, sinh_, sqrt_, square, squash, triple, two, vector, x, y, z, zero)

import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), UnaryOperation(..), visit)



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


i : Expression
i =
    Variable "i"


n : Expression
n =
    Variable "n"


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


int : Int -> Expression
int =
    Integer


vector : List Expression -> Expression
vector =
    Expression.List << List.map (\v -> Expression.List [ v ])


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

                                s ->
                                    [ s ]
                    in
                    Just <| plus <| List.concatMap extract (ol :: or :: oo)

                AssociativeOperation Multiplication ol or oo ->
                    let
                        extract el =
                            case squash el of
                                AssociativeOperation Multiplication il ir io ->
                                    List.concatMap extract <| il :: ir :: io

                                s ->
                                    [ s ]
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


determinant : Expression -> Expression
determinant expr =
    case expr of
        List ls ->
            let
                rows =
                    ls
                        |> List.filterMap
                            (\r ->
                                case r of
                                    List cs ->
                                        Just cs

                                    _ ->
                                        Nothing
                            )

                cols =
                    rows
                        |> List.map List.length
                        |> (\lens ->
                                if List.minimum lens == List.maximum lens then
                                    List.minimum lens

                                else
                                    Nothing
                           )
                        |> Maybe.withDefault -1
            in
            if List.length rows < List.length ls then
                Apply (KnownFunction Det) [ expr ]

            else if List.length rows /= cols then
                Apply (KnownFunction Det) [ expr ]

            else
                genericDeterminant { add = plus, negate = negate_, multiply = by } rows
                    |> Maybe.withDefault (Apply (KnownFunction Det) [ expr ])

        _ ->
            Apply (KnownFunction Det) [ expr ]


genericDeterminant : { add : List a -> a, negate : a -> a, multiply : List a -> a } -> List (List a) -> Maybe a
genericDeterminant { add, negate, multiply } mat =
    case mat of
        [] ->
            Just <|
                add []

        [ [ single ] ] ->
            Just <|
                single

        [ [ a_, b_ ], [ c_, d_ ] ] ->
            Just <|
                add [ multiply [ a_, d_ ], negate <| multiply [ b_, c_ ] ]

        [ [ a_, b_, c_ ], [ d_, e_, f_ ], [ g_, h_, i_ ] ] ->
            Just <|
                add
                    [ multiply [ a_, e_, i_ ]
                    , negate <| multiply [ a_, f_, h_ ]
                    , negate <| multiply [ b_, d_, i_ ]
                    , multiply [ b_, f_, g_ ]
                    , multiply [ c_, d_, h_ ]
                    , negate <| multiply [ c_, e_, g_ ]
                    ]

        [ [ a_, b_, c_, d_ ], [ e_, f_, g_, h_ ], [ i_, j_, k_, l_ ], [ m_, n_, o_, p_ ] ] ->
            Just <|
                add
                    [ multiply [ a_, f_, k_, p_ ]
                    , negate <| multiply [ a_, f_, l_, o_ ]
                    , negate <| multiply [ a_, g_, j_, p_ ]
                    , multiply [ a_, g_, l_, n_ ]
                    , multiply [ a_, h_, j_, o_ ]
                    , negate <| multiply [ a_, h_, k_, n_ ]
                    , negate <| multiply [ b_, e_, k_, p_ ]
                    , multiply [ b_, e_, l_, o_ ]
                    , multiply [ b_, g_, i_, p_ ]
                    , negate <| multiply [ b_, g_, l_, m_ ]
                    , negate <| multiply [ b_, h_, i_, o_ ]
                    , multiply [ b_, h_, k_, m_ ]
                    , multiply [ c_, e_, j_, p_ ]
                    , negate <| multiply [ c_, e_, l_, n_ ]
                    , negate <| multiply [ c_, f_, i_, p_ ]
                    , multiply [ c_, f_, l_, m_ ]
                    , multiply [ c_, h_, i_, n_ ]
                    , negate <| multiply [ c_, h_, j_, m_ ]
                    , negate <| multiply [ d_, e_, j_, o_ ]
                    , multiply [ d_, e_, k_, n_ ]
                    , multiply [ d_, f_, i_, o_ ]
                    , negate <| multiply [ d_, f_, k_, m_ ]
                    , negate <| multiply [ d_, g_, i_, n_ ]
                    , multiply [ d_, g_, j_, m_ ]
                    ]

        _ ->
            Nothing
