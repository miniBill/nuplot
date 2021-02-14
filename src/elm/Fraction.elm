module Fraction exposing (Fraction, build, by, div, divisor, fromInt, gcd, isZero, negate, plus, pow, toFloat, toPair)


type Fraction
    = Fraction Int Int


fromInt : Int -> Fraction
fromInt i =
    Fraction i 1


plus : Fraction -> Fraction -> Fraction
plus (Fraction ln ld) (Fraction rn rd) =
    build (ln * rd + rn * ld) (ld * rd)


negate : Fraction -> Fraction
negate (Fraction n d) =
    Fraction -n d


by : Fraction -> Fraction -> Fraction
by (Fraction ln ld) (Fraction rn rd) =
    build (ln * rn) (ld * rd)


div : Fraction -> Fraction -> Fraction
div (Fraction ln ld) (Fraction rn rd) =
    build (ln * rd) (ld * rn)


divisor : Fraction -> Int
divisor (Fraction _ d) =
    d


pow : Fraction -> Int -> Fraction
pow (Fraction n d) p =
    Fraction (n ^ p) (d ^ p)


build : Int -> Int -> Fraction
build n d =
    if d < 0 then
        build -n -d

    else
        let
            g =
                abs <| gcd n d
        in
        Fraction (n // g) (d // g)


toPair : Fraction -> ( Int, Int )
toPair (Fraction n d) =
    ( n, d )


toFloat : Fraction -> Float
toFloat (Fraction n d) =
    Basics.toFloat n / Basics.toFloat d


isZero : Fraction -> Bool
isZero (Fraction n _) =
    n == 0


gcd : Int -> Int -> Int
gcd ll rr =
    if ll < 0 then
        -(gcd -ll rr)

    else if ll < rr then
        gcd rr ll

    else if rr == 0 then
        ll

    else
        gcd rr (modBy rr ll)
