module Complex exposing (Complex(..), abs, acos, arg, asin, atan, atan2, by, ceiling, cos, cosh, div, exp, floor, fromReal, i, im, ln, minus, negate, one, plus, power, re, round, sign, sin, sinh, sqrt, tan, tanh, toString, zero)


type Complex
    = Complex Float Float


zero : Complex
zero =
    Complex 0 0


one : Complex
one =
    Complex 1 0


i : Complex
i =
    Complex 0 1


fromReal : Float -> Complex
fromReal r =
    Complex r 0


plus : Complex -> Complex -> Complex
plus (Complex lr li) (Complex rr ri) =
    Complex (lr + rr) (li + ri)


negate : Complex -> Complex
negate (Complex x y) =
    Complex -x -y


minus : Complex -> Complex -> Complex
minus l r =
    plus l (negate r)


by : Complex -> Complex -> Complex
by (Complex lr li) (Complex rr ri) =
    Complex (lr * rr - li * ri) (li * rr + lr * ri)


invert : Complex -> Complex
invert (Complex x y) =
    let
        l =
            x * x + y * y
    in
    Complex (x / l) (-y / l)


div : Complex -> Complex -> Complex
div l r =
    by l (invert r)


power : Complex -> Complex -> Complex
power ((Complex br bi) as b) ((Complex zr zi) as z) =
    if bi == 0 && zi == 0 then
        fromReal (br ^ zr)

    else
        let
            ( r, t ) =
                toPolar b
        in
        exp <| by z <| Complex (logBase e r) t


abs : Complex -> Float
abs (Complex x y) =
    Basics.sqrt (x * x + y * y)


sign : Complex -> Complex
sign (Complex x y) =
    let
        sgn q =
            if q > 0 then
                1

            else if q < 0 then
                -1

            else
                0
    in
    Complex (sgn x) (sgn y)


arg : Complex -> Float
arg (Complex x y) =
    Basics.atan2 y x


toPolar : Complex -> ( Float, Float )
toPolar c =
    ( abs c, arg c )


exp : Complex -> Complex
exp (Complex x y) =
    Complex (e ^ x * Basics.cos y) (e ^ x * Basics.sin y)


ln : Complex -> Complex
ln c =
    let
        ( a, t ) =
            toPolar c
    in
    Complex (Basics.logBase Basics.e a) t


re : Complex -> Complex
re (Complex x _) =
    fromReal x


im : Complex -> Complex
im (Complex _ y) =
    fromReal y


sinh_ : Float -> Float
sinh_ x =
    (e ^ x - e ^ -x) / 2


cosh_ : Float -> Float
cosh_ x =
    (e ^ x + e ^ -x) / 2


sin : Complex -> Complex
sin (Complex x y) =
    Complex (Basics.sin x * cosh_ y) (Basics.cos x * sinh_ y)


cos : Complex -> Complex
cos (Complex x y) =
    Complex (Basics.cos x * cosh_ y) (Basics.sin x * sinh_ y)


tan : Complex -> Complex
tan c =
    div (sin c) (cos c)


sinh : Complex -> Complex
sinh x =
    negate <| (by i <| sin <| by i x)


cosh : Complex -> Complex
cosh x =
    cos <| by i x


tanh : Complex -> Complex
tanh x =
    negate <| by i <| tan <| by i x


asin : Complex -> Complex
asin x =
    by i <| ln <| minus (sqrt <| minus one <| by x x) (by i x)


acos : Complex -> Complex
acos x =
    negate <| by i <| ln <| plus x <| by i <| sqrt <| minus one <| by x x


atan : Complex -> Complex
atan x =
    negate <| by i <| tan <| by i x


atan2 : Complex -> Complex -> Complex
atan2 y x =
    fromReal <| arg <| plus x <| by i y


sqrt : Complex -> Complex
sqrt (Complex x y) =
    let
        a =
            Basics.sqrt (x * x + y * y)

        r =
            Basics.sqrt ((a + x) / 2)

        mi =
            Basics.sqrt ((a - x) / 2)

        si =
            if y >= 0 then
                mi

            else
                -mi
    in
    Complex r si


floor : Complex -> Complex
floor (Complex x y) =
    Complex (toFloat <| Basics.floor x) (toFloat <| Basics.floor y)


ceiling : Complex -> Complex
ceiling (Complex x y) =
    Complex (toFloat <| Basics.ceiling x) (toFloat <| Basics.ceiling y)


round : Complex -> Complex
round (Complex x y) =
    Complex (toFloat <| Basics.round x) (toFloat <| Basics.round y)


toString : Complex -> String
toString (Complex x y) =
    if y == 0 then
        String.fromFloat x

    else if x == 0 then
        String.fromFloat y ++ "i"

    else if y == 1 then
        String.fromFloat x ++ " + i"

    else if y == -1 then
        String.fromFloat x ++ " - i"

    else if y > 0 then
        String.fromFloat x ++ " + " ++ String.fromFloat y ++ "i"

    else
        String.fromFloat x ++ " - " ++ String.fromFloat -y ++ "i"
