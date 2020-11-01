module Complex exposing (Complex(..), abs, by, cos, cosh, div, fromReal, invert, minus, negate, one, plus, power, real, sin, sinh, sqrt, zero)


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


real : Complex -> Float
real (Complex r _) =
    r


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
power b z =
    let
        ( r, t ) =
            toPolar b
    in
    exp <| by z <| Complex (logBase e r) t


abs : Complex -> Float
abs (Complex x y) =
    Basics.sqrt (x * x + y * y)


theta : Complex -> Float
theta (Complex x y) =
    atan2 y x


toPolar : Complex -> ( Float, Float )
toPolar c =
    ( abs c, theta c )


exp : Complex -> Complex
exp (Complex x y) =
    Complex (e ^ x * Basics.cos y) (e ^ x * Basics.sin y)


sinh_ : Float -> Float
sinh_ x =
    (e ^ x - e ^ -x) / 2


cosh_ : Float -> Float
cosh_ x =
    (e ^ x + e ^ -x) / 2


sinh : Complex -> Complex
sinh x =
    negate (by i <| sin <| by i x)


cosh : Complex -> Complex
cosh x =
    cos <| by i x


sin : Complex -> Complex
sin (Complex x y) =
    Complex (Basics.sin x * cosh_ y) (Basics.cos x * sinh_ y)


cos : Complex -> Complex
cos (Complex x y) =
    Complex (Basics.cos x * cosh_ y) (Basics.sin x * sinh_ y)


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
