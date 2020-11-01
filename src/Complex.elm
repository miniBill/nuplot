module Complex exposing (Complex(..), abs, by, cos, div, fromReal, invert, minus, negate, one, plus, power, real, sin, sqrt, zero)


type Complex
    = Complex Float Float


zero : Complex
zero =
    Complex 0 0


one : Complex
one =
    Complex 1 0


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
negate (Complex r i) =
    Complex -r -i


minus : Complex -> Complex -> Complex
minus l r =
    plus l (negate r)


by : Complex -> Complex -> Complex
by (Complex lr li) (Complex rr ri) =
    Complex (lr * rr - li * ri) (li * rr + lr * ri)


invert : Complex -> Complex
invert (Complex r i) =
    let
        l =
            r * r + i * i
    in
    Complex (r / l) (-i / l)


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
abs (Complex r i) =
    Basics.sqrt (r * r + i * i)


theta : Complex -> Float
theta (Complex x y) =
    atan2 y x


toPolar : Complex -> ( Float, Float )
toPolar c =
    ( abs c, theta c )


exp : Complex -> Complex
exp (Complex r i) =
    Complex (e ^ r * Basics.cos i) (e ^ r * Basics.sin i)


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


sqrt : Complex -> Complex
sqrt (Complex x y) =
    Debug.todo "sqrt"
