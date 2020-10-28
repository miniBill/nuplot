module Expression.Utils exposing (a, abs_, asin_, associativeOperation, b, by, c, cos_, d, div, double, f, g, i, icomplex, int, ipow, minus, n, negate_, one, plus, pow, sin_, sqrt_, square, squash, triple, two, x, y, z, zero)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), UnaryOperation(..))



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


func : String -> Expression -> Expression
func name x_ =
    by [ Variable name, x_ ]


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
    func "sqrt"


square : Expression -> Expression
square base =
    ipow base 2


abs_ : Expression -> Expression
abs_ =
    func "abs"


sin_ : Expression -> Expression
sin_ =
    func "sin"


cos_ : Expression -> Expression
cos_ =
    func "cos"


asin_ : Expression -> Expression
asin_ =
    func "asin"


double : Expression -> Expression
double num =
    by [ two, num ]


triple : Expression -> Expression
triple num =
    by [ Integer 3, num ]


int : Int -> Expression
int =
    Integer


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


icomplex real immaginary =
    case ( real, immaginary ) of
        ( _, 0 ) ->
            Integer real

        ( 0, 1 ) ->
            i

        ( 0, _ ) ->
            by [ Integer immaginary, i ]

        ( _, 1 ) ->
            plus [ Integer real, i ]

        _ ->
            plus [ Integer real, by [ Integer immaginary, i ] ]
