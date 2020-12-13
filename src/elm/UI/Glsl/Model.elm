module UI.Glsl.Model exposing (GlslConstant(..), GlslFunction(..), GlslOperation(..))


type GlslFunction
    = Abs22
    | Acos22
    | Arg22
    | Asin22
    | Atan22
    | Atan222
    | Cos22
    | Cosh11
    | Cosh22
    | Exp22
    | Im22
    | Ln22
    | Log1022
    | Pw22
    | Re22
    | Sin22
    | Sinh11
    | Sinh22
    | Sqrt22
    | Tan22
    | Tanh11
    | Tanh22


type GlslConstant
    = E
    | Pi
    | I


type GlslOperation
    = GlslAddition
    | GlslMultiplication
    | GlslNegation
    | GlslDivision
    | GlslPower
