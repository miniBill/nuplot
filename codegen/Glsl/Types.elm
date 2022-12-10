module Glsl.Types exposing (BinaryOperation(..), BooleanOperation(..), Expression(..), ForDirection(..), Function, RelationOperation(..), Statement(..), Type(..), UnaryOperation(..))


type alias Function =
    { returnType : Type
    , name : String
    , args : List ( Type, String )
    , stat : Statement
    , body : String
    , hasSuffix : Bool
    , deps : Set String
    }


type Type
    = TFloat
    | TInt
    | TVec2
    | TIVec2
    | TIVec3
    | TIVec4
    | TVec3
    | TVec4
    | TMat3
    | TVoid
    | TBool


type Statement
    = Expression Expression Statement
    | For { var : String, from : Expression, op : RelationOperation, to : Expression, direction : ForDirection, step : Statement } Statement
    | If Expression Statement Statement
    | Return Expression
    | Def { type_ : Type, var : String, val : Expression } Statement
    | Decl { type_ : Type, var : String } Statement
    | Nop


type ForDirection
    = PlusPlus
    | MinusMinus


type RelationOperation
    = LessThanOrEquals
    | LessThan
    | Equals
    | NotEquals
    | Assign
    | GreaterThanOrEquals
    | GreaterThan


type BinaryOperation
    = Add
    | Subtract
    | By
    | Div


type BooleanOperation
    = And
    | Or


type UnaryOperation
    = Negate


type Expression
    = Float Float
    | Int Int
    | Bool Bool
    | Dot Expression String
    | Variable String
    | Constant String
    | Call String (List Expression)
    | Arr Expression Expression
    | Ternary Expression Expression Expression
    | UnaryOperation UnaryOperation Expression
    | BinaryOperation BinaryOperation Expression Expression
    | BooleanOperation BooleanOperation (List Expression)
    | RelationOperation RelationOperation Expression Expression
    | PostfixIncrement Expression
    | PostfixDecrement Expression
