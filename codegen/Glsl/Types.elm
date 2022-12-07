module Glsl.Types exposing (BinaryOperation(..), BooleanOperation(..), Expression(..), Function, RelationOperation(..), Statement(..), UnaryOperation(..))


type alias Function =
    { returnType : String
    , name : String
    , args : List ( String, String )
    , body : Statement
    , hasSuffix : Bool
    }


type Statement
    = Expression Expression Statement
    | For String Expression RelationOperation Expression Bool Statement Statement
    | If Expression Statement Statement
    | Return Expression
    | Def String String Expression Statement
    | Decl String String Statement
    | Nop


type RelationOperation
    = LessThanOrEquals
    | LessThan
    | Equals
    | Assign
    | NotEquals
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
