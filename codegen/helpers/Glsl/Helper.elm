module Glsl.Helper exposing (BinaryOperation(..), ComboOperation(..), Expr(..), Expression(..), IVec2, IVec3, IVec4, Mat3(..), Name(..), RelationOperation(..), Stat(..), Statement(..), Type(..), TypedName(..), TypingFunction, UnaryOperation(..), Vec2(..), Vec3(..), Vec4(..), Void, unsafeCall0, unsafeCall1, unsafeCall2, unsafeCall3, unsafeCall4)

import Set exposing (Set)


unsafeCall0 : String -> Expression r
unsafeCall0 name =
    unsafeCall name []


unsafeCall1 : String -> Expression t -> Expression r
unsafeCall1 name (Expression arg0) =
    unsafeCall name [ arg0 ]


unsafeCall2 : String -> Expression t -> Expression u -> Expression r
unsafeCall2 name (Expression arg0) (Expression arg1) =
    unsafeCall name [ arg0, arg1 ]


unsafeCall3 : String -> Expression t -> Expression u -> Expression v -> Expression r
unsafeCall3 name (Expression arg0) (Expression arg1) (Expression arg2) =
    unsafeCall name [ arg0, arg1, arg2 ]


unsafeCall4 : String -> Expression t -> Expression u -> Expression v -> Expression w -> Expression r
unsafeCall4 name (Expression arg0) (Expression arg1) (Expression arg2) (Expression arg3) =
    unsafeCall name [ arg0, arg1, arg2, arg3 ]


unsafeCall : String -> List ExprWithDeps -> Expression t
unsafeCall name args =
    Expression
        { expr =
            args
                |> List.map .expr
                |> Call name
        , deps =
            args
                |> List.map .deps
                |> List.foldr Set.union (Set.singleton name)
        }



-- Typed expressions


type Expression t
    = Expression ExprWithDeps


type alias ExprWithDeps =
    { expr : Expr
    , deps : Set String
    }


type Expr
    = Bool Bool
    | Int Int
    | Float Float
    | Variable String
    | And Expr Expr
    | Or Expr Expr
    | Comparison RelationOperation Expr Expr
    | Ternary Expr Expr Expr
    | UnaryOperation UnaryOperation Expr
    | BinaryOperation BinaryOperation Expr Expr
    | Call String (List Expr)
    | PostfixIncrement Expr
    | PostfixDecrement Expr
    | Unknown String
    | Dot Expr String
    | Array Expr Expr
    | Assign Expr Expr
    | AssignCombo ComboOperation Expr Expr


type BinaryOperation
    = Add
    | Subtract
    | By
    | Div


type UnaryOperation
    = Negate


type RelationOperation
    = LessThan
    | LessThanOrEquals
    | Equals
    | NotEquals
    | GreaterThanOrEquals
    | GreaterThan


type ComboOperation
    = ComboAdd
    | ComboSubtract
    | ComboBy
    | ComboDiv



-- Typed statements


type Statement r
    = Statement
        { stat : Stat
        , deps : Set String
        }


type Stat
    = If Expr Stat Stat
    | IfElse Expr Stat Stat Stat
    | For String Expr RelationOperation Expr Expr Stat Stat
    | Line String
    | Return Expr
    | Break
    | Continue
    | ExpressionStatement Expr Stat
    | Decl String Name (Maybe Expr) Stat
    | Nop


type alias TypingFunction t =
    String -> TypedName t


type TypedName t
    = TypedName Type Name


type Type
    = Type String


type Name
    = Name String



-- Typelevel types. Eh.


type Vec2
    = Vec2 Vec2


type Vec3
    = Vec3 Vec3


type Vec4
    = Vec4 Vec4


type IVec2
    = IVec2 IVec2


type IVec3
    = IVec3 IVec3


type IVec4
    = IVec4 IVec4


type Mat3
    = Mat3 Mat3


type Void
    = Void Void
