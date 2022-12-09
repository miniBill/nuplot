module Glsl.Helper exposing (ComboOperation(..), Expr(..), Expression(..), Expression1, Expression2, Expression3, Expression33, Expression4, ExpressionX, Mat3(..), Name(..), RelationOperation(..), Stat(..), Statement(..), TypingFunction, Vec2(..), Vec3(..), Vec4(..), innerCall)

import Set exposing (Set)


innerCall : String -> List ExprWithDeps -> List String -> ExprWithDeps
innerCall name args deps =
    { expr = Call name (List.map .expr args)
    , deps =
        List.map .deps args
            |> List.foldr Set.union (Set.fromList deps)
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
    | Add Expr Expr
    | Subtract Expr Expr
    | By Expr Expr
    | Div Expr Expr
    | Call String (List Expr)
    | Negate Expr
    | PostfixIncrement Expr
    | PostfixDecrement Expr
    | Unknown String
    | Dot Expr String
    | Array Expr Expr
    | Assign Expr Expr
    | AssignCombo ComboOperation Expr Expr


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


type alias ExpressionX a t =
    { a | base : Expression t }


type alias Expression1 t =
    { base : Expression t
    }


type alias Expression2 =
    { base : Expression Vec2
    , x : Expression1 Float
    , y : Expression1 Float
    , yx : Expression1 Vec2
    }


type alias Expression3 =
    { base : Expression Vec3
    , x : Expression1 Float
    , y : Expression1 Float
    , z : Expression1 Float
    }


type alias Expression4 =
    { base : Expression Vec4
    , x : Expression1 Float
    , y : Expression1 Float
    , z : Expression1 Float
    , w : Expression1 Float
    , xy : Expression2
    , yx : Expression2
    , yzw : Expression3
    }


type alias Expression33 =
    { base : Expression Mat3 }



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


type alias TypingFunction t r =
    String -> ( TypedName t r, Expression t -> r )


type TypedName t r
    = TypedName Type Name r


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


type Mat3
    = Mat3 Mat3
