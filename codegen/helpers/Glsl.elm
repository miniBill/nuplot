module Glsl exposing
    ( Function
    , Statement(..), Stat(..), ForDirection(..)
    , Expression(..), Expr(..)
    , BinaryOperation(..), UnaryOperation(..), RelationOperation(..), ComboOperation(..)
    , BisectSignature
    , true, false, int, float, var
    , unsafeDot, dot2X, dot2Y, dot3X, dot3Y, dot3Z
    , TypingFunction, TypedName(..), Type(..)
    , Vec2, Vec3, Vec4, IVec2, IVec3, IVec4, Mat3, Void, In, Out
    , unsafeCall0, unsafeCall1, unsafeCall2, unsafeCall3, unsafeCall4, unsafeCall5
    , unsafeMap, unsafeMap2, unsafeMap3
    , buildStatement, statement, unsafeExprStat, unsafeStat1, withExpression, withStatement
    )

{-|


# Types

@docs Function
@docs Statement, Stat, ForDirection
@docs Expression, ExprWithDeps, Expr
@docs BinaryOperation, UnaryOperation, RelationOperation, ComboOperation
@docs BisectSignature


# Utils

@docs true, false, int, float, var
@docs unsafeDot, dot2X, dot2Y, dot3X, dot3Y, dot3Z


# Typelevel types

@docs TypingFunction, TypedName, Type
@docs Vec2, Vec3, Vec4, IVec2, IVec3, IVec4, Mat3, Void, In, Out


# Escape hatches

@docs unsafeCall0, unsafeCall1, unsafeCall2, unsafeCall3, unsafeCall4, unsafeCall5
@docs unsafeMap, unsafeMap2, unsafeMap3

-}

import SortedSet exposing (SortedSet)



-- UNSAFE --


unsafeCall0 : String -> List String -> Expression r
unsafeCall0 name deps =
    unsafeCall name deps []


unsafeCall1 : String -> List String -> Expression t -> Expression r
unsafeCall1 name deps (Expression arg0) =
    unsafeCall name deps [ arg0 ]


unsafeCall2 : String -> List String -> Expression t -> Expression u -> Expression r
unsafeCall2 name deps (Expression arg0) (Expression arg1) =
    unsafeCall name deps [ arg0, arg1 ]


unsafeCall3 : String -> List String -> Expression t -> Expression u -> Expression v -> Expression r
unsafeCall3 name deps (Expression arg0) (Expression arg1) (Expression arg2) =
    unsafeCall name deps [ arg0, arg1, arg2 ]


unsafeCall4 : String -> List String -> Expression t -> Expression u -> Expression v -> Expression w -> Expression r
unsafeCall4 name deps (Expression arg0) (Expression arg1) (Expression arg2) (Expression arg3) =
    unsafeCall name deps [ arg0, arg1, arg2, arg3 ]


unsafeCall5 : String -> List String -> Expression t -> Expression u -> Expression v -> Expression w -> Expression x -> Expression r
unsafeCall5 name deps (Expression arg0) (Expression arg1) (Expression arg2) (Expression arg3) (Expression arg4) =
    unsafeCall name deps [ arg0, arg1, arg2, arg3, arg4 ]


unsafeCall : String -> List String -> List ExprWithDeps -> Expression t
unsafeCall name deps args =
    Expression
        { expr =
            args
                |> List.map .expr
                |> Call name
        , deps =
            args
                |> List.map .deps
                |> List.foldl SortedSet.insertAll (SortedSet.fromList deps)
        }


unsafeMap : (Expr -> Expr) -> Expression a -> Expression b
unsafeMap f (Expression l) =
    Expression
        { expr = f l.expr
        , deps = l.deps
        }


unsafeMap2 : (Expr -> Expr -> Expr) -> Expression a -> Expression b -> Expression c
unsafeMap2 f (Expression l) (Expression r) =
    Expression
        { expr = f l.expr r.expr
        , deps =
            l.deps
                |> SortedSet.insertAll r.deps
        }


unsafeMap3 : (Expr -> Expr -> Expr -> Expr) -> Expression a -> Expression b -> Expression c -> Expression d
unsafeMap3 f (Expression l) (Expression m) (Expression r) =
    Expression
        { expr = f l.expr m.expr r.expr
        , deps =
            l.deps
                |> SortedSet.insertAll m.deps
                |> SortedSet.insertAll r.deps
        }


unsafeStat1 : (Stat -> Stat) -> Statement r -> Statement r
unsafeStat1 f (Statement stat) =
    Statement
        { stat = f stat.stat
        , deps = stat.deps
        }


unsafeExprStat : (Expr -> Stat -> Stat) -> Expression a -> Statement s -> Statement t
unsafeExprStat f (Expression l) (Statement r) =
    Statement
        { stat = f l.expr r.stat
        , deps =
            l.deps
                |> SortedSet.insertAll r.deps
        }


type StatementBuilder k
    = StatementBuilder k (SortedSet String)


statement : k -> StatementBuilder k
statement k =
    StatementBuilder k SortedSet.empty


withExpression : Expression e -> StatementBuilder (Expr -> k) -> StatementBuilder k
withExpression (Expression e) (StatementBuilder k deps) =
    StatementBuilder (k e.expr)
        (deps |> SortedSet.insertAll e.deps)


withStatement : Statement r -> StatementBuilder (Stat -> k) -> StatementBuilder k
withStatement (Statement s) (StatementBuilder k deps) =
    StatementBuilder (k s.stat)
        (deps |> SortedSet.insertAll s.deps)


buildStatement : StatementBuilder Stat -> Statement r
buildStatement (StatementBuilder stat deps) =
    Statement
        { stat = stat
        , deps = deps
        }



-- Typed expressions


type alias Function =
    { returnType : Type
    , name : String
    , args : List ( Type, String )
    , stat : Stat
    , body : String
    , hasSuffix : Bool
    }


type Expression t
    = Expression ExprWithDeps


type alias ExprWithDeps =
    { expr : Expr
    , deps : SortedSet String
    }


type Expr
    = Bool Bool
    | Int Int
    | Float Float
    | Variable String
    | Comparison RelationOperation Expr Expr
    | Ternary Expr Expr Expr
    | UnaryOperation UnaryOperation Expr
    | BinaryOperation BinaryOperation Expr Expr
    | Call String (List Expr)
    | PostfixIncrement Expr
    | PostfixDecrement Expr
    | Dot Expr String
    | Array Expr Expr
    | AssignCombo ComboOperation Expr Expr


type BinaryOperation
    = Add
    | Subtract
    | By
    | Div
    | And
    | Or


type UnaryOperation
    = Negate


type RelationOperation
    = LessThan
    | LessThanOrEquals
    | Equals
    | NotEquals
    | GreaterThanOrEquals
    | GreaterThan
    | Assign


type ComboOperation
    = ComboAdd
    | ComboSubtract
    | ComboBy
    | ComboDiv


type alias BisectSignature =
    Expression Vec3
    -> Expression Mat3
    -> Expression Float
    -> Expression (Out Vec3)
    -> Expression Bool



-- Typed statements


type Statement r
    = Statement
        { stat : Stat
        , deps : SortedSet String
        }


type Stat
    = If Expr Stat Stat
    | IfElse Expr Stat Stat Stat
    | For String Expr RelationOperation Expr ForDirection Stat Stat
    | Return Expr
    | Break
    | Continue
    | ExpressionStatement Expr Stat
    | Decl Type String (Maybe Expr) Stat
    | Nop


type ForDirection
    = PlusPlus
    | MinusMinus


type alias TypingFunction t =
    String -> TypedName t


type TypedName t
    = TypedName Type String


type Type
    = TVoid
    | TFloat
    | TInt
    | TBool
    | TVec2
    | TVec3
    | TVec4
    | TIVec2
    | TIVec3
    | TIVec4
    | TMat3
    | TIn Type
    | TOut Type



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


type In t
    = In (In t)


type Out t
    = Out (Out t)



-- Utils


true : Expression Bool
true =
    bool True


false : Expression Bool
false =
    bool False


bool : Bool -> Expression Bool
bool b =
    pure <| Bool b


int : Int -> Expression Int
int i =
    pure <| Int i


float : Float -> Expression Float
float i =
    pure <| Float i


var : String -> Expression t
var v =
    pure <| Variable v


dot2X : Expression Vec2 -> Expression Float
dot2X e =
    unsafeDot e "x"


dot2Y : Expression Vec2 -> Expression Float
dot2Y e =
    unsafeDot e "y"


dot3X : Expression Vec3 -> Expression Float
dot3X e =
    unsafeDot e "x"


dot3Y : Expression Vec3 -> Expression Float
dot3Y e =
    unsafeDot e "y"


dot3Z : Expression Vec3 -> Expression Float
dot3Z e =
    unsafeDot e "z"


unsafeDot : Expression t -> String -> Expression a
unsafeDot ex v =
    unsafeMap (\e -> Dot e v) ex


pure : Expr -> Expression t
pure e =
    Expression
        { expr = e
        , deps = SortedSet.empty
        }
