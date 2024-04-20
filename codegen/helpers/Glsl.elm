module Glsl exposing
    ( Function
    , Statement(..), Stat(..), ForDirection(..)
    , Expression(..), ExprWithDeps, Expr(..)
    , BinaryOperation(..), UnaryOperation(..), RelationOperation(..), ComboOperation(..)
    , BisectSignature
    , true, false, var
    , unsafeDot, dot2X, dot2Y, dot3X, dot3Y, dot3Z
    , TypingFunction, TypedName(..), Type(..)
    , Vec2, Vec3, Vec4, IVec2, IVec3, IVec4, Mat3, Void, In, Out
    , unsafeCall0, unsafeCall1, unsafeCall2, unsafeCall3, unsafeCall4, unsafeCall5
    , unsafeMap, unsafeMap2, unsafeMap3
    , build, withExpression, withStatement, buildExpression, buildStatement
    , Declaration(..), Uniform, WithDepsBuilder, dot4XY, float1, int1, unsafeTypecast
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
@docs unsafeTypecase


# Building while tracking dependencies

@docs build, withExpression, withStatement, buildExpression, buildStatement

-}

import SortedSet exposing (SortedSet)



-- UNSAFE --


unsafeCall0 : String -> List String -> Expression r
unsafeCall0 name deps =
    build (Call name [])
        |> withDependencies deps
        |> buildExpression


unsafeCall1 : String -> List String -> Expression t -> Expression r
unsafeCall1 name deps arg0 =
    build (\a0 -> Call name [ a0 ])
        |> withDependencies deps
        |> withExpression arg0
        |> buildExpression


unsafeCall2 : String -> List String -> Expression t -> Expression u -> Expression r
unsafeCall2 name deps arg0 arg1 =
    build (\a0 a1 -> Call name [ a0, a1 ])
        |> withDependencies deps
        |> withExpression arg0
        |> withExpression arg1
        |> buildExpression


unsafeCall3 : String -> List String -> Expression t -> Expression u -> Expression v -> Expression r
unsafeCall3 name deps arg0 arg1 arg2 =
    build (\a0 a1 a2 -> Call name [ a0, a1, a2 ])
        |> withDependencies deps
        |> withExpression arg0
        |> withExpression arg1
        |> withExpression arg2
        |> buildExpression


unsafeCall4 : String -> List String -> Expression t -> Expression u -> Expression v -> Expression w -> Expression r
unsafeCall4 name deps arg0 arg1 arg2 arg3 =
    build (\a0 a1 a2 a3 -> Call name [ a0, a1, a2, a3 ])
        |> withDependencies deps
        |> withExpression arg0
        |> withExpression arg1
        |> withExpression arg2
        |> withExpression arg3
        |> buildExpression


unsafeCall5 : String -> List String -> Expression t -> Expression u -> Expression v -> Expression w -> Expression x -> Expression r
unsafeCall5 name deps arg0 arg1 arg2 arg3 arg4 =
    build (\a0 a1 a2 a3 a4 -> Call name [ a0, a1, a2, a3, a4 ])
        |> withDependencies deps
        |> withExpression arg0
        |> withExpression arg1
        |> withExpression arg2
        |> withExpression arg3
        |> withExpression arg4
        |> buildExpression


unsafeMap : (Expr -> Expr) -> Expression a -> Expression b
unsafeMap f l =
    build f
        |> withExpression l
        |> buildExpression


unsafeMap2 : (Expr -> Expr -> Expr) -> Expression a -> Expression b -> Expression c
unsafeMap2 f l r =
    build f
        |> withExpression l
        |> withExpression r
        |> buildExpression


unsafeMap3 : (Expr -> Expr -> Expr -> Expr) -> Expression a -> Expression b -> Expression c -> Expression d
unsafeMap3 f l m r =
    build f
        |> withExpression l
        |> withExpression m
        |> withExpression r
        |> buildExpression


unsafeTypecast : Expression a -> Expression b
unsafeTypecast (Expression a) =
    Expression a


type WithDepsBuilder k
    = WithDepsBuilder k (SortedSet String)


build : k -> WithDepsBuilder k
build k =
    WithDepsBuilder k SortedSet.empty


withExpression : Expression e -> WithDepsBuilder (Expr -> k) -> WithDepsBuilder k
withExpression (Expression e) (WithDepsBuilder k deps) =
    WithDepsBuilder (k e.expr)
        (deps |> SortedSet.insertAll e.deps)


withStatement : Statement r -> WithDepsBuilder (Stat -> k) -> WithDepsBuilder k
withStatement (Statement s) (WithDepsBuilder k deps) =
    WithDepsBuilder (k s.stat)
        (deps |> SortedSet.insertAll s.deps)


withDependencies : List String -> WithDepsBuilder k -> WithDepsBuilder k
withDependencies additionalDeps (WithDepsBuilder k deps) =
    WithDepsBuilder k
        (deps |> SortedSet.insertAll (SortedSet.fromList additionalDeps))


buildStatement : WithDepsBuilder Stat -> Statement r
buildStatement (WithDepsBuilder stat deps) =
    Statement
        { stat = stat
        , deps = deps
        }


buildExpression : WithDepsBuilder Expr -> Expression t
buildExpression (WithDepsBuilder expr deps) =
    Expression
        { expr = expr
        , deps = deps
        }



-- Typed expressions


type Declaration
    = FunctionDeclaration Function
    | UniformDeclaration Uniform


type alias Function =
    { returnType : Type
    , name : String
    , args : List ( Type, String )
    , stat : Stat
    , body : String
    , hasSuffix : Bool
    }


type alias Uniform =
    { tipe : Type
    , name : String
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
    bool1 True


false : Expression Bool
false =
    bool1 False


bool1 : Bool -> Expression Bool
bool1 b =
    pure <| Bool b


int1 : Int -> Expression Int
int1 i =
    pure <| Int i


float1 : Float -> Expression Float
float1 i =
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


dot4XY : Expression Vec4 -> Expression Vec2
dot4XY e =
    unsafeDot e "xy"


unsafeDot : Expression t -> String -> Expression a
unsafeDot ex v =
    unsafeMap (\e -> Dot e v) ex


pure : Expr -> Expression t
pure e =
    Expression
        { expr = e
        , deps = SortedSet.empty
        }
