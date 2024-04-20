module Glsl.Operations exposing
    ( add11, add22, add33, add44
    , subtract11, subtract22, subtract33, subtract44
    , negate1, negate2, negate3, negate4
    , by11, by22, by33, by44
    , by12, by13
    , by31
    , div11, div22, div33, div44
    , array33
    , lt, eq, gt
    )

{-|


# Addition

@docs add11, add22, add33, add44


# Subtraction

@docs subtract11, subtract22, subtract33, subtract44


# Negate

@docs negate1, negate2, negate3, negate4


# Multiplication

@docs by11, by22, by33, by44
@docs by12, by13
@docs by31


# Division

@docs div11, div22, div33, div44


# Array access

@docs array33


# Comparison

@docs lt, eq, gt

-}

import Expression exposing (BinaryOperation(..))
import Glsl exposing (BinaryOperation(..), Expr(..), Expression, Mat3, RelationOperation(..), UnaryOperation(..), Vec2, Vec3, Vec4)



-- Addition


add11 : Expression Float -> Expression Float -> Expression Float
add11 =
    add


add22 : Expression Vec2 -> Expression Vec2 -> Expression Vec2
add22 =
    add


add33 : Expression Vec3 -> Expression Vec3 -> Expression Vec3
add33 =
    add


add44 : Expression Vec4 -> Expression Vec4 -> Expression Vec4
add44 =
    add


add : Expression a -> Expression a -> Expression a
add l r =
    Glsl.unsafeMap2 (BinaryOperation Add) l r



-- Subtraction


subtract11 : Expression Float -> Expression Float -> Expression Float
subtract11 =
    subtract


subtract22 : Expression Vec2 -> Expression Vec2 -> Expression Vec2
subtract22 =
    subtract


subtract33 : Expression Vec3 -> Expression Vec3 -> Expression Vec3
subtract33 =
    subtract


subtract44 : Expression Vec4 -> Expression Vec4 -> Expression Vec4
subtract44 =
    subtract


subtract : Expression a -> Expression a -> Expression a
subtract l r =
    Glsl.unsafeMap2 (BinaryOperation Subtract) l r



-- Negation


negate1 : Expression Float -> Expression Float
negate1 =
    negate


negate2 : Expression Vec2 -> Expression Vec2
negate2 =
    negate


negate3 : Expression Vec3 -> Expression Vec3
negate3 =
    negate


negate4 : Expression Vec4 -> Expression Vec4
negate4 =
    negate


negate : Expression a -> Expression a
negate l =
    Glsl.unsafeMap (UnaryOperation Negate) l



-- Multiplication


by11 : Expression Float -> Expression Float -> Expression Float
by11 =
    by


by22 : Expression Vec2 -> Expression Vec2 -> Expression Vec2
by22 =
    by


by33 : Expression Vec3 -> Expression Vec3 -> Expression Vec3
by33 =
    by


by44 : Expression Vec4 -> Expression Vec4 -> Expression Vec4
by44 =
    by


by12 : Expression Float -> Expression Vec2 -> Expression Vec2
by12 l r =
    Glsl.unsafeMap2 (BinaryOperation By) l r


by13 : Expression Float -> Expression Vec3 -> Expression Vec3
by13 l r =
    Glsl.unsafeMap2 (BinaryOperation By) l r


by31 : Expression Vec3 -> Expression Float -> Expression Vec3
by31 l r =
    Glsl.unsafeMap2 (BinaryOperation By) l r


by : Expression a -> Expression a -> Expression a
by l r =
    Glsl.unsafeMap2 (BinaryOperation By) l r



-- Division


div11 : Expression Float -> Expression Float -> Expression Float
div11 =
    div


div22 : Expression Vec2 -> Expression Vec2 -> Expression Vec2
div22 =
    div


div33 : Expression Vec3 -> Expression Vec3 -> Expression Vec3
div33 =
    div


div44 : Expression Vec4 -> Expression Vec4 -> Expression Vec4
div44 =
    div


div : Expression a -> Expression a -> Expression a
div l r =
    Glsl.unsafeMap2 (BinaryOperation Div) l r



-- Array access


array33 : Expression Mat3 -> Expression Int -> Expression Vec3
array33 =
    Glsl.unsafeMap2 Array



-- Comparisons


lt : Expression Float -> Expression Float -> Expression Bool
lt =
    Glsl.unsafeMap2 (Comparison LessThan)


gt : Expression Float -> Expression Float -> Expression Bool
gt =
    Glsl.unsafeMap2 (Comparison GreaterThan)


eq : Expression t -> Expression t -> Expression Bool
eq =
    Glsl.unsafeMap2 (Comparison Equals)
