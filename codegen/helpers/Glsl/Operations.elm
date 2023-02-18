module Glsl.Operations exposing
    ( add11, add22, add33, add44
    , subtract11, subtract22, subtract33, subtract44
    , negate1, negate2, negate3, negate4
    , by11, by22, by33, by44
    )

{-|


# Addition

@docs add11, add22, add33, add44
@docs subtract11, subtract22, subtract33, subtract44
@docs negate1, negate2, negate3, negate4
@docs by11, by22, by33, by44

-}

import Glsl exposing (BinaryOperation(..), Expr(..), Expression(..), UnaryOperation(..), Vec2, Vec3, Vec4)


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


by : Expression a -> Expression a -> Expression a
by l r =
    Glsl.unsafeMap2 (BinaryOperation By) l r
