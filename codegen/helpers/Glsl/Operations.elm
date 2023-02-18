module Glsl.Operations exposing (add11, add22, add33, add44)

{-|


# Addition

@docs add11, add22, add33, add44

-}

import Glsl exposing (BinaryOperation(..), Expr(..), Expression(..), Vec2, Vec3, Vec4)


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
