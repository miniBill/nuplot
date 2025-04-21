module Glsl.Dot exposing
    ( x, y, z, w
    , xx, xy, xz, xw
    , yx, yy, yz, yw
    , zx, zy, zz, zw
    , wx, wy, wz, ww
    , Dot(..)
    , dot1, dot2, dot3, dot4
    , dx, dy, dz, dw
    )

{-|


# Simple swizzles

@docs x, y, z, w
@docs xx, xy, xz, xw
@docs yx, yy, yz, yw
@docs zx, zy, zz, zw
@docs wx, wy, wz, ww


# Arbitrary swizzles

@docs Dot
@docs dot1, dot2, dot3, dot4
@docs dx, dy, dz, dw

-}

import Glsl exposing (D1, D2, D3, D4, Expression, Vec, unsafeDot)


type Dot q
    = Dotter Char


dx : Dot { a | x : x }
dx =
    Dotter 'x'


dy : Dot { a | y : y }
dy =
    Dotter 'y'


dz : Dot { a | z : z }
dz =
    Dotter 'z'


dw : Dot { a | w : w }
dw =
    Dotter 'w'


dot1 : Dot q -> Expression (Vec t q) -> Expression (Vec t D1)
dot1 (Dotter d1) e =
    unsafeDot e (String.fromChar d1)


dot2 : Dot q -> Dot q -> Expression (Vec t q) -> Expression (Vec t D2)
dot2 (Dotter d1) (Dotter d2) e =
    unsafeDot e (String.fromList [ d1, d2 ])


xx : Expression (Vec t { a | x : x }) -> Expression (Vec t D2)
xx =
    dot2 dx dx


xy : Expression (Vec t { a | x : x, y : y }) -> Expression (Vec t D2)
xy =
    dot2 dx dy


xz : Expression (Vec t { a | x : x, z : z }) -> Expression (Vec t D2)
xz =
    dot2 dx dz


xw : Expression (Vec t { a | x : x, w : w }) -> Expression (Vec t D2)
xw =
    dot2 dx dw


yx : Expression (Vec t { a | y : y, x : x }) -> Expression (Vec t D2)
yx =
    dot2 dy dx


yy : Expression (Vec t { a | y : y }) -> Expression (Vec t D2)
yy =
    dot2 dy dy


yz : Expression (Vec t { a | y : y, z : z }) -> Expression (Vec t D2)
yz =
    dot2 dy dz


yw : Expression (Vec t { a | y : y, w : w }) -> Expression (Vec t D2)
yw =
    dot2 dy dw


zx : Expression (Vec t { a | z : z, x : x }) -> Expression (Vec t D2)
zx =
    dot2 dz dx


zy : Expression (Vec t { a | z : z, y : y }) -> Expression (Vec t D2)
zy =
    dot2 dz dy


zz : Expression (Vec t { a | z : z }) -> Expression (Vec t D2)
zz =
    dot2 dz dz


zw : Expression (Vec t { a | z : z, w : w }) -> Expression (Vec t D2)
zw =
    dot2 dz dw


wx : Expression (Vec t { a | w : w, x : x }) -> Expression (Vec t D2)
wx =
    dot2 dw dx


wy : Expression (Vec t { a | w : w, y : y }) -> Expression (Vec t D2)
wy =
    dot2 dw dy


wz : Expression (Vec t { a | w : w, z : z }) -> Expression (Vec t D2)
wz =
    dot2 dw dz


ww : Expression (Vec t { a | w : w }) -> Expression (Vec t D2)
ww =
    dot2 dw dw


dot3 : Dot q -> Dot q -> Dot q -> Expression (Vec t q) -> Expression (Vec t D3)
dot3 (Dotter d1) (Dotter d2) (Dotter d3) e =
    unsafeDot e (String.fromList [ d1, d2, d3 ])


dot4 : Dot q -> Dot q -> Dot q -> Dot q -> Expression (Vec t q) -> Expression (Vec t D4)
dot4 (Dotter d1) (Dotter d2) (Dotter d3) (Dotter d4) e =
    unsafeDot e (String.fromList [ d1, d2, d3, d4 ])


x : Expression (Vec t { a | x : x }) -> Expression (Vec t D1)
x e =
    dot1 dx e


y : Expression (Vec t { a | y : y }) -> Expression (Vec t D1)
y e =
    dot1 dy e


z : Expression (Vec t { a | z : z }) -> Expression (Vec t D1)
z e =
    dot1 dz e


w : Expression (Vec t { a | w : w }) -> Expression (Vec t D1)
w e =
    dot1 dw e
