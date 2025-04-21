module Glsl.Operations exposing
    ( add, adds, adds2, adds3, adds4
    , add1f, addf1
    , subtract, subtract1f, subtractf1
    , negate_
    , by, bymv
    , by1v, byv1, byfv, byvf
    , div, divv1, divvf
    , arraymi
    , lt, leq, eq, geq, gt
    , ternary
    , and, or, ands, ors
    )

{-|


# Addition

@docs add, adds, adds2, adds3, adds4
@docs add1f, addf1


# Subtraction

@docs subtract, subtract1f, subtractf1


# Negate

@docs negate_


# Multiplication

@docs by, bymv
@docs by1v, byv1, byfv, byvf


# Division

@docs div, divv1, divvf


# Array access

@docs arraymi


# Comparison

@docs lt, leq, eq, geq, gt


# Logic

@docs ternary
@docs and, or, ands, ors

-}

import Glsl exposing (BinaryOperation(..), Bool_, D2, Expr(..), Expression, Float_, Mat, RelationOperation(..), UnaryOperation(..), Vec, Vec2, Vec3, Vec4, false, float1, int1, true, unsafeMap, unsafeMap2, unsafeMap3)
import Glsl.Functions exposing (vec2i1, vec3i1, vec4i1)



-- Addition


{-| -}
add : Expression (Vec Float d) -> Expression (Vec Float d) -> Expression (Vec Float d)
add =
    unsafeBinary Add


{-| -}
adds : Expression (Vec Float d) -> List (Expression (Vec Float d)) -> Expression (Vec Float d)
adds vecZero es =
    case es of
        [] ->
            vecZero

        h :: t ->
            List.foldl (\e a -> add a e) h t


{-| -}
vec2Zero : Expression Vec2
vec2Zero =
    vec2i1 (int1 0)


{-| -}
vec3Zero : Expression Vec3
vec3Zero =
    vec3i1 (int1 0)


{-| -}
vec4Zero : Expression Vec4
vec4Zero =
    vec4i1 (int1 0)


{-| -}
adds2 : List (Expression Vec2) -> Expression Vec2
adds2 =
    adds vec2Zero


{-| -}
adds3 : List (Expression Vec3) -> Expression Vec3
adds3 =
    adds vec3Zero


{-| -}
adds4 : List (Expression Vec4) -> Expression Vec4
adds4 =
    adds vec4Zero


{-| -}
add1f : Expression Float_ -> Float -> Expression Float_
add1f l r =
    add l (float1 r)


{-| -}
addf1 : Float -> Expression Float_ -> Expression Float_
addf1 l r =
    add (float1 l) r



-- Subtraction


{-| -}
subtract : Expression (Vec Float d) -> Expression (Vec Float d) -> Expression (Vec Float d)
subtract =
    unsafeBinary Subtract


{-| -}
subtract1f : Expression Float_ -> Float -> Expression Float_
subtract1f l r =
    subtract l (float1 r)


{-| -}
subtractf1 : Float -> Expression Float_ -> Expression Float_
subtractf1 l r =
    subtract (float1 l) r


{-| -}
negate_ : Expression (Vec Float d) -> Expression (Vec Float d)
negate_ l =
    unsafeMap (UnaryOperation Negate) l


{-| -}
unsafeBinary : BinaryOperation -> Expression a -> Expression b -> Expression c
unsafeBinary op =
    unsafeMap2 (\l r -> BinaryOperation l op r)



-- Multiplication


{-| -}
by1v : Expression Float_ -> Expression (Vec Float d) -> Expression (Vec Float d)
by1v =
    unsafeBinary By


{-| -}
byfv : Float -> Expression (Vec Float d) -> Expression (Vec Float d)
byfv f =
    by1v (float1 f)


{-| -}
byv1 : Expression (Vec Float d) -> Expression Float_ -> Expression (Vec Float d)
byv1 =
    unsafeBinary By


{-| -}
byvf : Expression (Vec Float d) -> Float -> Expression (Vec Float d)
byvf v f =
    byv1 v (float1 f)


{-| -}
bymv : Expression (Mat Float D2 d) -> Expression (Vec Float d) -> Expression (Vec Float d)
bymv =
    unsafeBinary By


{-| -}
by : Expression (Vec Float d) -> Expression (Vec Float d) -> Expression (Vec Float d)
by =
    unsafeBinary By



-- Division


{-| -}
divv1 : Expression (Vec Float d) -> Expression Float_ -> Expression (Vec Float d)
divv1 =
    unsafeBinary Div


{-| -}
divvf : Expression (Vec Float d) -> Float -> Expression (Vec Float d)
divvf l r =
    divv1 l (float1 r)


{-| -}
div : Expression (Vec Float d) -> Expression (Vec Float d) -> Expression (Vec Float d)
div =
    unsafeBinary Div



-- Array access


{-| -}
arraymi : Expression (Mat Float d d) -> Expression Int -> Expression (Vec Float d)
arraymi =
    unsafeBinary ArraySubscript



-- Comparisons


{-| -}
lt : Expression Float_ -> Expression Float_ -> Expression Bool
lt =
    unsafeBinary (RelationOperation LessThan)


{-| -}
leq : Expression Float_ -> Expression Float_ -> Expression Bool
leq =
    unsafeBinary (RelationOperation LessThanOrEquals)


{-| -}
gt : Expression Float_ -> Expression Float_ -> Expression Bool
gt =
    unsafeBinary (RelationOperation GreaterThan)


{-| -}
geq : Expression Float_ -> Expression Float_ -> Expression Bool
geq =
    unsafeBinary (RelationOperation GreaterThanOrEquals)


{-| -}
eq : Expression t -> Expression t -> Expression Bool
eq =
    unsafeBinary (RelationOperation Equals)



--Ternary


{-| -}
ternary : Expression Bool_ -> Expression (Vec t d) -> Expression (Vec t d) -> Expression (Vec t d)
ternary =
    unsafeMap3 Ternary



-- Logic


{-| -}
and : Expression Bool_ -> Expression Bool_ -> Expression Bool_
and =
    unsafeMap2 (\l r -> BinaryOperation l And r)


{-| -}
ands : List (Expression Bool_) -> Expression Bool_
ands es =
    case es of
        [] ->
            true

        h :: t ->
            List.foldl (\e a -> and a e) h t


{-| -}
or : Expression Bool_ -> Expression Bool_ -> Expression Bool_
or =
    unsafeMap2 (\l r -> BinaryOperation l Or r)


{-| -}
ors : List (Expression Bool_) -> Expression Bool_
ors es =
    case es of
        [] ->
            false

        h :: t ->
            List.foldl (\e a -> or a e) h t
