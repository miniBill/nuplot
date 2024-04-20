module UI.Glsl.Polynomial exposing (asPolynomial, getDegree, glslFromPolySolutions, glslFromSolutions)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), RelationOperation(..), UnaryOperation(..))
import Expression.Polynomial exposing (Exponents)
import Expression.Utils
import Glsl exposing (BisectSignature, Statement, Vec3, dot2X, dot2Y, dot3X, dot3Y, dot3Z, float1, int)
import Glsl.Functions exposing (abs1, cby22, cdiv22, csqrt2, min11, mix331)
import Glsl.Operations exposing (add22, add33, array33, by11, by12, by13, gt, lt, negate2, subtract22)
import Maybe.Extra as Maybe
import UI.Glsl.Code exposing (threshold)
import UI.Glsl.Generator exposing (ands, assign, assignOut, boolT, def, def2, def3, expr, floatT, fun4, mat3T, or, out, return, ternary, vec2T, vec3T, zero)


getDegree : Dict (List ( a, number )) b -> number
getDegree poly =
    poly
        |> Dict.toList
        |> List.map (Tuple.first >> List.map Tuple.second >> List.sum)
        |> List.maximum
        |> Maybe.withDefault 0


asPolynomial : List String -> Expression -> Maybe (Dict Exponents Float)
asPolynomial vars e =
    case e of
        RelationOperation Equals l r ->
            asPolynomial vars (Expression.Utils.minus l r)

        _ ->
            e
                |> Expression.Polynomial.asPolynomial vars
                |> Maybe.map Dict.toList
                |> Maybe.andThen (Maybe.traverse (\( k, v ) -> Maybe.map (Tuple.pair k) (toFloat v)))
                |> Maybe.map Dict.fromList


toFloat : Expression -> Maybe Float
toFloat e =
    case e of
        Integer i ->
            Just <| Basics.toFloat i

        Float f ->
            Just <| f

        AssociativeOperation Addition l m r ->
            (l :: m :: r)
                |> Maybe.traverse toFloat
                |> Maybe.map List.sum

        AssociativeOperation Multiplication l m r ->
            (l :: m :: r)
                |> Maybe.traverse toFloat
                |> Maybe.map List.product

        UnaryOperation Negate c ->
            Maybe.map negate <| toFloat c

        BinaryOperation Division n d ->
            Maybe.map2 (/) (toFloat n) (toFloat d)

        BinaryOperation Power b ex ->
            Maybe.map2 (^) (toFloat b) (toFloat ex)

        _ ->
            Nothing



-- getSolutions :
--     Glsl.Expression Float
--     -> Dict Int Expression
--     ->
--         Maybe
--             ( Int
--             , { t : Glsl.Expression Float
--               , o : Glsl.Expression Vec3
--               , dMix : Glsl.Expression Vec3
--               }
--               -> Statement r
--               -> Statement r
--             )
-- getSolutions maxDistance poly =
--     let
--         deg =
--             poly
--                 |> Dict.keys
--                 |> List.maximum
--                 |> Maybe.withDefault 0
--         get { dMix, o } degree =
--             Dict.get degree poly
--                 |> Maybe.withDefault Expression.Utils.zero
--                 |> UI.Glsl.Code.expressionToGlsl
--         -- [ ( "dx", dot3X dMix )
--         -- , ( "dy", dot3Y dMix )
--         -- , ( "dz", dot3Z dMix )
--         -- , ( "o.x", dot3X o )
--         -- , ( "o.y", dot3Y o )
--         -- , ( "o.z", dot3Z o )
--         -- ]
--         -- tFrom : Glsl.Expression Vec2 -> Glsl.Statement r -> Glsl.Statement r
--         tFrom { t } v =
--             expr
--                 (assign t
--                     (ternary
--                         (or
--                             (lt (dot2X v) zero)
--                             (gt (abs1 <| dot2Y v) (threshold maxDistance))
--                         )
--                         t
--                         (min11 (dot2X v) t)
--                     )
--                 )
--         sols =
--             case deg of
--                 1 ->
--                     Just <|
--                         \vars cont ->
--                             -- ax + b = 0
--                             -- x = -b / a
--                             def vec2T "a" (get vars 1) <|
--                                 \a ->
--                                     def vec2T "b" (get vars 0) <|
--                                         \b ->
--                                             def vec2T "x" (cdiv22 (negate2 b) a) <|
--                                                 \x ->
--                                                     tFrom vars x cont
--                 2 ->
--                     Just <|
--                         \vars cont ->
--                             -- ax² + bx + c = 0
--                             def3
--                                 ( vec2T "a", get vars 2 )
--                                 ( vec2T "b", get vars 1 )
--                                 ( vec2T "c", get vars 1 )
--                             <|
--                                 \a b c ->
--                                     def vec2T "delta" (subtract22 (cby22 b b) (by12 (float 4.0) (cby22 a c))) <|
--                                         \delta ->
--                                             def2
--                                                 ( vec2T "sqdelta", csqrt2 delta )
--                                                 ( vec2T "den", by12 (float 2.0) a )
--                                             <|
--                                                 \sqdelta den ->
--                                                     def vec2T "pos" (cdiv22 (add22 (negate2 b) sqdelta) den) <|
--                                                         \pos ->
--                                                             tFrom vars pos <|
--                                                                 def vec2T "neg" (cdiv22 (add22 (negate2 b) (negate2 sqdelta)) den) <|
--                                                                     \neg ->
--                                                                         tFrom vars neg cont
--                 {- 3 ->
--                    [ ( "!dx", "mix(d[0].x, d[1].x, 0.5)" )
--                    , ( "!dy", "mix(d[0].y, d[1].y, 0.5)" )
--                    , ( "!dz", "mix(d[0].z, d[1].z, 0.5)" )
--                    , ( "a", get 3 )
--                    , ( "b", get 2 )
--                    , ( "c", get 1 )
--                    , ( "dd", get 0 )
--                    , ( "deltaZero", "cby(b,b) - 3.0 * cby(a,c)" )
--                    , ( "deltaOne", "2.0 * cby(b,b,b) - 9.0 * cby(a,b,c) + 27.0 * cby(a,a, dd)" )
--                    , ( "inner", "csqrt(cby(deltaOne,deltaOne) - 4.0 * cby(deltaZero,deltaZero,deltaZero))" )
--                    , ( "CargPlus", "deltaOne + inner" )
--                    , ( "CargMinus", "deltaOne - inner" )
--                    , ( "Carg", "length(CargMinus) > length(CargPlus) ? CargMinus : CargPlus" )
--                    , ( "C", "ccbrt(0.5 * Carg)" )
--                    , ( "coeff", "-cdiv(vec2(1,0), 3.0 * a)" )
--                    , ( "xi", "0.5 * vec2(-1,sqrt(3.0))" )
--                    , ( "x", "cby(coeff, b + C + cdiv(deltaZero, C))" )
--                    , tFrom "x"
--                    , ( "C", "cby(xi,C)" )
--                    , ( "x", "cby(coeff, b + C + cdiv(deltaZero, C))" )
--                    , tFrom "x"
--                    , ( "C", "cby(xi,C)" )
--                    , ( "x", "cby(coeff, b + C + cdiv(deltaZero, C))" )
--                    , tFrom "x"
--                    ]
--                 -}
--                 _ ->
--                     Nothing
--     in
--     Maybe.map (Tuple.pair deg) sols


glslFromSolutions :
    String
    ->
        ({ t : Glsl.Expression Float
         , o : Glsl.Expression Vec3
         , dMix : Glsl.Expression Vec3
         }
         -> (() -> Statement Bool)
         -> Statement Bool
        )
    -> BisectSignature
glslFromSolutions suffix sols =
    fun4 boolT ("bisect" ++ suffix) (vec3T "o") (mat3T "d") (floatT "max_distance") (out vec3T "found") <| \o d maxDistance found ->
    vec3 "dMix" (mix331 (array33 d (int 0)) (array33 d (int 1)) (float1 0.5)) <| \dMix ->
    def floatT "t" (by11 maxDistance <| float1 2) <| \t ->
    sols { dMix = dMix, o = o, t = t } <| \_ ->
    expr (assignOut found (add33 o (by13 t (mix331 (array33 d (int 0)) (array33 d (int 1)) (float1 0.5))))) <| \_ ->
    return (ands [ lt t maxDistance, gt t zero ])


glslFromPolySolutions :
    String
    -> Glsl.Expression Float
    -> Dict Int Expression
    -> Maybe BisectSignature
glslFromPolySolutions suffix maxDistance poly =
    -- getSolutions maxDistance poly |> Maybe.map (\( _, sols ) -> glslFromSolutions suffix sols)
    let
        _ =
            Debug.todo
    in
    Nothing
