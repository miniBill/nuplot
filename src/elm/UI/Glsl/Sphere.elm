module UI.Glsl.Sphere exposing (Sphere, asSphere, toGlsl)

import Dict exposing (Dict)
import Expression exposing (Expression)
import Expression.Polynomial exposing (Exponents)
import Glsl exposing (BisectSignature, false, float1, int1, true)
import Glsl.Functions exposing (dot33, mix331, sqrt1, vec3111)
import Glsl.Generator exposing (assignOut, boolT, expr, float, floatT, fun4, if_, mat3T, out, return, vec3, vec3T, zero)
import Glsl.Operations exposing (add33, array33, by11, by13, lt, negate1, subtract11, subtract33)
import Maybe.Extra
import UI.Glsl.Code
import UI.Glsl.Polynomial as Polynomial


type Sphere
    = Sphere
        { center : { x : Float, y : Float, z : Float }
        , radius : Float
        }


asSphere : Expression -> Maybe Sphere
asSphere e =
    e
        |> Polynomial.asPolynomial [ "x", "y", "z" ]
        |> Maybe.andThen reduce
        |> Maybe.andThen
            (\poly ->
                if Polynomial.getDegree poly /= 2 then
                    Nothing

                else
                    -- (x-a)²+(y-b)²+(z-c)²=r²
                    -- x²-2ax+a² + y²-2by+b² + z²-2cz+c² - r² = 0
                    -- k = a² + b² + c² - r²
                    -- r² = a²+b²+c²-k
                    let
                        getCenter : String -> Maybe Float
                        getCenter v =
                            case ( Dict.get [ ( v, 2 ) ] poly, Maybe.withDefault 0 <| Dict.get [ ( v, 1 ) ] poly ) of
                                ( Just s, l ) ->
                                    if s /= 1 then
                                        Nothing

                                    else
                                        Just <| -l / 2

                                _ ->
                                    Nothing
                    in
                    Maybe.Extra.andThen3
                        (\x y z ->
                            let
                                rSquared : Float
                                rSquared =
                                    x * x + y * y + z * z - Maybe.withDefault 0 (Dict.get [] poly)
                            in
                            if rSquared >= 0 then
                                { center =
                                    { x = x
                                    , y = y
                                    , z = z
                                    }
                                , radius = sqrt rSquared
                                }
                                    |> Sphere
                                    |> Just

                            else
                                Nothing
                        )
                        (getCenter "x")
                        (getCenter "y")
                        (getCenter "z")
            )
        -- TODO: fix
        |> always Nothing


reduce : Dict Exponents Float -> Maybe (Dict Exponents Float)
reduce poly =
    Dict.get [ ( "x", 2 ) ] poly
        |> Maybe.andThen
            (\c ->
                if c == 0 then
                    Nothing

                else
                    Just <| Dict.map (\_ v -> v / c) poly
            )


toGlsl : String -> Sphere -> BisectSignature
toGlsl suffix (Sphere { center, radius }) =
    fun4 boolT ("bisect" ++ suffix) (vec3T "o") (mat3T "d") (floatT "max_distance") (out vec3T "found") <| \o d maxDistance found ->
    vec3 "center" (vec3111 (float1 center.x) (float1 center.y) (float1 center.z)) <| \centerVar ->
    vec3 "dMix" (mix331 (array33 d (int1 0)) (array33 d (int1 1)) (float1 0.5)) <| \dMix ->
    vec3 "to_center" (subtract33 o centerVar) <| \to_center ->
    float "b" (dot33 to_center dMix) <| \b ->
    float "c" (subtract11 (dot33 to_center to_center) (float1 <| radius * radius)) <| \c ->
    float "delta" (subtract11 (by11 b b) c) <| \delta ->
    if_ (lt delta zero)
        (return false)
    <| \_ ->
    float "x" (subtract11 (negate1 b) (sqrt1 delta)) <| \x ->
    if_ (lt x <| UI.Glsl.Code.toThreshold maxDistance)
        (return false)
    <| \_ ->
    expr (assignOut found (add33 o (by13 x dMix))) <| \_ ->
    return true
