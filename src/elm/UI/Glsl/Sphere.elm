module UI.Glsl.Sphere exposing (Sphere, asSphere, toGlsl)

import Dict exposing (Dict)
import Expression exposing (Expression)
import Expression.Polynomial exposing (Exponents)
import Glsl exposing (dot33)
import UI.Glsl.Code exposing (threshold)
import UI.Glsl.Generator exposing (FunDecl, Mat3, Vec3, assign, bool, boolT, def, expr, float, floatT, fun4, if_, int, mat3T, out, return, vec3T, zero)
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
                    Maybe.map3
                        (\x y z ->
                            Sphere
                                { center =
                                    { x = x
                                    , y = y
                                    , z = z
                                    }
                                , radius = sqrt <| max 0 <| x * x + y * y + z * z - Maybe.withDefault 0 (Dict.get [] poly)
                                }
                        )
                        (getCenter "x")
                        (getCenter "y")
                        (getCenter "z")
            )


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


toGlsl : String -> Sphere -> ( FunDecl, Expression Vec3 -> Expression Mat3 -> Expression Float -> Expression Vec3 -> Expression Bool )
toGlsl suffix (Sphere { center, radius }) =
    fun4 boolT ("bisect" ++ suffix) (vec3T "o") (mat3T "d") (floatT "max_distance") (out vec3T "found") <|
        \o d maxDistance found nop ->
            def vec3T "center" (vec3 (float center.x) (float center.y) (float center.z)) <|
                \centerVar ->
                    def vec3T "to_center" (subtract o centerVar) <|
                        \to_center ->
                            def floatT "b" (dot33 to_center (mix (arr d (int 0)) (arr d (int 1)) (float 0.5))) <|
                                \b ->
                                    def floatT "c" (subtract (dot33 to_center to_center) (float <| radius * radius)) <|
                                        \c ->
                                            def floatT "delta" (subtract (by b b) c) <|
                                                \delta ->
                                                    if_ (lt delta zero)
                                                        (return <| bool False)
                                                    <|
                                                        \_ ->
                                                            def floatT "x" (subtract (negate_ b) (sqrt_ delta)) <|
                                                                \x ->
                                                                    if_ (lt x <| threshold maxDistance)
                                                                        (return false)
                                                                    <|
                                                                        \_ ->
                                                                            expr (assign found (add o (byF x (mix (arr d (int 0)) (arr d (int 1)) (float 0.5))))) <|
                                                                                \_ ->
                                                                                    return true nop
