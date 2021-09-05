module UI.Glsl.Sphere exposing (Sphere, asSphere, toGlsl)

import Dict exposing (Dict)
import Expression exposing (Expression)
import Expression.Polynomial exposing (Exponents)
import UI.Glsl.Code exposing (threshold)
import UI.Glsl.Generator as Generator exposing (Expression1, ExpressionX, FunDecl, Mat3, Vec3, boolT, floatT, floatToGlsl, fun4, mat3T, out, unknownStatement, vec3T)
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


toGlsl : String -> Sphere -> ( FunDecl, ExpressionX xa Vec3 -> ExpressionX xb Mat3 -> ExpressionX xc Float -> ExpressionX xd Vec3 -> Expression1 Bool )
toGlsl suffix (Sphere { center, radius }) =
    fun4 boolT ("bisect" ++ suffix) (vec3T "o") (mat3T "d") (floatT "max_distance") (out vec3T "found") <| \o d maxDistance found ->
    unknownStatement
        ("""
        vec3 center = vec3(""" ++ floatToGlsl center.x ++ """,""" ++ floatToGlsl center.y ++ """,""" ++ floatToGlsl center.z ++ """);

        vec3 to_center = o - center;
        float b = dot(to_center, mix(d[0], d[1], 0.5));
        float c = dot(to_center, to_center) - """ ++ floatToGlsl (radius * radius) ++ """;
        float delta = b*b - c;
        if(delta < 0.)
            return false;
        float x = -b - sqrt(delta);
        if(x < """ ++ Generator.expressionToGlsl (threshold maxDistance).base ++ """)
            return false;
        found = o + x * mix(d[0], d[1], 0.5);
        return true;
    """)
