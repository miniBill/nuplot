module UI.Glsl.Sphere exposing (Sphere, asSphere, toGlsl)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), UnaryOperation(..))
import List
import UI.Glsl.Code exposing (floatToGlsl)
import UI.Glsl.Poly as Poly exposing (Poly)


type Sphere
    = Sphere
        { center : { x : Float, y : Float, z : Float }
        , radius : Float
        }


asSphere : Expression -> Maybe Sphere
asSphere e =
    e
        |> Poly.asPoly
        |> Maybe.andThen reduce
        |> Maybe.andThen
            (\poly ->
                if
                    List.any
                        (\p ->
                            not <|
                                List.member p
                                    [ []
                                    , [ "x" ]
                                    , [ "y" ]
                                    , [ "z" ]
                                    , [ "x", "x" ]
                                    , [ "y", "y" ]
                                    , [ "z", "z" ]
                                    ]
                        )
                        (Dict.keys poly)
                then
                    Nothing

                else
                    -- (x-a)²+(y-b)²+(z-c)²=r²
                    -- x²-2ax+a² + y²-2by+b² + z²-2cz+c² - r² = 0
                    -- k = a² + b² + c² - r²
                    -- r² = a²+b²+c²-k
                    let
                        getCenter v =
                            case ( Dict.get [ v, v ] poly, Maybe.withDefault 0 <| Dict.get [ v ] poly ) of
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


reduce : Poly -> Maybe Poly
reduce poly =
    Dict.get [ "x", "x" ] poly
        |> Maybe.andThen
            (\c ->
                if c == 0 then
                    Nothing

                else
                    Just <| Dict.map (\_ v -> v / c) poly
            )


toGlsl : String -> Sphere -> String
toGlsl suffix (Sphere { center, radius }) =
    """
    bool bisect""" ++ suffix ++ """(vec3 o, vec3 d, float max_distance, out vec3 found) {
        vec3 center = vec3(""" ++ floatToGlsl center.x ++ """,""" ++ floatToGlsl center.y ++ """,""" ++ floatToGlsl center.z ++ """);

        vec3 to_center = o - center;
        float b = dot(to_center, d);
        float c = dot(to_center, to_center) - """ ++ floatToGlsl (radius * radius) ++ """;
        float delta = b*b - c;
        if(delta < 0.0)
            return false;
        float x = -b - sqrt(delta);
        if(x < 0.000001 * max_distance)
            return false;
        found = o + x * d;
        return true;
    }
    """
