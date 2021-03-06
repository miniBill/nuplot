module UI.Glsl.Plane exposing (Plane, asPlane, toGlsl)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), UnaryOperation(..))
import Maybe
import UI.Glsl.Code exposing (floatToGlsl, threshold)
import UI.Glsl.Polynomial as Polynomial


type Plane
    = Plane { x : Float, y : Float, z : Float, known : Float }


asPlane : Expression -> Maybe Plane
asPlane e =
    Polynomial.asPolynomial [ "x", "y", "z" ] e
        |> Maybe.andThen
            (\poly ->
                if Polynomial.getDegree poly /= 1 then
                    Nothing

                else
                    let
                        get k =
                            Maybe.withDefault 0 <| Dict.get k poly
                    in
                    Just <|
                        Plane
                            { x = get [ ( "x", 1 ) ]
                            , y = get [ ( "y", 1 ) ]
                            , z = get [ ( "z", 1 ) ]
                            , known = get []
                            }
            )


toGlsl : String -> Plane -> String
toGlsl suffix (Plane { x, y, z, known }) =
    -- a x + b y + c z + k = 0
    -- x = ox + t dx
    -- y = oy + t dy
    -- z = oz + t dz
    -- a ox + a t dx + b oy + b t dy + c oz + c t dz + k = 0
    -- t (a dx + b dy + c dy) = - k - (a ox + b oy + c oz)
    -- t = - (k + dot abc o) / (dot abc d)
    """
    bool bisect""" ++ suffix ++ """(vec3 o, vec3 d, float max_distance, out vec3 found) {
        vec3 coeffs = vec3(""" ++ floatToGlsl x ++ "," ++ floatToGlsl y ++ "," ++ floatToGlsl z ++ """);
        float t = -(""" ++ floatToGlsl known ++ """ + dot(coeffs, o)) / dot(coeffs, d);
        found = o + t * d;
        return t > """ ++ threshold ++ """;
    }
    """
