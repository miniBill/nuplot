module UI.Glsl.Plane exposing (Plane, asPlane, toGlsl)

import Dict
import Expression exposing (Expression)
import Glsl exposing (BisectSignature, float, int)
import Glsl.Functions exposing (dot33, mix331, vec3111)
import Glsl.Operations exposing (add11, add33, array33, by13, div11, gt, negate1)
import Maybe
import UI.Glsl.Code exposing (threshold)
import UI.Glsl.Generator exposing (assignOut, boolT, def, def2, expr, floatT, fun4, mat3T, out, return, vec3T)
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


toGlsl : String -> Plane -> BisectSignature
toGlsl suffix (Plane { x, y, z, known }) =
    fun4 boolT ("bisect" ++ suffix) (vec3T "o") (mat3T "d") (floatT "max_distance") (out vec3T "found") <|
        \o d maxDistance found ->
            -- a x + b y + c z + k = 0
            -- x = ox + t dx
            -- y = oy + t dy
            -- z = oz + t dz
            -- a ox + a t dx + b oy + b t dy + c oz + c t dz + k = 0
            -- t (a dx + b dy + c dy) = - k - (a ox + b oy + c oz)
            -- t = - (k + dot abc o) / (dot abc d)
            def2
                ( vec3T "coeffs", vec3111 (float x) (float y) (float z) )
                ( vec3T "dMix", mix331 (array33 d (int 0)) (array33 d (int 1)) (float 0.5) )
            <|
                \coeffs dMix ->
                    def floatT
                        "t"
                        (div11
                            (negate1 <| add11 (float known) (dot33 coeffs o))
                            (dot33 coeffs dMix)
                        )
                    <|
                        \t ->
                            expr (assignOut found <| add33 o <| by13 t dMix) <|
                                return (gt t <| threshold maxDistance)
