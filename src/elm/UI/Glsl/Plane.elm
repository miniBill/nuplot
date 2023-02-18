module UI.Glsl.Plane exposing (Plane, asPlane, toGlsl)

import Dict
import Expression exposing (Expression)
import Glsl exposing (dot33)
import Maybe
import UI.Glsl.Code exposing (threshold)
import UI.Glsl.Generator exposing (Expression1, ExpressionX, FunDecl, Mat3, Vec3, add, arr, assign, boolT, byF, def, div, expr, float, floatT, fun4, gt, int, mat3T, negate_, out, return, vec3, vec3T)
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


toGlsl : String -> Plane -> ( FunDecl, Expression Vec3 -> Expression Mat3 -> Expression Float -> Expression Vec3 -> Expression1 Bool )
toGlsl suffix (Plane { x, y, z, known }) =
    fun4 boolT ("bisect" ++ suffix) (vec3T "o") (mat3T "d") (floatT "max_distance") (out vec3T "found") <|
        \o d maxDistance found nop ->
            -- a x + b y + c z + k = 0
            -- x = ox + t dx
            -- y = oy + t dy
            -- z = oz + t dz
            -- a ox + a t dx + b oy + b t dy + c oz + c t dz + k = 0
            -- t (a dx + b dy + c dy) = - k - (a ox + b oy + c oz)
            -- t = - (k + dot abc o) / (dot abc d)
            def vec3T "coeffs" (vec3 (float x) (float y) (float z)) <|
                \coeffs ->
                    def vec3T "dsum" (byF (float 0.5) <| add (arr d <| int 0) (arr d <| int 1)) <|
                        \dsum ->
                            def floatT
                                "t"
                                (div
                                    (negate_ <| add (float known) (dot coeffs o))
                                    (dot33 coeffs dsum)
                                )
                            <|
                                \t ->
                                    expr (assign found <| add o <| byF t dsum) <|
                                        \_ ->
                                            return (gt t <| threshold maxDistance) nop
