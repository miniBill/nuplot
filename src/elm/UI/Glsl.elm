module UI.Glsl exposing (getGlsl)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), RelationOperation(..))
import Expression.Graph exposing (Graph(..))
import Expression.Polynomial exposing (asPolynomial)
import Expression.Utils exposing (by, minus, plus, square)
import Glsl.Helper
import Maybe.Extra as Maybe
import SortedAnySet as Set
import UI.Glsl.Code exposing (atanPlusDecl, constantToGlsl, dupDecl, gnumDecl, intervalFunctionToGlsl, intervalOperationToGlsl, mainGlsl, straightFunctionToGlsl, straightOperationToGlsl, toSrc3D, toSrcContour, toSrcImplicit, toSrcParametric, toSrcPolar, toSrcRelation, toSrcVectorField2D)
import UI.Glsl.Generator as Generator exposing (Expression1, ExpressionX, FunDecl, Mat3, Vec3, expr, fileToGlsl, unknown)
import UI.Glsl.Plane as Plane
import UI.Glsl.Polynomial
import UI.Glsl.Sphere as Sphere


getGlsl : Bool -> Graph -> String
getGlsl rayDifferentials graph =
    let
        { expr, funDecls, usesThetaDelta, pixel2D, pixel3D } =
            extract "" graph

        build2d toSrc e =
            let
                ( decl, call ) =
                    toSrc e
            in
            { expr = e
            , funDecls = decl
            , usesThetaDelta = False
            , pixel2D = [ { call = call, color = True } ]
            , pixel3D = []
            }

        extract prefix g =
            case g of
                Explicit2D c ->
                    build2d (toSrcImplicit prefix) <|
                        minus Expression.Utils.y c

                Implicit2D l r ->
                    build2d (toSrcImplicit prefix) <|
                        minus l r

                Polar2D e ->
                    build2d (toSrcPolar prefix) e

                Parametric2D x y ->
                    { pixel2D =
                        [ { call = toSrcParametric prefix e
                          , color = True
                          }
                        ]
                    , pixel3D = []
                    }

                Relation2D e ->
                    build2d (toSrcRelation prefix) e

                Contour e ->
                    { pixel2D = [ { call = toSrcContour prefix e, color = False } ]
                    , pixel3D = []
                    }

                Implicit3D e ->
                    let
                        f =
                            get3DSource prefix e
                    in
                    { expr = f.expr
                    , pixel2D = []
                    , pixel3D = [ f.bisect ]
                    }

                GraphList children ->
                    let
                        prefix_ i =
                            prefix ++ "_" ++ String.fromInt i

                        extracted =
                            List.indexedMap (extract << prefix_) children
                    in
                    { pixel2D = List.concatMap .pixel2D extracted
                    , pixel3D = List.concatMap .pixel3D extracted
                    }

                VectorField2D x y ->
                    { pixel2D = [ { call = toSrcVectorField2D prefix x y, color = False } ]
                    , pixel3D = []
                    }

        reqs =
            let
                _ =
                    -- Can remove in the future
                    Debug.todo
            in
            expr
                |> expressionToRequirements
                |> List.sortWith requirementSort
                |> List.map (requirementToGlsl interval)
                |> String.join "\n"
    in
    reqs
        ++ "\n/* Expression */\n"
        ++ Generator.fileToGlsl funDecls
        ++ mainGlsl rayDifferentials pixel2D pixel3D


get3DSource :
    String
    -> Expression
    ->
        (Glsl.Helper.Expression Vec3
         -> Glsl.Helper.Expression Mat3
         -> Glsl.Helper.Expression Float
         -> Glsl.Helper.Expression Vec3
         -> Glsl.Helper.Expression Bool
        )
get3DSource prefix e =
    case Sphere.asSphere e |> Maybe.map (Sphere.toGlsl prefix) of
        Just bisect ->
            bisect

        Nothing ->
            case Plane.asPlane e |> Maybe.map (Plane.toGlsl prefix) of
                Just ( glsl, bisect ) ->
                    { bisect = bisect
                    }

                Nothing ->
                    case tryGlslFromPolynomial prefix e of
                        Just o ->
                            o

                        Nothing ->
                            let
                                ( glsl, interval ) =
                                    toSrc3D prefix e

                                ( decl, bisect ) =
                                    UI.Glsl.Code.suffixToBisect interval prefix
                            in
                            { expr = e
                            , funDecls = glsl ++ [ decl ]
                            , bisect = bisect
                            }


tryGlslFromPolynomial :
    String
    -> Expression
    ->
        Maybe
            { expr : Expression
            , funDecls : List FunDecl
            , bisect :
                Glsl.Helper.Expression Vec3
                -> Glsl.Helper.Expression Mat3
                -> Glsl.Helper.Expression Float
                -> Glsl.Helper.Expression Vec3
                -> Glsl.Helper.Expression Bool
            }
tryGlslFromPolynomial suffix e =
    let
        t =
            Variable "t"

        var v =
            plus [ Variable <| "o." ++ v, by [ t, Variable <| "d" ++ v ] ]

        repls =
            [ "x", "y", "z" ]
                |> List.map (\c -> ( c, var c ))
                |> Dict.fromList

        replaced =
            Expression.fullSubstitute repls e

        poly =
            (case replaced of
                RelationOperation Equals l r ->
                    minus l r

                _ ->
                    replaced
            )
                |> asPolynomial [ "t" ]
                |> Maybe.map Dict.toList
                |> Maybe.andThen
                    (Maybe.traverse
                        (\p ->
                            case p of
                                ( [], k ) ->
                                    Just ( 0, k )

                                ( [ ( v, d ) ], k ) ->
                                    if v == "t" then
                                        Just ( d, k )

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                        )
                    )
                |> Maybe.map Dict.fromList
                |> Maybe.withDefault Dict.empty
    in
    UI.Glsl.Polynomial.glslFromPolySolutions suffix { base = unknown "max_distance" } poly
        |> Maybe.map
            (\{ funDecls, bisect } ->
                { funDecls = funDecls
                , expr = Expression.Utils.div Expression.Utils.one <| Expression.Utils.sqrt_ <| Expression.Utils.cbrt <| Expression.Utils.ipow e 3
                , bisect = bisect
                }
            )
