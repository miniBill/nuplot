module UI.Glsl exposing (getGlsl)

import Dict
import Expression exposing (Expression(..), RelationOperation(..))
import Expression.Graph exposing (Graph(..))
import Expression.Polynomial exposing (asPolynomial)
import Expression.Utils exposing (by, minus, plus)
import Glsl exposing (BisectSignature, ExprWithDeps)
import Glsl.Generator as Generator
import Maybe.Extra as Maybe
import UI.Glsl.Code exposing (mainGlsl, toSrcVectorField2D)
import UI.Glsl.Plane as Plane
import UI.Glsl.Polynomial
import UI.Glsl.Sphere as Sphere


getGlsl : Bool -> Graph -> String
getGlsl rayDifferentials graph =
    let
        { expr, funDecls, usesThetaDelta, pixel2D, pixel3D } =
            extract "" graph

        build2d :
            (Expression -> ExprWithDeps)
            -> Expression
            ->
                { expr : ExprWithDeps
                , funDecls : a
                , usesThetaDelta : Bool
                , pixel2D : List { call : b, color : Bool }
                , pixel3D : List c
                }
        build2d toSrc e =
            { expr = toSrc e
            , usesThetaDelta = False
            , pixel2D = [ { call = call, color = True } ]
            , pixel3D = []
            }

        extract : String -> Graph -> { pixel2D : List { call : String, color : Bool }, pixel3D : List a }
        extract suffix g =
            case g of
                Explicit2D c ->
                    -- build2d (toSrcImplicit suffix) <|
                    --     minus Expression.Utils.y c
                    Debug.todo "Explicit2D"

                Implicit2D l r ->
                    -- build2d (toSrcImplicit suffix) <|
                    --     minus l r
                    Debug.todo "Implicit2D"

                Polar2D e ->
                    -- build2d (toSrcPolar suffix) e
                    Debug.todo "Polar2D"

                Parametric2D x y ->
                    -- { pixel2D =
                    --     [ { call = toSrcParametric suffix e
                    --       , color = True
                    --       }
                    --     ]
                    -- , pixel3D = []
                    -- }
                    Debug.todo "Parametric2D"

                Relation2D e ->
                    -- build2d (toSrcRelation suffix) e
                    Debug.todo "Relation2D"

                Contour e ->
                    -- { pixel2D = [ { call = toSrcContour suffix e, color = False } ]
                    -- , pixel3D = []
                    -- }
                    Debug.todo "Countour"

                Implicit3D e ->
                    -- let
                    --     f =
                    --         get3DSource suffix e
                    -- in
                    -- { expr = f.expr
                    -- , pixel2D = []
                    -- , pixel3D = [ f.bisect ]
                    -- }
                    Debug.todo "Implicit3D"

                GraphList children ->
                    let
                        suffix_ i =
                            suffix ++ "_" ++ String.fromInt i

                        extracted =
                            List.indexedMap (extract << suffix_) children
                    in
                    { pixel2D = List.concatMap .pixel2D extracted
                    , pixel3D = List.concatMap .pixel3D extracted
                    }

                VectorField2D x y ->
                    { pixel2D = [ { call = toSrcVectorField2D suffix x y, color = False } ]
                    , pixel3D = []
                    }

        reqs =
            expr.deps
    in
    reqs
        ++ "\n/* Expression */\n"
        ++ Generator.fileToGlsl funDecls
        ++ mainGlsl rayDifferentials pixel2D pixel3D


get3DSource : String -> Expression -> BisectSignature
get3DSource suffix e =
    case Sphere.asSphere e |> Maybe.map (Sphere.toGlsl suffix) of
        Just bisect ->
            bisect

        Nothing ->
            case Plane.asPlane e |> Maybe.map (Plane.toGlsl suffix) of
                Just bisect ->
                    bisect

                Nothing ->
                    case tryGlslFromPolynomial suffix e of
                        Just o ->
                            o

                        Nothing ->
                            -- UI.Glsl.Code.suffixToBisect (toSrc3D suffix e) suffix
                            Debug.todo ""


tryGlslFromPolynomial : String -> Expression -> Maybe BisectSignature
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
    UI.Glsl.Polynomial.glslFromPolySolutions suffix (Glsl.var "max_distance") poly
