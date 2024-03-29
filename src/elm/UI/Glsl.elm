module UI.Glsl exposing (getGlsl)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), RelationOperation(..))
import Expression.Graph exposing (Graph(..))
import Expression.Polynomial exposing (asPolynomial)
import Expression.Utils exposing (by, cbrt, div, ipow, minus, one, plus, sqrt_, square)
import Maybe.Extra as Maybe
import SortedAnySet as Set
import UI.Glsl.Code exposing (constantToGlsl, deindent, intervalFunctionToGlsl, intervalOperationToGlsl, mainGlsl, straightFunctionToGlsl, straightOperationToGlsl, toSrc3D, toSrcContour, toSrcImplicit, toSrcParametric, toSrcPolar, toSrcRelation, toSrcVectorField2D)
import UI.Glsl.Generator as Generator exposing (floatT, uniform, unknownTypedName, vec2T)
import UI.Glsl.Model exposing (GlslConstant(..), GlslFunction(..), GlslOperation(..))
import UI.Glsl.Plane as Plane
import UI.Glsl.Polynomial
import UI.Glsl.Sphere as Sphere


getGlsl : Bool -> Bool -> Graph -> String
getGlsl expandIntervals rayDifferentials graph =
    let
        uniforms =
            { u_canvasWidth = uniform floatT "u_canvasWidth"
            , u_canvasHeight = uniform floatT "u_canvasHeight"
            , u_viewportWidth = uniform floatT "u_viewportWidth"
            , u_viewportHeight = uniform floatT "u_viewportHeight"
            , u_zoomCenter = uniform vec2T "u_zoomCenter"
            , u_drawAxes = uniform floatT "u_drawAxes"
            }

        { expr, srcExpr, interval, usesThetaDelta, pixel2D, pixel3D } =
            extract "" graph

        build2d toSrc prefix e =
            { expr = e
            , srcExpr = toSrc prefix e
            , interval = StraightOnly
            , usesThetaDelta = False
            , pixel2D = [ { name = "pixel" ++ prefix, color = True } ]
            , pixel3D = []
            }

        extract prefix g =
            case g of
                Explicit2D c ->
                    build2d toSrcImplicit prefix <|
                        minus Expression.Utils.y c

                Implicit2D l r ->
                    build2d toSrcImplicit prefix <|
                        minus l r

                Polar2D e ->
                    build2d toSrcPolar prefix e

                Parametric2D x y ->
                    let
                        e =
                            plus
                                [ square (minus (Variable "x") x)
                                , square (minus (Variable "y") y)
                                ]
                    in
                    { expr = e
                    , srcExpr = toSrcParametric expandIntervals prefix e
                    , interval = IntervalOnly
                    , usesThetaDelta = False
                    , pixel2D = [ { name = "pixel" ++ prefix, color = True } ]
                    , pixel3D = []
                    }

                Relation2D e ->
                    build2d toSrcRelation prefix e

                Contour e ->
                    { expr = e
                    , srcExpr = toSrcContour prefix e
                    , interval = StraightOnly
                    , usesThetaDelta = True
                    , pixel2D = [ { name = "pixel" ++ prefix, color = False } ]
                    , pixel3D = []
                    }

                Implicit3D e ->
                    let
                        f =
                            get3DSource expandIntervals prefix e
                    in
                    { expr = f.expr
                    , srcExpr = f.srcExpr
                    , interval = IntervalAndStraight
                    , usesThetaDelta = True
                    , pixel2D = []
                    , pixel3D = [ prefix ]
                    }

                GraphList children ->
                    let
                        prefix_ i =
                            prefix ++ "_" ++ String.fromInt i

                        extracted =
                            List.indexedMap (extract << prefix_) children
                    in
                    { expr = List <| List.map .expr extracted
                    , srcExpr = String.join "\n" (List.map .srcExpr extracted)
                    , interval =
                        if List.any (\c -> c.interval /= StraightOnly) extracted then
                            IntervalAndStraight

                        else
                            StraightOnly
                    , usesThetaDelta = List.any .usesThetaDelta extracted
                    , pixel2D = List.concatMap .pixel2D extracted
                    , pixel3D = List.concatMap .pixel3D extracted
                    }

                VectorField2D x y ->
                    { expr = Apply (KnownFunction Arg) [ x, y ]
                    , srcExpr = toSrcVectorField2D prefix x y
                    , interval = StraightOnly
                    , usesThetaDelta = True
                    , pixel2D = [ { name = "pixel" ++ prefix, color = False } ]
                    , pixel3D = []
                    }

        thetaDeltaCode =
            if usesThetaDelta then
                deindent 4 <| UI.Glsl.Code.thetaDelta

            else
                ""

        reqs =
            expr
                |> expressionToRequirements
                |> transitiveClosure
                |> List.sortWith requirementSort
                |> List.map (requirementToGlsl interval)
                |> String.join "\n"
    in
    thetaDeltaCode
        ++ reqs
        ++ "\n/* Expression */\n"
        ++ deindent 4 srcExpr
        ++ mainGlsl
            uniforms
            rayDifferentials
            (List.map
                (\p ->
                    { name = unknownTypedName p.name
                    , color = p.color
                    }
                )
                pixel2D
            )
            pixel3D


get3DSource : Bool -> String -> Expression -> { expr : Expression, srcExpr : String }
get3DSource expandIntervals prefix e =
    case Sphere.asSphere e |> Maybe.map (Sphere.toGlsl prefix) of
        Just glsl ->
            { expr = e
            , srcExpr = glsl
            }

        Nothing ->
            case Plane.asPlane e |> Maybe.map (Plane.toGlsl prefix) of
                Just glsl ->
                    { expr = e
                    , srcExpr = glsl
                    }

                Nothing ->
                    case tryGlslFromPolynomial prefix e of
                        Just o ->
                            o

                        Nothing ->
                            let
                                glsl =
                                    toSrc3D expandIntervals prefix e
                                        |> Generator.fileToGlsl
                            in
                            { expr = e
                            , srcExpr = glsl ++ UI.Glsl.Code.suffixToBisect prefix
                            }


tryGlslFromPolynomial : String -> Expression -> Maybe { expr : Expression, srcExpr : String }
tryGlslFromPolynomial prefix e =
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

        fromSols sols =
            let
                checks =
                    sols
                        |> List.foldl
                            (\( k, v ) ( known, lines ) ->
                                let
                                    ( decl, drop ) =
                                        if List.member k known then
                                            ( "", 0 )

                                        else if String.startsWith "!" k then
                                            ( "float ", 1 )

                                        else
                                            ( "vec2 ", 0 )

                                    newLine =
                                        decl ++ String.dropLeft drop k ++ " = " ++ v ++ ";"
                                in
                                ( k :: known, newLine :: lines )
                            )
                            ( [ "t" ], [] )
                        |> Tuple.second
                        |> List.reverse
                        |> String.join "\n                        "

                srcExpr =
                    """
                    bool bisect""" ++ prefix ++ """(vec3 o, mat3 d, float max_distance, out vec3 found) {
                        float t = max_distance * 2.0;
                        """ ++ checks ++ """
                        found = o + t * mix(d[0], d[1], 0.5);
                        return t < max_distance && t > 0.0;
                    }
                    """
            in
            { expr = div one <| sqrt_ <| cbrt <| ipow e 3
            , srcExpr = deindent 16 srcExpr
            }
    in
    UI.Glsl.Polynomial.getSolutions poly |> Maybe.map (\( _, sols ) -> fromSols sols)


transitiveClosure : List Requirement -> List Requirement
transitiveClosure =
    let
        go set ls =
            case ls of
                [] ->
                    Set.toList set

                x :: xs ->
                    if Set.member x set then
                        go set xs

                    else
                        let
                            deps =
                                dependenciesOf x
                        in
                        if List.all (\d -> Set.member d set) deps then
                            go (Set.insert x set) xs

                        else
                            go set (deps ++ ls)
    in
    go Set.empty


requirementSort : Requirement -> Requirement -> Order
requirementSort l r =
    if l |> dependsOn r then
        GT

    else if r |> dependsOn l then
        LT

    else
        EQ


dependsOn : Requirement -> Requirement -> Bool
dependsOn r l =
    let
        d =
            dependenciesOf l
    in
    List.member r d || List.any (dependsOn r) d


type Requirement
    = RequireFunction GlslFunction
    | RequireConstant GlslConstant
    | RequireOperation GlslOperation


type RequiresInterval
    = IntervalOnly
    | StraightOnly
    | IntervalAndStraight


requirementToGlsl : RequiresInterval -> Requirement -> String
requirementToGlsl i r =
    case r of
        RequireFunction f ->
            functionToGlsl i f

        RequireConstant c ->
            constantToGlsl c |> Generator.funDeclToGlsl

        RequireOperation o ->
            operationToGlsl i o


expressionToRequirements : Expression -> List Requirement
expressionToRequirements e =
    case e of
        Apply (KnownFunction n) args ->
            case toGlslFunction n of
                Just gf ->
                    RequireFunction gf :: List.concatMap expressionToRequirements args

                Nothing ->
                    List.concatMap expressionToRequirements args

        Apply (UserFunction _) args ->
            List.concatMap expressionToRequirements args

        Integer _ ->
            []

        Float _ ->
            []

        Variable _ ->
            []

        UnaryOperation _ c ->
            RequireOperation GlslNegation :: expressionToRequirements c

        BinaryOperation Power (Variable v) (Integer _) ->
            if v == "i" then
                [ RequireOperation GlslPower ]

            else
                [ RequireOperation GlslPower, RequireOperation GlslMultiplication, RequireFunction Square22 ]

        BinaryOperation Power b (Integer _) ->
            [ RequireOperation GlslPower, RequireOperation GlslMultiplication, RequireFunction Square22 ] ++ expressionToRequirements b

        BinaryOperation Power b (Float _) ->
            [ RequireOperation GlslPower, RequireOperation GlslMultiplication, RequireFunction Square22 ] ++ expressionToRequirements b

        BinaryOperation op l r ->
            let
                op_ =
                    case op of
                        Power ->
                            GlslPower

                        Division ->
                            GlslDivision
            in
            RequireOperation op_ :: List.concatMap expressionToRequirements [ l, r ]

        RelationOperation _ l r ->
            RequireOperation GlslRelations :: List.concatMap expressionToRequirements [ l, r ]

        AssociativeOperation op l m r ->
            let
                op_ =
                    case op of
                        Addition ->
                            GlslAddition

                        Multiplication ->
                            GlslMultiplication
            in
            RequireOperation op_ :: List.concatMap expressionToRequirements (l :: m :: r)

        Replace ctx c ->
            List.concatMap expressionToRequirements <| c :: List.filterMap identity (Dict.values ctx)

        List args ->
            List.concatMap expressionToRequirements args

        Lambda _ f ->
            expressionToRequirements f


toGlslFunction : KnownFunction -> Maybe GlslFunction
toGlslFunction name =
    case name of
        Sin ->
            Just Sin22

        Cos ->
            Just Cos22

        Tan ->
            Just Tan22

        Asin ->
            Just Asin22

        Acos ->
            Just Acos22

        Atan ->
            Just Atan22

        Atan2 ->
            Just Atan222

        Sinh ->
            Just Sinh22

        Cosh ->
            Just Cosh22

        Tanh ->
            Just Tanh22

        Abs ->
            Just Abs22

        Sign ->
            Just Sign22

        Root 2 ->
            Just Sqrt22

        Root 3 ->
            Just Cbrt22

        Root _ ->
            Nothing

        Ln ->
            Just Ln22

        Log10 ->
            Just Log1022

        Exp ->
            Just Exp22

        Re ->
            Just Re22

        Im ->
            Just Im22

        Arg ->
            Just Arg22

        Gra ->
            Nothing

        Pw ->
            Just Pw22

        Floor ->
            Just Floor22

        Ceiling ->
            Just Ceiling22

        Round ->
            Just Round22

        Min ->
            Just Min222

        Max ->
            Just Max222

        Mod ->
            Just Mod22

        Det ->
            Nothing

        Dd ->
            Nothing

        Ii ->
            Nothing

        Plot ->
            Nothing

        APlot ->
            Nothing

        Simplify ->
            Nothing

        StepSimplify ->
            Nothing

        Solve ->
            Nothing

        Mbrot ->
            Just Mbrot22

        For ->
            Nothing


dependenciesOf : Requirement -> List Requirement
dependenciesOf req =
    case req of
        RequireFunction Sin22 ->
            List.map RequireFunction [ Sinh11, Cosh11 ]

        RequireFunction Cos22 ->
            List.map RequireFunction [ Sin22, Sinh11, Cosh11 ]

        RequireFunction Tan22 ->
            RequireOperation GlslDivision :: List.map RequireFunction [ Sin22, Cos22 ]

        RequireFunction Asin22 ->
            RequireOperation GlslMultiplication :: List.map RequireFunction [ Sqrt22, Ln22 ]

        RequireFunction Acos22 ->
            [ RequireConstant Pi, RequireFunction Asin22 ]

        RequireFunction Atan22 ->
            [ RequireConstant I, RequireOperation GlslMultiplication, RequireOperation GlslDivision, RequireFunction Ln22 ]

        RequireFunction Atan222 ->
            [ RequireFunction Atan22, RequireOperation GlslMultiplication, RequireFunction Sign22 ]

        RequireFunction Sinh22 ->
            [ RequireFunction Exp22, RequireFunction Sinh11 ]

        RequireFunction Cosh22 ->
            [ RequireFunction Exp22 ]

        RequireFunction Tanh22 ->
            [ RequireFunction Cosh11, RequireFunction Tanh11, RequireFunction Exp22, RequireOperation GlslDivision ]

        RequireFunction Abs22 ->
            []

        RequireFunction Sign22 ->
            []

        RequireFunction Sqrt22 ->
            []

        RequireFunction Cbrt22 ->
            [ RequireOperation GlslPower ]

        RequireFunction Square22 ->
            []

        RequireFunction Min222 ->
            []

        RequireFunction Max222 ->
            []

        RequireFunction Mod22 ->
            [ RequireOperation GlslDivision, RequireOperation GlslMultiplication, RequireFunction Floor22, RequireOperation GlslNegation ]

        RequireFunction Ln22 ->
            []

        RequireFunction Log1022 ->
            [ RequireFunction Ln22, RequireOperation GlslDivision ]

        RequireFunction Exp22 ->
            []

        RequireFunction Re22 ->
            []

        RequireFunction Im22 ->
            []

        RequireFunction Arg22 ->
            []

        RequireFunction Pw22 ->
            []

        RequireFunction Sinh11 ->
            []

        RequireFunction Cosh11 ->
            []

        RequireFunction Tanh11 ->
            []

        RequireFunction Ceiling22 ->
            []

        RequireFunction Floor22 ->
            []

        RequireFunction Round22 ->
            []

        RequireFunction Mbrot22 ->
            [ RequireOperation GlslMultiplication ]

        RequireOperation GlslPower ->
            RequireOperation GlslMultiplication :: List.map RequireFunction [ Exp22, Ln22 ]

        RequireOperation GlslAddition ->
            []

        RequireOperation GlslMultiplication ->
            []

        RequireOperation GlslDivision ->
            [ RequireOperation GlslMultiplication ]

        RequireOperation GlslNegation ->
            []

        RequireOperation GlslRelations ->
            []

        RequireConstant _ ->
            []


operationToGlsl : RequiresInterval -> GlslOperation -> String
operationToGlsl interval op =
    case interval of
        StraightOnly ->
            Generator.fileToGlsl <| straightOperationToGlsl op

        IntervalOnly ->
            deindent 12 <| intervalOperationToGlsl op

        IntervalAndStraight ->
            (Generator.fileToGlsl <| straightOperationToGlsl op)
                ++ "\n"
                ++ deindent 12 (intervalOperationToGlsl op)


functionToGlsl : RequiresInterval -> GlslFunction -> String
functionToGlsl interval name =
    case interval of
        StraightOnly ->
            Generator.fileToGlsl <| straightFunctionToGlsl name

        IntervalOnly ->
            deindent 12 <| intervalFunctionToGlsl name

        IntervalAndStraight ->
            Generator.fileToGlsl (straightFunctionToGlsl name)
                ++ "\n"
                ++ deindent 12 (intervalFunctionToGlsl name)
