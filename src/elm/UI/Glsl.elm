module UI.Glsl exposing (getGlsl)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), RelationOperation(..))
import Expression.Graph exposing (Graph(..))
import Expression.Polynomial exposing (asPolynomial)
import Expression.Utils exposing (by, minus, plus, square)
import Maybe.Extra as Maybe
import SortedAnySet as Set
import UI.Glsl.Code exposing (atanPlusDecl, constantToGlsl, dupDecl, gnumDecl, intervalFunctionToGlsl, intervalOperationToGlsl, mainGlsl, straightFunctionToGlsl, straightOperationToGlsl, toSrc3D, toSrcContour, toSrcImplicit, toSrcParametric, toSrcPolar, toSrcRelation, toSrcVectorField2D)
import UI.Glsl.Generator as Generator exposing (Expression1, ExpressionX, FunDecl, Mat3, Vec3, expr, fileToGlsl, unknown)
import UI.Glsl.Model exposing (GlslConstant(..), GlslFunction(..), GlslOperation(..))
import UI.Glsl.Plane as Plane
import UI.Glsl.Polynomial
import UI.Glsl.Sphere as Sphere


getGlsl : Bool -> Graph -> String
getGlsl rayDifferentials graph =
    let
        { expr, funDecls, interval, usesThetaDelta, pixel2D, pixel3D } =
            extract "" graph

        build2d toSrc e =
            let
                ( decl, call ) =
                    toSrc e
            in
            { expr = e
            , funDecls = decl
            , interval = StraightOnly
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
                    let
                        ( src, call ) =
                            toSrcParametric prefix e

                        e =
                            plus
                                [ square (minus (Variable "x") x)
                                , square (minus (Variable "y") y)
                                ]
                    in
                    { expr = e
                    , funDecls = src
                    , interval = IntervalOnly
                    , usesThetaDelta = False
                    , pixel2D = [ { call = call, color = True } ]
                    , pixel3D = []
                    }

                Relation2D e ->
                    build2d (toSrcRelation prefix) e

                Contour e ->
                    let
                        ( src, pixel ) =
                            toSrcContour prefix e
                    in
                    { expr = e
                    , funDecls = src
                    , interval = StraightOnly
                    , usesThetaDelta = True
                    , pixel2D = [ { call = pixel, color = False } ]
                    , pixel3D = []
                    }

                Implicit3D e ->
                    let
                        f =
                            get3DSource prefix e
                    in
                    { expr = f.expr
                    , funDecls = f.funDecls
                    , interval = IntervalAndStraight
                    , usesThetaDelta = True
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
                    { expr = List <| List.map .expr extracted
                    , funDecls = List.concatMap .funDecls extracted
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
                    let
                        ( src, pixel ) =
                            toSrcVectorField2D prefix x y
                    in
                    { expr = Apply (KnownFunction Arg) [ x, y ]
                    , funDecls = src
                    , interval = StraightOnly
                    , usesThetaDelta = True
                    , pixel2D = [ { call = pixel, color = False } ]
                    , pixel3D = []
                    }

        thetaDeltaCode =
            if usesThetaDelta then
                Generator.fileToGlsl [ UI.Glsl.Code.thetaDeltaDecl ]

            else
                ""

        reqs =
            let
                _ =
                    -- Can remove in the future
                    Debug.todo
            in
            expr
                |> expressionToRequirements
                |> transitiveClosure
                |> List.sortWith requirementSort
                |> List.map (requirementToGlsl interval)
                |> String.join "\n"
    in
    declarations
        ++ thetaDeltaCode
        ++ reqs
        ++ "\n/* Expression */\n"
        ++ Generator.fileToGlsl funDecls
        ++ mainGlsl rayDifferentials pixel2D pixel3D


declarations : String
declarations =
    [ atanPlusDecl, gnumDecl, dupDecl ]
        |> Generator.fileToGlsl


get3DSource :
    String
    -> Expression
    ->
        { expr : Expression
        , funDecls : List FunDecl
        , bisect :
            ExpressionX xa Vec3
            -> ExpressionX xb Mat3
            -> ExpressionX xc Float
            -> ExpressionX xd Vec3
            -> Expression1 Bool
        }
get3DSource prefix e =
    case Sphere.asSphere e |> Maybe.map (Sphere.toGlsl prefix) of
        Just ( glsl, bisect ) ->
            { expr = e
            , funDecls = [ glsl ]
            , bisect = bisect
            }

        Nothing ->
            case Plane.asPlane e |> Maybe.map (Plane.toGlsl prefix) of
                Just ( glsl, bisect ) ->
                    { expr = e
                    , funDecls = [ glsl ]
                    , bisect = bisect
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
                ExpressionX xa Vec3
                -> ExpressionX xb Mat3
                -> ExpressionX xc Float
                -> ExpressionX xd Vec3
                -> Expression1 Bool
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
            constantToGlsl c
                |> Tuple.first
                |> (\l -> [ l ])
                |> fileToGlsl

        RequireOperation o ->
            operationToGlsl i o |> fileToGlsl


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


operationToGlsl : RequiresInterval -> GlslOperation -> List FunDecl
operationToGlsl interval op =
    case interval of
        StraightOnly ->
            straightOperationToGlsl op

        IntervalOnly ->
            intervalOperationToGlsl op

        IntervalAndStraight ->
            straightOperationToGlsl op
                ++ intervalOperationToGlsl op


functionToGlsl : RequiresInterval -> GlslFunction -> String
functionToGlsl interval name =
    case interval of
        StraightOnly ->
            Generator.fileToGlsl <| straightFunctionToGlsl name

        IntervalOnly ->
            intervalFunctionToGlsl name

        IntervalAndStraight ->
            Generator.fileToGlsl (straightFunctionToGlsl name)
                ++ "\n"
                ++ intervalFunctionToGlsl name
