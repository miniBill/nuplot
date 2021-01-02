module UI.Glsl exposing (getGlsl)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), Graph(..), KnownFunction(..))
import Expression.Utils
import List
import List.Extra as List
import List.MyExtra as List
import SortedAnySet as Set
import UI.Glsl.Code exposing (constantToGlsl, deindent, intervalFunctionToGlsl, intervalOperationToGlsl, mainGlsl, straightFunctionToGlsl, straightOperationToGlsl, toSrc3D, toSrcContour, toSrcImplicit, toSrcPolar, toSrcRelation)
import UI.Glsl.Model exposing (GlslConstant(..), GlslFunction(..), GlslOperation(..))


getGlsl : Graph -> String
getGlsl graph =
    let
        { expr, srcExpr, interval, thetaDelta, pixel2D, pixel3D } =
            extract "" graph

        build2d toSrc prefix e =
            { expr = e
            , srcExpr = toSrc prefix e
            , interval = StraightOnly
            , thetaDelta = False
            , pixel2D = [ { name = "pixel" ++ prefix, color = True } ]
            , pixel3D = []
            }

        extract prefix g =
            case g of
                Explicit2D c ->
                    build2d toSrcImplicit prefix <|
                        Expression.Utils.minus Expression.Utils.y c

                Implicit2D l r ->
                    build2d toSrcImplicit prefix <|
                        Expression.Utils.minus l r

                Polar2D e ->
                    build2d toSrcPolar prefix e

                Relation2D e ->
                    build2d toSrcRelation prefix e

                Contour e ->
                    { expr = e
                    , srcExpr = toSrcContour prefix e
                    , interval = StraightOnly
                    , thetaDelta = True
                    , pixel2D = [ { name = "pixel" ++ prefix, color = False } ]
                    , pixel3D = []
                    }

                Implicit3D e ->
                    { expr = e
                    , srcExpr = "BORK" ++ toSrc3D prefix e
                    , interval = IntervalAndStraight
                    , thetaDelta = True
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
                    , thetaDelta = List.any .thetaDelta extracted
                    , pixel2D = List.concatMap .pixel2D extracted
                    , pixel3D = List.concatMap .pixel3D extracted
                    }

        thetaDeltaCode =
            deindent 4 <|
                if thetaDelta then
                    UI.Glsl.Code.thetaDelta

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
    thetaDeltaCode ++ reqs ++ "\n/* Expression */" ++ deindent 4 srcExpr ++ mainGlsl pixel2D pixel3D


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

        Sqrt ->
            Just Sqrt22

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

        Simplify ->
            Nothing

        Solve ->
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
            []

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

        RequireFunction Square22 ->
            []

        RequireFunction Min222 ->
            []

        RequireFunction Max222 ->
            []

        RequireFunction Mod22 ->
            []

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
            deindent 12 <| straightOperationToGlsl op

        IntervalOnly ->
            deindent 12 <| intervalOperationToGlsl op

        IntervalAndStraight ->
            deindent 12 <| straightOperationToGlsl op ++ "\n" ++ intervalOperationToGlsl op


functionToGlsl : RequiresInterval -> GlslFunction -> String
functionToGlsl interval name =
    case interval of
        StraightOnly ->
            deindent 12 <| straightFunctionToGlsl name

        IntervalOnly ->
            deindent 12 <| intervalFunctionToGlsl name

        IntervalAndStraight ->
            deindent 12 <| straightFunctionToGlsl name ++ "\n" ++ intervalFunctionToGlsl name
