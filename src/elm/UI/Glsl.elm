module UI.Glsl exposing (getGlsl)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), Graph(..), KnownFunction(..))
import Expression.Utils
import List
import List.Extra as List
import List.MyExtra as List
import SortedAnySet as Set
import UI.Glsl.Code exposing (constantToGlsl, intervalFunctionToGlsl, intervalOperationToGlsl, straightFunctionToGlsl, straightOperationToGlsl, toSrc3D, toSrcContour, toSrcImplicit, toSrcRelation)
import UI.Glsl.Model exposing (GlslConstant(..), GlslFunction(..), GlslOperation(..))


getGlsl : Graph -> String
getGlsl graph =
    let
        { expr, srcExpr, interval, hsl } =
            extract "" graph

        extract prefix g =
            case g of
                Explicit2D c ->
                    let
                        e =
                            Expression.Utils.minus Expression.Utils.y c
                    in
                    { expr = e, srcExpr = toSrcImplicit prefix e, interval = StraightOnly, hsl = False }

                Implicit2D l r ->
                    let
                        e =
                            Expression.Utils.minus l r
                    in
                    { expr = e, srcExpr = toSrcImplicit prefix e, interval = StraightOnly, hsl = False }

                Relation2D e ->
                    { expr = e, srcExpr = toSrcRelation prefix e, interval = StraightOnly, hsl = False }

                Contour e ->
                    { expr = e, srcExpr = toSrcContour prefix e, interval = StraightOnly, hsl = True }

                Implicit3D e ->
                    { expr = e, srcExpr = toSrc3D prefix e, interval = IntervalAndStraight, hsl = True }

                GraphList l ->
                    let
                        prefix_ i =
                            prefix ++ "_" ++ String.fromInt i

                        children =
                            List.indexedMap (extract << prefix_) l

                        addPixel i =
                            "res = max(res, pixel" ++ prefix_ i ++ "(deltaX, deltaY, x, y));"

                        inner =
                            l
                                |> List.indexedMap (\i _ -> addPixel i)
                                |> String.join "\n                                    "

                        src =
                            """
                                vec3 pixel""" ++ prefix ++ """(float deltaX, float deltaY, float x, float y) {
                                    vec3 res = vec3(0,0,0);
                                    """ ++ inner ++ """
                                    return res;
                                }"""
                    in
                    { expr = List <| List.map .expr children
                    , srcExpr = String.join "\n" (List.map .srcExpr children) ++ deindent 8 src
                    , interval =
                        if List.any (\c -> c.interval /= StraightOnly) children then
                            IntervalAndStraight

                        else
                            StraightOnly
                    , hsl = List.any .hsl children
                    }

        hslCode =
            deindent 20 <|
                if hsl then
                    """
                    vec3 hl2rgb(float h, float l)
                    {
                        vec3 rgb = clamp(abs(mod(h*6.0+vec3(0.0,4.0,2.0),6.0)-3.0)-1.0,0.0,1.0);
                        return l + (rgb - 0.5) * (1.0 - abs(2.0 * l - 1.0));
                    }

                    float thetaDelta(float theta) {
                        if(u_whiteLines < 1.0)
                            return 100.0;
                        float thetaSix = theta * u_whiteLines;
                        float thetaNeigh = 0.05;
                        return abs(thetaSix - floor(thetaSix + 0.5)) / thetaNeigh;
                    }
                    """

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
    hslCode ++ reqs ++ "\n/* Expression */" ++ deindent 4 srcExpr


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


deindent : Int -> String -> String
deindent i =
    String.split "\n"
        >> List.map (String.dropLeft i)
        >> String.join "\n"


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
            List.concatMap expressionToRequirements [ l, r ]

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


dependenciesOf : Requirement -> List Requirement
dependenciesOf req =
    case req of
        RequireFunction Sin22 ->
            List.map RequireFunction [ Sinh11, Cosh11 ]

        RequireFunction Cos22 ->
            List.map RequireFunction [ Sinh11, Cosh11 ]

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
            [ RequireFunction Exp22 ]

        RequireFunction Cosh22 ->
            [ RequireFunction Exp22 ]

        RequireFunction Tanh22 ->
            [ RequireFunction Exp22, RequireOperation GlslDivision ]

        RequireFunction Abs22 ->
            []

        RequireFunction Sqrt22 ->
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

        RequireOperation GlslPower ->
            RequireOperation GlslMultiplication :: List.map RequireFunction [ Exp22, Ln22 ]

        RequireOperation GlslAddition ->
            []

        RequireOperation GlslMultiplication ->
            []

        RequireOperation GlslDivision ->
            []

        RequireOperation GlslNegation ->
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
