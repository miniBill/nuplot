module UI.Glsl exposing (getGlsl)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), Graph(..), KnownFunction(..))
import Expression.Utils
import List
import List.Extra as List
import List.MyExtra as List
import SortedAnySet as Set exposing (SortedAnySet)


getGlsl : Graph -> String
getGlsl graph =
    let
        ( expr, srcExpr, interval ) =
            extract "" graph

        extract prefix g =
            case g of
                Explicit2D c ->
                    let
                        e =
                            Expression.Utils.minus Expression.Utils.y c
                    in
                    ( e, toSrcImplicit prefix e, StraightOnly )

                Implicit2D l r ->
                    let
                        e =
                            Expression.Utils.minus l r
                    in
                    ( e, toSrcImplicit prefix e, StraightOnly )

                Relation2D e ->
                    ( e, toSrcRelation prefix e, StraightOnly )

                Contour e ->
                    ( e, toSrcContour prefix e, StraightOnly )

                Implicit3D e ->
                    ( e, toSrc3D prefix e, IntervalAndStraight )

                GraphList l ->
                    let
                        ( es, srcs, intervals ) =
                            l
                                |> List.indexedMap (\i -> extract (prefix ++ "_" ++ String.fromInt i))
                                |> List.unzip3

                        addPixel i =
                            "res = max(res, pixel" ++ prefix ++ "_" ++ String.fromInt i ++ "(deltaX, deltaY, x, y));"

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
                    ( List es
                    , String.join "\n" srcs ++ deindent 8 src
                    , if List.any ((/=) StraightOnly) intervals then
                        IntervalAndStraight

                      else
                        StraightOnly
                    )

        reqs =
            expr
                |> expressionToRequirements
                |> transitiveClosure
                |> List.sortWith requirementSort
                |> List.map (requirementToGlsl interval)
                |> String.join "\n"
    in
    reqs ++ "\n/* Expression */" ++ deindent 4 srcExpr


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


type GlslFunction
    = Abs22
    | Acos22
    | Arg22
    | Asin22
    | Atan22
    | Atan222
    | Cos22
    | Cosh11
    | Cosh22
    | Exp22
    | Im22
    | Ln22
    | Log1022
    | Pw22
    | Re22
    | Sin22
    | Sinh11
    | Sinh22
    | Sqrt22
    | Tan22
    | Tanh11
    | Tanh22


type GlslConstant
    = E
    | Pi
    | I


type RequiresInterval
    = IntervalOnly
    | StraightOnly
    | IntervalAndStraight


type GlslOperation
    = GlslAddition
    | GlslMultiplication
    | GlslNegation
    | GlslDivision
    | GlslPower


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
            List.map RequireFunction [ Sqrt22, Ln22 ]

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
            List.map RequireFunction [ Exp22, Ln22 ]

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

        _ ->
            Debug.todo "operationToGlsl"


straightOperationToGlsl : GlslOperation -> String
straightOperationToGlsl op =
    case op of
        GlslAddition ->
            ""

        GlslMultiplication ->
            """
            vec2 by(vec2 a, vec2 b) {
                return vec2(a.x*b.x-a.y*b.y, a.x*b.y+a.y*b.x);
            }
            """

        GlslNegation ->
            ""

        GlslDivision ->
            """
            vec2 div(vec2 a, vec2 b) {
                float k = 1.0 / dot(b, b);
                float r = k * dot(a, b);
                float i = k * (a.y*b.x - a.x*b.y);
                return vec2(r, i);
            }
            """

        GlslPower ->
            """
            vec2 cpow(vec2 w, vec2 z) {
                if(w.y == 0.0 && z.y == 0.0) {
                    return vec2(pow(w.x, z.x), 0);
                }
                return cexp(by(w, clog(z)));
            }
            """


constantToGlsl : GlslConstant -> String
constantToGlsl c =
    case c of
        I ->
            """
            vec2 i() {
                return vec2(0,1);
            }
            """

        Pi ->
            """
            vec2 pi() {
                return vec2(radians(180.0), 0.0);
            }
            """

        E ->
            """
            vec2 e() {
                return vec2(exp(1.0), 0.0);
            }
            """


functionToGlsl : RequiresInterval -> GlslFunction -> String
functionToGlsl interval name =
    case interval of
        StraightOnly ->
            deindent 12 <| straightFunctionToGlsl name

        _ ->
            Debug.todo "functionToGlsl"


straightFunctionToGlsl : GlslFunction -> String
straightFunctionToGlsl name =
    case name of
        Sinh11 ->
            """
            float sinh(float x) {
                return 0.5 * (exp(x) - exp(-x));
            }
            """

        Cosh11 ->
            """
            float cosh(float x) {
                return 0.5 * (exp(x) + exp(-x));
            }
            """

        Tanh11 ->
            """
            float tanh(float x) {
                float p = exp(x);
                float m = exp(-x);
                return (p - m) / (p + m);
            }
            """

        Sin22 ->
            """
            vec2 csin(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(sin(z.x), 0);
                }
                return vec2(sin(z.x) * cosh(z.y), cos(z.x) * sinh(z.y));
            }
            """

        Cos22 ->
            """
            vec2 ccos(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(cos(z.x), 0);
                }
                return vec2(cos(z.x) * cosh(z.y), sin(z.x) * sinh(z.y));
            }
            """

        Tan22 ->
            """
            vec2 ctan(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(tan(z.x), 0);
                }
                return div(csin(z), ccos(z));
            }
            """

        Asin22 ->
            """
            vec2 casin(vec2 z) {
                vec2 s = csqrt(vec2(1, 0) - by(z, z));
                vec2 arg = s - by(i(), z);
                return by(i(), cln(arg));
            }
            """

        Acos22 ->
            """
            vec2 cacos(vec2 z) {
                return pi() * 0.5 - casin(z);
            }
            """

        Atan22 ->
            """
            vec2 catan(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(atan(z.x), 0);
                }
                vec2 o = vec2(1, 0);
                vec2 iz = by(i(), z);
                vec2 l = div(o + iz, o - iz);
                return -0.5 * by(i(), cln(l));
            }
            """

        Atan222 ->
            """
            vec2 catan2(vec2 y, vec2 x) {
                vec2 z = vec2(x.x - y.y, x.y + y.x);
                return vec2(atan(z.y, z.x), 0.0);
            }
            """

        Sinh22 ->
            """
            vec2 csinh(vec2 z) {
                return 0.5 * (cexp(z) - cexp(-z));
            }
            """

        Cosh22 ->
            """
            vec2 ccosh(vec2 z) {
                return 0.5 * (cexp(z) + cexp(-z));
            }
            """

        Tanh22 ->
            """
            vec2 ctanh(vec2 z) {
                vec2 p = cexp(z);
                vec2 m = cexp(-z);
                return div(p - m, p + m);
            }
            """

        Abs22 ->
            """
            vec2 cabs(vec2 z) {
                return vec2(length(z), 0.0);
            }
            """

        Sqrt22 ->
            """
            vec2 csqrt(vec2 z) {
                if(z.y == 0.0 && z.x >= 0.0) {
                    return vec2(sqrt(z.x), 0);
                }
                float r = pow(dot(z, z), 0.25);
                float t = atan(z.y, z.x) * 0.5;
                return vec2(r * cos(t), r * sin(t));
            }
            """

        Ln22 ->
            """
            vec2 cln(vec2 z) {
                if(z.y == 0.0 && z.x >= 0.0) {
                    return vec2(log(z.x), 0);
                }
                float px = length(z);
                float py = atan(z.y, z.x);
                return vec2(log(px), py);
            }
            """

        Log1022 ->
            """
            vec2 clog10(vec2 z) {
                return div(cln(z), vec2(log(10.0), 0));
            }
            """

        Exp22 ->
            """
            vec2 cexp(vec2 z) {
                if(z.y == 0.0) {
                    return vec2(exp(z.x), 0);
                }
                return vec2(cos(z.y) * exp(z.x), sin(z.y) * exp(z.x));
            }
            """

        Re22 ->
            """
            vec2 cre(vec2 z) {
                return vec2(z.x, 0.0);
            }
            """

        Im22 ->
            """
            vec2 cim(vec2 z) {
                return vec2(z.y, 0.0);
            }
            """

        Arg22 ->
            """
            vec2 carg(vec2 v) {
                return vec2(atan(v.y, v.x), 0);
            }
            """

        Pw22 ->
            """
            vec2 cpw(vec2 c, vec2 t, vec2 f) {
                return c.x > 0.0 ? t : f;
            }
            """


epsilon : Float
epsilon =
    0.00001


toSrcImplicit : String -> Expression -> String
toSrcImplicit suffix e =
    """
    float f""" ++ suffix ++ """(float x, float y) {
        vec2 complex = """ ++ Expression.toGLString e ++ """;
        if(abs(complex.y) > """ ++ String.fromFloat epsilon ++ """) {
            return -1.0;
        }
        return complex.x > 0.0 ? 1.0 : 0.0;
    }

    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        float h = f""" ++ suffix ++ """(x,y);
        float l = f""" ++ suffix ++ """(x - deltaX,y);
        float ul = f""" ++ suffix ++ """(x - deltaX,y - deltaY);
        float u = f""" ++ suffix ++ """(x,y - deltaY);
        return (h != l || h != u || h != ul) && (h >= 0.0 && l >= 0.0 && ul >= 0.0 && u >= 0.0) ? vec3(1,1,1) : vec3(0,0,0);
    }
    """


toSrcRelation : String -> Expression -> String
toSrcRelation suffix e =
    """
    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        vec2 complex = """ ++ Expression.toGLString e ++ """;
        return complex.x > 0.0 && abs(complex.y) < """ ++ String.fromFloat epsilon ++ """ ? vec3(0.8,0.5,0.5) : vec3(0,0,0);
    }
    """


toSrcContour : String -> Expression -> String
toSrcContour suffix e =
    """
    vec3 pixel""" ++ suffix ++ """_o(float deltaX, float deltaY, float x, float y) {
        vec2 z = """ ++ Expression.toGLString e ++ """;

        float theta = atan(z.y, z.x) / radians(360.0);
        float td = thetaDelta(theta);

        float radius = sqrt(z.x*z.x + z.y*z.y);
        float logRadius = log2(radius);
        float powerRemainder = logRadius - floor(logRadius);
        float squished = 0.7 - powerRemainder * 0.4;

        if(u_completelyReal > 0.0) {
            return hl2rgb(theta, squished);
        }

        float l = td < 1.0 ? squished * td + (1.0 - td) : squished;
        return hl2rgb(theta, l);
    }

    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        if(1 > 0)
            return pixel""" ++ suffix ++ """_o(deltaX, deltaY, x, y);

        // Antialiasing

        float dist = 1.0 / 3.0;
        vec3 a = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x + dist * deltaX, y + dist * deltaY);
        vec3 b = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x - dist * deltaX, y - dist * deltaY);
        vec3 c = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x + dist * deltaX, y - dist * deltaY);
        vec3 d = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x - dist * deltaX, y + dist * deltaY);

        vec3 diff = abs(max(a, max(b, max(c, d))) - min(a, min(b, min(c, d))));
        if (0 > 0 && diff.x < dist && diff.y < dist && diff.z < dist)
            return (a + b + c + d) / 4.0;

        vec3 e = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x + 2.0 * dist * deltaX, y);
        vec3 f = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x - 2.0 * dist * deltaX, y);
        vec3 g = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x, y - 2.0 * dist * deltaY);
        vec3 h = pixel""" ++ suffix ++ """_o(deltaX, deltaY, x, y + 2.0 * dist * deltaY);

        return (a + b + c + d + e + f + g + h) / 8.0;
    }
    """


toSrc3D : String -> Expression -> String
toSrc3D suffix e =
    """
    vec3 pixel""" ++ suffix ++ """_o(float deltaX, float deltaY, float x, float y) {
        vec2 z = """ ++ Expression.toGLString e ++ """;

        float theta = atan(z.y, z.x) / radians(360.0);
        float td = thetaDelta(theta);

        float radius = sqrt(z.x*z.x + z.y*z.y);
        float logRadius = log2(radius);
        float powerRemainder = logRadius - floor(logRadius);
        float squished = 0.7 - powerRemainder * 0.4;

        if(u_completelyReal > 0.0) {
            return hl2rgb(theta, squished);
        }

        float l = td < 1.0 ? squished * td + (1.0 - td) : squished;
        return hl2rgb(theta, l);
    }

    vec3 pixel""" ++ suffix ++ """(float deltaX, float deltaY, float x, float y) {
        return pixel""" ++ suffix ++ """_o(deltaX, deltaY, x, y);
    }
    """
