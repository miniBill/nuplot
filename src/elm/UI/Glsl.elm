module UI.Glsl exposing (getGlsl)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), Graph(..), KnownFunction(..), toGLString)
import Expression.Utils
import List.Extra as List


getGlsl : Graph -> String
getGlsl graph =
    let
        ( utils, srcExpr ) =
            case graph of
                Explicit2D f ->
                    let
                        e =
                            Expression.Utils.minus Expression.Utils.y f
                    in
                    ( getFunctionsGlsl e, toSrcImplicit e )

                Implicit2D l r ->
                    let
                        e =
                            Expression.Utils.minus l r
                    in
                    ( getFunctionsGlsl e, toSrcImplicit e )

                Relation2D op l r ->
                    let
                        e =
                            RelationOperation op l r
                    in
                    ( getFunctionsGlsl e, toSrcRelation e )

                Contour e ->
                    ( getFunctionsGlsl e, toSrcContour e )
    in
    utils ++ "\n/* Expression */" ++ deindent 4 srcExpr


getFunctionsGlsl : Expression -> String
getFunctionsGlsl e =
    let
        { functions, constants, operations } =
            collectRequirements e

        withHeader h l =
            if List.isEmpty l then
                []

            else
                ("/* " ++ h ++ " */") :: l ++ [ "\n\n" ]

        functionsGlsl =
            functions
                |> List.sortBy (KnownFunction >> Expression.functionNameToString)
                |> List.uniqueBy (KnownFunction >> Expression.functionNameToString)
                |> List.map (getFunctionGlsl >> deindent 12)
                |> withHeader "Functions"

        constantsGlsl =
            constants
                |> List.sort
                |> List.unique
                |> List.map (getConstantGlsl >> deindent 12)
                |> withHeader "Constants"

        operationsGlsl =
            operations
                |> List.sortBy operationToString
                |> List.uniqueBy operationToString
                |> List.map (getOperationGlsl >> deindent 12)
                |> withHeader "Basic operations"
    in
    String.concat (constantsGlsl ++ operationsGlsl ++ functionsGlsl)


deindent : Int -> String -> String
deindent i =
    String.split "\n"
        >> List.map (String.dropLeft i)
        >> String.join "\n"


operationToString : Operation -> String
operationToString op =
    case op of
        OpAddition ->
            "+"

        OpMultiplication ->
            "by"

        OpDivision ->
            "div"

        OpPower ->
            "cpow"

        OpSinh ->
            "sinh"

        OpCosh ->
            "cosh"


type alias Requirements =
    { constants : List String
    , functions : List KnownFunction
    , operations : List Operation
    }


emptyRequirements : Requirements
emptyRequirements =
    { constants = []
    , functions = []
    , operations = []
    }


collectRequirements : Expression -> Requirements
collectRequirements e =
    case e of
        Apply (KnownFunction n) args ->
            let
                c =
                    collectRequirementsOnList args

                r =
                    knownFunctionDeps n
            in
            mergeRequirements r c

        Apply (UserFunction _) args ->
            collectRequirementsOnList args

        Integer _ ->
            emptyRequirements

        Float _ ->
            emptyRequirements

        Variable _ ->
            emptyRequirements

        UnaryOperation _ c ->
            collectRequirements c

        BinaryOperation op l r ->
            let
                c =
                    collectRequirementsOnList [ l, r ]

                op_ =
                    case op of
                        Power ->
                            OpPower

                        Division ->
                            OpDivision
            in
            { c | operations = op_ :: c.operations }

        RelationOperation _ l r ->
            collectRequirementsOnList [ l, r ]

        AssociativeOperation op l m r ->
            let
                c =
                    collectRequirementsOnList (l :: m :: r)

                op_ =
                    case op of
                        Addition ->
                            OpAddition

                        Multiplication ->
                            OpMultiplication
            in
            { c | operations = op_ :: c.operations }

        Replace ctx c ->
            collectRequirementsOnList <| c :: Dict.values ctx

        List args ->
            collectRequirementsOnList args


collectRequirementsOnList : List Expression -> Requirements
collectRequirementsOnList =
    List.foldl (collectRequirements >> mergeRequirements) emptyRequirements


mergeRequirements : Requirements -> Requirements -> Requirements
mergeRequirements l r =
    { constants = l.constants ++ r.constants
    , functions = l.functions ++ r.functions
    , operations = l.operations ++ r.operations
    }


knownFunctionDeps : KnownFunction -> Requirements
knownFunctionDeps =
    let
        go name =
            let
                base =
                    case name of
                        Sin ->
                            { emptyRequirements | operations = [ OpSinh, OpCosh ] }

                        Cos ->
                            { emptyRequirements | operations = [ OpSinh, OpCosh ] }

                        Tan ->
                            { constants = []
                            , functions = [ Sin, Cos ]
                            , operations = [ OpDivision ]
                            }

                        Sinh ->
                            { constants = [ "i" ]
                            , functions = [ Sin ]
                            , operations = [ OpMultiplication ]
                            }

                        Cosh ->
                            { constants = [ "i" ]
                            , functions = [ Cos ]
                            , operations = [ OpMultiplication ]
                            }

                        Tanh ->
                            { constants = []
                            , functions = [ Sinh, Cosh ]
                            , operations = [ OpDivision ]
                            }

                        Atan2 ->
                            { emptyRequirements | constants = [ "i" ] }

                        _ ->
                            emptyRequirements
            in
            List.foldl mergeRequirements
                { base | functions = name :: base.functions }
                (List.map go base.functions)
    in
    go


type Operation
    = OpAddition
    | OpMultiplication
    | OpDivision
    | OpPower
    | OpSinh
    | OpCosh


getOperationGlsl : Operation -> String
getOperationGlsl op =
    case op of
        OpAddition ->
            ""

        OpMultiplication ->
            """
            vec2 by(vec2 a, vec2 b) {
                return vec2(a.x*b.x-a.y*b.y, a.x*b.y+a.y*b.x);
            }
            """

        OpDivision ->
            """
            vec2 div(vec2 a, vec2 b) {
                float r = (a.x*b.x+a.y*b.y)/(b.x*b.x+b.y*b.y);
                float i = (a.y*b.x-a.x*b.y)/(b.x*b.x+b.y*b.y);
                return vec2(r, i);
            }
            """

        OpPower ->
            """
            vec2 cpow(vec2 b, vec2 e) {
                vec2 v = by(cln(b), e);
                return vec2(cos(v.y) * exp(v.x), sin(v.y) * exp(v.x));
            }
            """

        OpSinh ->
            """
            float sinh(float x) {
                return 0.5 * (exp(x) - exp(-x));
            }
            """

        OpCosh ->
            """
            float cosh(float x) {
                return 0.5 * (exp(x) + exp(-x));
            }
            """


getConstantGlsl : String -> String
getConstantGlsl c =
    case c of
        "i" ->
            """
            vec2 i(){
                return vec2(0,1);
            }
            """

        "pi" ->
            """
            vec2 pi() {
                return vec2(radians(180.0), 0.0);
            }
            """

        _ ->
            "#error: unknown constant " ++ c


getFunctionGlsl : KnownFunction -> String
getFunctionGlsl name =
    case name of
        Sin ->
            """
            vec2 csin(vec2 z) {
                return vec2(sin(z.x) * cosh(z.y), cos(z.x) * sinh(z.y));
            }
            """

        Cos ->
            """
            vec2 ccos(vec2 z) {
                return vec2(cos(z.x) * cosh(z.y), sin(z.x) * sinh(z.y));
            }
            """

        Tan ->
            """
            vec2 ctan(vec2 z) {
                return div(csin(z), ccos(z));
            }
            """

        Asin ->
            Debug.todo "Asin"

        Acos ->
            Debug.todo "Acos"

        Atan ->
            Debug.todo "Atan"

        Atan2 ->
            """
            vec2 catan2(vec2 y, vec2 x) {
                vec2 z = vec2(x.x - y.y, x.y + y.x);
                return vec2(atan(z.y, z.x), 0.0);
            }
            """

        Sinh ->
            """
            vec2 csinh(vec2 z) {
                return by(-i(), csin(by(i(), z)));
            }
            """

        Cosh ->
            """
            vec2 ccosh(vec2 z) {
                return ccos(by(i(), z));
            }
            """

        Tanh ->
            """
            vec2 ctanh(vec2 z) {
                return div(csinh(z), ccosh(z));
            }
            """

        Abs ->
            """
            vec2 cabs(vec2 v) {
                return vec2(sqrt(v.x*v.x + v.y*v.y), 0.0);
            }
            """

        Sqrt ->
            """
            vec2 csqrt(vec2 z) {
                float r = pow(z.x*z.x+z.y*z.y, 0.25);
                float t = atan(z.y, z.x) * 0.5;
                return vec2(r * cos(t), r * sin(t));
            }
            """

        Ln ->
            """
            vec2 cln(vec2 b) {
                float px = sqrt(b.x*b.x+b.y*b.y);
                float py = atan(b.y, b.x);
                return vec2(log(px), py);
            }
            """

        Log10 ->
            Debug.todo "Log10"

        Exp ->
            """
            vec2 cexp(vec2 v) {
                return vec2(cos(v.y) * exp(v.x), sin(v.y) * exp(v.x));
            }
            """

        Re ->
            """
            vec2 cre(vec2 z) {
                return vec2(z.x, 0.0);
            }
            """

        Im ->
            """
            vec2 cim(vec2 z) {
                return vec2(z.y, 0.0);
            }
            """

        Arg ->
            """
            vec2 carg(vec2 v) {
                return vec2(atan(v.y, v.x), 0);
            }
            """

        Gra ->
            Debug.todo "Gra"

        Dd ->
            Debug.todo "Dd"

        Ii ->
            Debug.todo "Ii"

        Pw ->
            """
            vec2 cpw(vec2 c, vec2 t, vec2 f) {
                return c.x > 0.0 ? t : f;
            }
            """

        Plot ->
            Debug.todo "Plot"

        Simplify ->
            Debug.todo "Simplify"


toSrcImplicit : Expression -> String
toSrcImplicit e =
    """
    bool f(float x, float y) {
        vec2 complex = """ ++ Expression.toGLString e ++ """;
        return complex.x > 0.0;
    }

    vec3 pixel(float deltaX, float deltaY, float x, float y) {
        bool h = f(x,y);
        bool l = f(x - deltaX,y);
        bool ul = f(x - deltaX,y - deltaY);
        bool u = f(x,y - deltaY);
        return (h != l || h != u || h != ul) ? vec3(1,1,1) : vec3(0,0,0);
    }
    """


toSrcRelation : Expression -> String
toSrcRelation e =
    """
    vec3 pixel(float deltaX, float deltaY, float x, float y) {
        vec2 complex = """ ++ Expression.toGLString e ++ """;
        return complex.x > 0.0 ? vec3(0.8,0.5,0.5) : vec3(0,0,0);
    }
    """


toSrcContour : Expression -> String
toSrcContour e =
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

    vec3 pixel(float deltaX, float deltaY, float x, float y) {
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
    """
