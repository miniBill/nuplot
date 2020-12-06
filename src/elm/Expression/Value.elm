module Expression.Value exposing (value)

import Complex exposing (Complex(..))
import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), RelationOperation(..), UnaryOperation(..), Value(..), partialSubstitute)
import Expression.Parser
import Expression.Simplify
import Expression.Utils exposing (by, plus)


defaultValueContext : Dict String Value
defaultValueContext =
    Dict.fromList
        [ ( "e", ComplexValue <| Complex.fromReal e )
        , ( "pi", ComplexValue <| Complex.fromReal pi )
        , ( "π", ComplexValue <| Complex.fromReal pi )
        ]


value : Dict String Value -> Expression -> Value
value context =
    innerValue (Dict.union context defaultValueContext)


innerValue : Dict String Value -> Expression -> Value
innerValue context expr =
    case expr of
        Variable "i" ->
            ComplexValue Complex.i

        Variable v ->
            Dict.get v context
                |> Maybe.withDefault (SymbolicValue expr)

        UnaryOperation Negate e ->
            complexMap (UnaryOperation Negate) Complex.negate <| innerValue context e

        BinaryOperation Division l r ->
            complexMap2 (BinaryOperation Division) Complex.div (innerValue context l) (innerValue context r)

        BinaryOperation Power l r ->
            complexMap2 (BinaryOperation Power) Complex.power (innerValue context l) (innerValue context r)

        RelationOperation o l r ->
            complexMap2 (RelationOperation o) (relationValue o) (innerValue context l) (innerValue context r)

        AssociativeOperation Addition l r o ->
            List.foldl
                (complexMap2 (\u v -> AssociativeOperation Addition u v []) Complex.plus)
                (innerValue context l)
            <|
                List.map (innerValue context) (r :: o)

        AssociativeOperation Multiplication l r o ->
            List.foldl (complexMap2 (\u v -> AssociativeOperation Multiplication u v []) Complex.by)
                (innerValue context l)
            <|
                List.map (innerValue context) (r :: o)

        Apply name args ->
            applyValue context name args

        Integer i ->
            ComplexValue <| Complex.fromReal <| toFloat i

        Float f ->
            ComplexValue <| Complex.fromReal f

        List ls ->
            ListValue <| List.map (innerValue context) ls

        Replace ctx e ->
            innerValue context <| List.foldl (\( k, v ) -> partialSubstitute k v) e (Dict.toList ctx)


applyValue : Dict String Value -> FunctionName -> List Expression -> Value
applyValue context name args =
    case name of
        KnownFunction Sin ->
            unaryFunctionValue context args Sin Complex.sin

        KnownFunction Cos ->
            unaryFunctionValue context args Cos Complex.cos

        KnownFunction Tan ->
            unaryFunctionValue context args Tan Complex.tan

        KnownFunction Sinh ->
            unaryFunctionValue context args Sinh Complex.sinh

        KnownFunction Cosh ->
            unaryFunctionValue context args Cosh Complex.cosh

        KnownFunction Tanh ->
            unaryFunctionValue context args Tanh Complex.tanh

        KnownFunction Asin ->
            unaryFunctionValue context args Asin Complex.asin

        KnownFunction Acos ->
            unaryFunctionValue context args Acos Complex.acos

        KnownFunction Atan ->
            unaryFunctionValue context args Atan Complex.atan

        KnownFunction Atan2 ->
            case args of
                [ y, x ] ->
                    complexMap2 (\l r -> Apply name [ l, r ]) Complex.atan2 (innerValue context y) (innerValue context x)

                _ ->
                    ErrorValue "Unexpected number of args to atan2, expected 2"

        KnownFunction Sqrt ->
            unaryFunctionValue context args Sqrt Complex.sqrt

        KnownFunction Ln ->
            unaryFunctionValue context args Ln Complex.ln

        KnownFunction Arg ->
            unaryFunctionValue context args Arg (Complex.fromReal << Complex.arg)

        KnownFunction Re ->
            unaryFunctionValue context args Re Complex.re

        KnownFunction Im ->
            unaryFunctionValue context args Im Complex.im

        KnownFunction Abs ->
            unaryFunctionValue context args Abs (Complex.fromReal << Complex.abs)

        KnownFunction Log10 ->
            unaryFunctionValue context args Log10 <| \v -> Complex.div v (Complex.ln <| Complex.fromReal 10)

        KnownFunction Exp ->
            unaryFunctionValue context args Exp Complex.exp

        KnownFunction Pw ->
            case args of
                [ c, t, f ] ->
                    if ComplexValue Complex.zero == innerValue context c then
                        innerValue context f

                    else
                        innerValue context t

                _ ->
                    ErrorValue "Unexpected number of args to pw, expected 3"

        KnownFunction Gra ->
            ErrorValue "TODO"

        KnownFunction Dd ->
            ErrorValue "TODO"

        KnownFunction Ii ->
            ErrorValue "TODO"

        KnownFunction Simplify ->
            case args of
                [ e ] ->
                    SymbolicValue <| Expression.Simplify.simplify e

                _ ->
                    ErrorValue "Error in simplify"

        KnownFunction Plot ->
            case args of
                [ e ] ->
                    GraphValue <| Expression.Parser.expressionToGraph e

                _ ->
                    ErrorValue "Error in plot"

        UserFunction _ ->
            ErrorValue "TODO"


unaryFunctionValue : Dict String Value -> List Expression -> KnownFunction -> (Complex -> Complex) -> Value
unaryFunctionValue context args s f =
    case args of
        [ e ] ->
            case innerValue context e of
                ComplexValue z ->
                    ComplexValue <| f z

                SymbolicValue w ->
                    SymbolicValue <| Apply (KnownFunction s) [ w ]

                ListValue ls ->
                    ListValue <| List.map (complexMap (\w -> Apply (KnownFunction s) [ w ]) f) ls

                ErrorValue err ->
                    ErrorValue err

                u ->
                    ErrorValue <| "Unexpected argument: " ++ Debug.toString u

        _ ->
            ErrorValue "Unexpected number of arguments, expected 1"


relationValue : RelationOperation -> Complex -> Complex -> Complex
relationValue o ((Complex lv _) as lc) ((Complex rv _) as rc) =
    let
        toComplex check =
            if check then
                Complex.one

            else
                Complex.zero
    in
    case o of
        LessThan ->
            toComplex <| lv < rv

        LessThanOrEquals ->
            toComplex <| lv <= rv

        Equals ->
            Complex.minus lc rc

        GreaterThanOrEquals ->
            toComplex <| lv >= rv

        GreaterThan ->
            toComplex <| lv > rv


complexMap : (Expression -> Expression) -> (Complex -> Complex) -> Value -> Value
complexMap s f v =
    case v of
        ErrorValue _ ->
            v

        ListValue ls ->
            ListValue <| List.map (complexMap s f) ls

        ComplexValue c ->
            ComplexValue <| f c

        GraphValue _ ->
            ErrorValue "Tried to apply function to graph"

        SymbolicValue w ->
            SymbolicValue <| s w


complexMap2 : (Expression -> Expression -> Expression) -> (Complex -> Complex -> Complex) -> Value -> Value -> Value
complexMap2 s f v w =
    case ( v, w ) of
        ( ErrorValue _, _ ) ->
            v

        ( _, ErrorValue _ ) ->
            w

        ( ListValue ls, ListValue rs ) ->
            ListValue <| List.map2 (complexMap2 s f) ls rs

        ( ComplexValue l, ComplexValue r ) ->
            ComplexValue <| f l r

        ( SymbolicValue l, SymbolicValue r ) ->
            SymbolicValue <| s l r

        ( SymbolicValue l, ComplexValue r ) ->
            SymbolicValue <| s l (complexToSymbolic r)

        ( ComplexValue l, SymbolicValue r ) ->
            SymbolicValue <| s (complexToSymbolic l) r

        _ ->
            ErrorValue <| "Incompatible: " ++ Debug.toString { v = v, w = w }


complexToSymbolic : Complex -> Expression
complexToSymbolic (Complex r i) =
    if i == 0 then
        Float r

    else if r == 0 then
        if i == 1 then
            Variable "i"

        else
            by [ Float i, Variable "i" ]

    else
        plus [ Float r, by [ Float i, Variable "i" ] ]
