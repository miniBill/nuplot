module Expression.Value exposing (complexToSymbolic, value)

import Complex exposing (Complex(..))
import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), Graph(..), KnownFunction(..), RelationOperation(..), UnaryOperation(..), Value(..), partialSubstitute)
import Expression.Parser
import Expression.Simplify
import Expression.Utils as Utils


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
            complexMap { symbolic = UnaryOperation Negate, complex = Complex.negate, list = Nothing } <| innerValue context e

        BinaryOperation Division l r ->
            complexMap2 { symbolic = BinaryOperation Division, complex = Complex.div, list = Nothing } (innerValue context l) (innerValue context r)

        BinaryOperation Power l r ->
            complexMap2 { symbolic = BinaryOperation Power, complex = Complex.power, list = Nothing } (innerValue context l) (innerValue context r)

        RelationOperation o l r ->
            complexMap2 { symbolic = RelationOperation o, complex = relationValue o, list = Nothing } (innerValue context l) (innerValue context r)

        AssociativeOperation Addition l r o ->
            List.foldl plus (innerValue context l) <| List.map (innerValue context) (r :: o)

        AssociativeOperation Multiplication l r o ->
            List.foldl by (innerValue context l) <| List.map (innerValue context) (r :: o)

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


plus : Value -> Value -> Value
plus =
    let
        inner u v =
            case ( u, v ) of
                ( Float f, _ ) ->
                    if f == 0 then
                        v

                    else
                        AssociativeOperation Addition u v []

                ( _, Float f ) ->
                    if f == 0 then
                        u

                    else
                        AssociativeOperation Addition u v []

                _ ->
                    AssociativeOperation Addition u v []
    in
    complexMap2
        { symbolic = inner
        , complex = Complex.plus
        , list = Nothing
        }


by : Value -> Value -> Value
by =
    let
        inner u v =
            case ( u, v ) of
                ( Float f, _ ) ->
                    if f == 0 then
                        Float 0

                    else if f == 1 then
                        v

                    else
                        AssociativeOperation Multiplication u v []

                ( _, Float f ) ->
                    if f == 0 then
                        Float 0

                    else if f == 1 then
                        u

                    else
                        AssociativeOperation Multiplication u v []

                _ ->
                    AssociativeOperation Multiplication u v []
    in
    complexMap2
        { symbolic = inner
        , complex = Complex.by
        , list = Just matrixMultiplication
        }


matrixMultiplication : List Value -> List Value -> Value
matrixMultiplication l r =
    let
        getRow j m =
            m
                |> List.drop j
                |> List.head
                |> Maybe.withDefault []

        getCol j m =
            m
                |> List.filterMap
                    (List.drop j >> List.head)

        size m =
            let
                rows =
                    List.length m

                asLists =
                    m
                        |> List.filterMap
                            (\w ->
                                case w of
                                    ListValue us ->
                                        Just us

                                    _ ->
                                        Nothing
                            )
            in
            case asLists of
                [] ->
                    Nothing

                h :: t ->
                    let
                        cols =
                            List.length h
                    in
                    if List.length t < rows - 1 || List.any (List.length >> (/=) cols) asLists then
                        Nothing

                    else
                        Just ( rows, cols, asLists )

        dot x y =
            case List.map2 by x y of
                [] ->
                    ComplexValue Complex.zero

                h :: t ->
                    List.foldl plus h t

        inner ( lr, lc, lm ) ( rr, rc, rm ) =
            if lc /= rr then
                ErrorValue "Cannot multiply matrices, wrong size"

            else
                List.range 0 (lr - 1)
                    |> List.map
                        (\row ->
                            List.range 0 (rc - 1)
                                |> List.map
                                    (\col ->
                                        dot
                                            (getRow row lm)
                                            (getCol col rm)
                                    )
                                |> ListValue
                        )
                    |> ListValue
    in
    Maybe.map2 inner (size l) (size r)
        |> Maybe.withDefault (ErrorValue "Can only multiply rectangular matrices")


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
                    complexMap2
                        { symbolic = \l r -> Apply name [ l, r ]
                        , complex = Complex.atan2
                        , list = Nothing
                        }
                        (innerValue context y)
                        (innerValue context x)

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

        KnownFunction Det ->
            case args of
                [ c ] ->
                    determinant <| innerValue context c

                _ ->
                    ErrorValue "Unexpected number of args to pw, expected 1"

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


determinant : Value -> Value
determinant val =
    case val of
        ListValue ls ->
            let
                rows =
                    ls
                        |> List.filterMap
                            (\r ->
                                case r of
                                    ListValue cs ->
                                        Just cs

                                    _ ->
                                        Nothing
                            )

                cols =
                    rows
                        |> List.map List.length
                        |> (\lens ->
                                if List.minimum lens == List.maximum lens then
                                    List.minimum lens

                                else
                                    Nothing
                           )
                        |> Maybe.withDefault -1

                fromList default f xs =
                    case List.reverse xs of
                        [] ->
                            default

                        h :: t ->
                            List.foldr (\e a -> f a e) h t
            in
            if List.length rows < List.length ls then
                ErrorValue "Can only calculate the determinant of matrices"

            else if List.length rows /= cols then
                ErrorValue <|
                    "Can only calculate the determinant of square matrices, but the size was "
                        ++ String.fromInt (List.length ls)
                        ++ "x"
                        ++ String.fromInt cols

            else
                rows
                    |> Utils.genericDeterminant
                        { add = fromList (ComplexValue Complex.zero) plus
                        , multiply = fromList (ComplexValue <| Complex.fromReal 1) by
                        , negate = complexMap { symbolic = UnaryOperation Negate, complex = Complex.negate, list = Nothing }
                        }
                    |> Maybe.withDefault (ErrorValue "Error in calculating the determinant")

        SymbolicValue s ->
            SymbolicValue <| Utils.determinant s

        GraphValue _ ->
            ErrorValue "Cannot calculate the determinant of a graph"

        ComplexValue _ ->
            val

        ErrorValue _ ->
            val


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
                    ListValue <|
                        List.map
                            (complexMap
                                { symbolic = \w -> Apply (KnownFunction s) [ w ]
                                , complex = f
                                , list = Nothing
                                }
                            )
                            ls

                ErrorValue err ->
                    ErrorValue err

                u ->
                    ErrorValue <| "Unexpected argument: " ++ toString u

        _ ->
            ErrorValue "Unexpected number of arguments, expected 1"


toString : Value -> String
toString v =
    case v of
        ComplexValue z ->
            Complex.toString z

        ListValue ls ->
            "{" ++ String.join ", " (List.map toString ls) ++ "}"

        ErrorValue e ->
            "Error: " ++ e

        SymbolicValue s ->
            Expression.toString s

        GraphValue g ->
            "Graph: " ++ Expression.graphToString g


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


complexMap :
    { symbolic : Expression -> Expression
    , complex : Complex -> Complex
    , list : Maybe (List Value -> Value)
    }
    -> Value
    -> Value
complexMap ({ symbolic, complex, list } as fs) v =
    case v of
        ErrorValue _ ->
            v

        ListValue ls ->
            case list of
                Just lf ->
                    lf ls

                Nothing ->
                    ListValue <| List.map (complexMap fs) ls

        ComplexValue c ->
            ComplexValue <| complex c

        GraphValue _ ->
            ErrorValue "Tried to apply function to graph"

        SymbolicValue w ->
            SymbolicValue <| symbolic w


complexMap2 :
    { symbolic : Expression -> Expression -> Expression
    , complex : Complex -> Complex -> Complex
    , list : Maybe (List Value -> List Value -> Value)
    }
    -> Value
    -> Value
    -> Value
complexMap2 ({ symbolic, complex, list } as fs) v w =
    case ( v, w ) of
        ( ErrorValue _, _ ) ->
            v

        ( _, ErrorValue _ ) ->
            w

        ( ListValue ls, ListValue rs ) ->
            case list of
                Just lf ->
                    lf ls rs

                Nothing ->
                    ListValue <| List.map2 (complexMap2 fs) ls rs

        ( ComplexValue l, ComplexValue r ) ->
            ComplexValue <| complex l r

        ( SymbolicValue l, _ ) ->
            SymbolicValue <| symbolic l (toSymbolic w)

        ( _, SymbolicValue r ) ->
            SymbolicValue <| symbolic (toSymbolic v) r

        ( ListValue l, ComplexValue _ ) ->
            ListValue <| List.map (\le -> complexMap2 fs le w) l

        ( ComplexValue _, ListValue r ) ->
            ListValue <| List.map (\re -> complexMap2 fs v re) r

        ( GraphValue _, _ ) ->
            ErrorValue "Cannot perform calculations on graphs"

        ( _, GraphValue _ ) ->
            ErrorValue "Cannot perform calculations on graphs"


toSymbolic : Value -> Expression
toSymbolic v =
    let
        graphToExpression g =
            case g of
                Explicit2D e ->
                    e

                Relation2D o l r ->
                    RelationOperation o l r

                Implicit2D l r ->
                    RelationOperation Equals l r

                Contour e ->
                    e

                GraphList gs ->
                    List <| List.map graphToExpression gs
    in
    case v of
        SymbolicValue s ->
            s

        ComplexValue c ->
            complexToSymbolic c

        ListValue l ->
            List <| List.map toSymbolic l

        GraphValue g ->
            Apply (KnownFunction Plot) [ graphToExpression g ]

        ErrorValue _ ->
            Integer 0


complexToSymbolic : Complex -> Expression
complexToSymbolic (Complex r i) =
    if i == 0 then
        Float r

    else if r == 0 then
        if i == 1 then
            Variable "i"

        else
            Utils.by [ Float i, Variable "i" ]

    else
        Utils.plus [ Float r, Utils.by [ Float i, Variable "i" ] ]
