module Expression.Value exposing (Value(..), complexToSymbolic, toExpression, toString, value)

import Complex exposing (Complex(..))
import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), RelationOperation(..), SolutionTree, UnaryOperation(..), filterContext, fullSubstitute, genericAsSquareMatrix, genericDeterminant, genericMatrixMultiplication)
import Expression.Graph exposing (Graph)
import Expression.Simplify
import Expression.Solver
import Expression.Utils as Utils
import UI.L10N exposing (L10N, invariant)


type Value
    = ErrorValue (L10N String)
    | SolutionTreeValue SolutionTree
    | SymbolicValue Expression
    | ComplexValue Complex
    | GraphValue { axes : Bool, graph : Graph }
    | LambdaValue String Value
    | ListValue (List Value)


defaultValueContext : Dict String Value
defaultValueContext =
    Dict.fromList
        [ ( "e", ComplexValue <| Complex.fromReal e )
        , ( "pi", ComplexValue <| Complex.fromReal pi )
        , ( "π", ComplexValue <| Complex.fromReal pi )
        ]


value : Dict String Value -> Expression -> Value
value context =
    Expression.Simplify.hoistLambda
        >> innerValue (Dict.union context defaultValueContext)


innerValue : Dict String Value -> Expression -> Value
innerValue context expr =
    case expr of
        Variable "i" ->
            ComplexValue Complex.i

        Variable v ->
            Dict.get v context
                |> Maybe.withDefault (SymbolicValue expr)

        UnaryOperation Negate e ->
            negateValue <| innerValue context e

        BinaryOperation Division l r ->
            simpleComplexMap2 (BinaryOperation Division) Complex.div context l r

        BinaryOperation Power l r ->
            simpleComplexMap2 (BinaryOperation Power) Complex.power context l r

        RelationOperation o l r ->
            simpleComplexMap2 (RelationOperation o) (relationValue o) context l r

        AssociativeOperation Addition l r o ->
            plus context <| List.map (innerValue context) (l :: r :: o)

        AssociativeOperation Multiplication l r o ->
            by context <| List.map (innerValue context) (l :: r :: o)

        Apply name args ->
            applyValue context name args

        Lambda x f ->
            LambdaValue x <| innerValue (Dict.insert x (SymbolicValue <| Variable x) context) f

        Integer i ->
            ComplexValue <| Complex.fromReal <| toFloat i

        Float f ->
            ComplexValue <| Complex.fromReal f

        List ls ->
            ListValue <| List.map (innerValue context) ls

        Replace ctx e ->
            innerValue context <| fullSubstitute (filterContext ctx) e


negateValue : Value -> Value
negateValue =
    complexMap
        { symbolic = UnaryOperation Negate
        , complex = Complex.negate
        , list = Nothing
        , lambda = Nothing
        }


plusTwo : Dict String Value -> Value -> Value -> Value
plusTwo context =
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
        , lambda = Nothing
        , context = context
        }


byTwo : Dict String Value -> Value -> Value -> Value
byTwo context =
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
        , list = Just <| matrixMultiplication context
        , lambda =
            Just
                ( \x f r -> partialSubstitute x r f
                , \l x f -> LambdaValue x <| byTwo context l f
                )
        , context = context
        }


partialSubstitute : String -> Value -> Value -> Value
partialSubstitute var val expr =
    case expr of
        SymbolicValue s ->
            SymbolicValue <| Expression.partialSubstitute var (toExpression val) s

        LambdaValue x f ->
            if x == var then
                expr

            else
                LambdaValue x <| partialSubstitute var val f

        ListValue ls ->
            ListValue <| List.map (partialSubstitute var val) ls

        GraphValue _ ->
            expr

        ComplexValue _ ->
            expr

        ErrorValue _ ->
            expr

        SolutionTreeValue _ ->
            expr


matrixMultiplication : Dict String Value -> List Value -> List Value -> Value
matrixMultiplication context l r =
    genericMatrixMultiplication { asList = asList, by = by context, plus = plus context, toList = ListValue } (ListValue l) (ListValue r)
        |> Maybe.withDefault
            (ErrorValue
                { en = "Cannot multiply the matrices"
                , it = "Impossibile moltiplicare let matrici"
                }
            )


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

        KnownFunction Floor ->
            unaryFunctionValue context args Floor Complex.floor

        KnownFunction Ceiling ->
            unaryFunctionValue context args Ceiling Complex.ceiling

        KnownFunction Round ->
            unaryFunctionValue context args Round Complex.round

        KnownFunction Min ->
            arbitraryFunctionValue context args Min Complex.min

        KnownFunction Max ->
            arbitraryFunctionValue context args Max Complex.max

        KnownFunction Atan2 ->
            binaryFunctionValue context args Atan2 Complex.atan2

        KnownFunction (Root 2) ->
            unaryFunctionValue context args (Root 2) Complex.sqrt

        KnownFunction (Root n) ->
            unaryFunctionValue context args (Root n) (\v -> Complex.power v (Complex.fromReal <| 1.0 / toFloat n))

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

        KnownFunction Sign ->
            unaryFunctionValue context args Sign Complex.sign

        KnownFunction Log10 ->
            unaryFunctionValue context args Log10 <| \v -> Complex.div v (Complex.ln <| Complex.fromReal 10)

        KnownFunction Mbrot ->
            SymbolicValue <| Apply name args

        KnownFunction Det ->
            case args of
                [ c ] ->
                    determinant context <| innerValue context c

                _ ->
                    unexpectedArgCount (Just "det") [ 1 ]

        KnownFunction Exp ->
            unaryFunctionValue context args Exp Complex.exp

        KnownFunction Pw ->
            case args of
                [ c, t, f ] ->
                    case innerValue context c of
                        ComplexValue cv ->
                            innerValue context <|
                                if cv == Complex.zero then
                                    f

                                else
                                    t

                        ErrorValue e ->
                            ErrorValue e

                        SymbolicValue s ->
                            SymbolicValue <| Apply (KnownFunction Pw) [ s, t, f ]

                        _ ->
                            SymbolicValue <| Apply (KnownFunction Pw) args

                _ ->
                    unexpectedArgCount (Just "pw") [ 3 ]

        KnownFunction Solve ->
            case args of
                [ e, x ] ->
                    SolutionTreeValue <|
                        Expression.Solver.solve e x

                _ ->
                    unexpectedArgCount (Just "solve") [ 2 ]

        KnownFunction Simplify ->
            case args of
                [ e ] ->
                    SymbolicValue <| Expression.Simplify.simplify e

                _ ->
                    unexpectedArgCount (Just "simplify") [ 1 ]

        KnownFunction StepSimplify ->
            case args of
                [ e ] ->
                    SolutionTreeValue <| Expression.Solver.stepSimplify e

                _ ->
                    unexpectedArgCount (Just "stepsimplify") [ 1 ]

        KnownFunction Plot ->
            case args of
                [ e ] ->
                    GraphValue
                        { axes = True
                        , graph = Expression.Graph.fromExpression <| toExpression <| value Dict.empty e
                        }

                _ ->
                    unexpectedArgCount (Just "plot") [ 1 ]

        KnownFunction APlot ->
            case args of
                [ e ] ->
                    GraphValue
                        { axes = False
                        , graph = Expression.Graph.fromExpression <| toExpression <| value Dict.empty e
                        }

                _ ->
                    unexpectedArgCount (Just "aplot") [ 1 ]

        KnownFunction For ->
            case Utils.runForLoop args of
                Just values ->
                    value context <| List values

                _ ->
                    unexpectedArgCount (Just "for") [ 2, 3 ]

        KnownFunction Gra ->
            --TODO
            SymbolicValue <| Apply name args

        KnownFunction Dd ->
            --TODO
            SymbolicValue <| Apply name args

        KnownFunction Ii ->
            --TODO
            SymbolicValue <| Apply name args

        KnownFunction Mod ->
            --TODO
            SymbolicValue <| Apply name args

        UserFunction _ ->
            --TODO
            SymbolicValue <| Apply name args


unexpectedArgCount : Maybe String -> List Int -> Value
unexpectedArgCount maybeName count =
    case maybeName of
        Just name ->
            ErrorValue
                { en =
                    "Unexpected number of args to "
                        ++ name
                        ++ ", expected "
                        ++ String.join " or " (List.map String.fromInt count)
                , it =
                    "Numero di argomenti inatteso per "
                        ++ name
                        ++ ", "
                        ++ (if count == [ 1 ] then
                                "atteso "

                            else
                                "attesi "
                           )
                        ++ String.join " o " (List.map String.fromInt count)
                }

        Nothing ->
            ErrorValue
                { en =
                    "Unexpected number of args, expected "
                        ++ String.join " or " (List.map String.fromInt count)
                , it =
                    "Numero di argomenti inatteso, "
                        ++ (if count == [ 1 ] then
                                "atteso "

                            else
                                "attesi "
                           )
                        ++ String.join " o " (List.map String.fromInt count)
                }


asList : Value -> Maybe (List Value)
asList v =
    case v of
        ListValue ls ->
            Just ls

        _ ->
            Nothing


asSquareMatrix : Value -> Maybe (List (List Value))
asSquareMatrix =
    genericAsSquareMatrix asList


assocToList : a -> (a -> a -> a) -> List a -> a
assocToList default f xs =
    case xs of
        [] ->
            default

        h :: t ->
            List.foldl (\e a -> f a e) h t


plus : Dict String Value -> List Value -> Value
plus context =
    assocToList (ComplexValue Complex.zero) (plusTwo context)


by : Dict String Value -> List Value -> Value
by context =
    assocToList (ComplexValue <| Complex.fromReal 1) (byTwo context)


determinant : Dict String Value -> Value -> Value
determinant context val =
    case val of
        ListValue _ ->
            case asSquareMatrix val of
                Just rows ->
                    rows
                        |> genericDeterminant
                            { plus = plus context
                            , by = by context
                            , negate = negateValue
                            }
                        |> Maybe.withDefault
                            (ErrorValue
                                { en = "Error in calculating the determinant"
                                , it = "Errore nel calcolo del determinante"
                                }
                            )

                Nothing ->
                    ErrorValue <|
                        UI.L10N.concat
                            [ strings.cannotCalculateDeterminantOf
                            , { en = "a nonquare matrix"
                              , it = "una matrice non quadrata"
                              }
                            ]

        SymbolicValue s ->
            SymbolicValue <| Utils.determinant s

        GraphValue _ ->
            ErrorValue <|
                UI.L10N.concat
                    [ strings.cannotCalculateDeterminantOf
                    , strings.graphs
                    ]

        ComplexValue _ ->
            val

        ErrorValue _ ->
            val

        LambdaValue _ _ ->
            val

        SolutionTreeValue _ ->
            ErrorValue <|
                UI.L10N.concat
                    [ strings.cannotCalculateDeterminantOf
                    , strings.solutionTrees
                    ]


unaryFunctionValue : Dict String Value -> List Expression -> KnownFunction -> (Complex -> Complex) -> Value
unaryFunctionValue context args s f =
    case args of
        [ e ] ->
            complexMap
                { symbolic = \sv -> Apply (KnownFunction s) [ sv ]
                , complex = f
                , list = Nothing
                , lambda = Nothing
                }
                (innerValue context e)

        _ ->
            unexpectedArgCount Nothing [ 1 ]


binaryFunctionValue : Dict String Value -> List Expression -> KnownFunction -> (Complex -> Complex -> Complex) -> Value
binaryFunctionValue context args s f =
    case args of
        [ l, r ] ->
            complexMap2
                { symbolic = \lv rv -> Apply (KnownFunction s) [ lv, rv ]
                , complex = f
                , list = Nothing
                , lambda = Nothing
                , context = context
                }
                (innerValue context l)
                (innerValue context r)

        _ ->
            unexpectedArgCount Nothing [ 2 ]


arbitraryFunctionValue : Dict String Value -> List Expression -> KnownFunction -> (Complex -> Complex -> Complex) -> Value
arbitraryFunctionValue context args s f =
    case args of
        h :: t ->
            List.foldr
                (\e ->
                    complexMap2
                        { symbolic = \lv rv -> Apply (KnownFunction s) [ lv, rv ]
                        , complex = f
                        , list = Nothing
                        , lambda = Nothing
                        , context = context
                        }
                        (innerValue context e)
                )
                (innerValue context h)
                t

        _ ->
            unexpectedArgCount Nothing [ 1 ]


toString : Value -> L10N String
toString v =
    case v of
        ComplexValue z ->
            invariant <| Complex.toString z

        ListValue ls ->
            UI.L10N.concat
                [ invariant "{"
                , UI.L10N.map (String.join ", ") (UI.L10N.traverse toString ls)
                , invariant "}"
                ]

        ErrorValue e ->
            { en = "Error: " ++ e.en
            , it = "Errore: " ++ e.it
            }

        SymbolicValue s ->
            invariant <| Expression.toString s

        GraphValue g ->
            { en =
                if g.axes then
                    "Graph: " ++ Expression.Graph.toString g.graph

                else
                    "Graph without axes: " ++ Expression.Graph.toString g.graph
            , it =
                if g.axes then
                    "Grafico: " ++ Expression.Graph.toString g.graph

                else
                    "Grafico senza assi: " ++ Expression.Graph.toString g.graph
            }

        LambdaValue x f ->
            UI.L10N.map (\fs -> "Lambda: " ++ x ++ " => " ++ fs) (toString f)

        SolutionTreeValue s ->
            UI.L10N.concat
                [ { en = "SolutionTree: ", it = "Albero di Soluzioni: " }
                , Expression.solutionTreeToString s
                ]


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
    , lambda : Maybe (String -> Value -> Value)
    }
    -> Value
    -> Value
complexMap ({ lambda, symbolic, complex, list } as fs) v =
    case v of
        ErrorValue _ ->
            v

        LambdaValue x f ->
            case lambda of
                Just l ->
                    l x f

                Nothing ->
                    LambdaValue x <| complexMap fs f

        ListValue ls ->
            case list of
                Just lf ->
                    lf ls

                Nothing ->
                    ListValue <| List.map (complexMap fs) ls

        ComplexValue c ->
            ComplexValue <| complex c

        GraphValue _ ->
            ErrorValue <|
                UI.L10N.concat
                    [ strings.cannotApplyFunctionsTo
                    , strings.graphs
                    ]

        SolutionTreeValue _ ->
            ErrorValue <|
                UI.L10N.concat
                    [ strings.cannotApplyFunctionsTo
                    , strings.solutionTrees
                    ]

        SymbolicValue w ->
            SymbolicValue <| symbolic w


simpleComplexMap2 :
    (Expression -> Expression -> Expression)
    -> (Complex -> Complex -> Complex)
    -> Dict String Value
    -> Expression
    -> Expression
    -> Value
simpleComplexMap2 symbolic complex context l r =
    complexMap2
        { symbolic = symbolic
        , complex = complex
        , list = Nothing
        , lambda = Nothing
        , context = context
        }
        (innerValue context l)
        (innerValue context r)


strings =
    { cannotCalculateDeterminantOf =
        { en = "Cannot calculate the determinant of "
        , it = "Impossibile calcolare il determinante di "
        }
    , cannotPerformCalculationsOn =
        { en = "Cannot perform calculations on "
        , it = "Impossibile effettuare operazioni su "
        }
    , cannotApplyFunctionsTo =
        { en = "Cannot apply functions to "
        , it = "Impossibile applicare funzioni a "
        }
    , graphs =
        { en = "graphs"
        , it = "grafici"
        }
    , solutionTrees =
        { en = "solution trees"
        , it = "alberi di soluzioni"
        }
    }


complexMap2 :
    { symbolic : Expression -> Expression -> Expression
    , complex : Complex -> Complex -> Complex
    , list : Maybe (List Value -> List Value -> Value)
    , lambda : Maybe ( String -> Value -> Value -> Value, Value -> String -> Value -> Value )
    , context : Dict String Value
    }
    -> Value
    -> Value
    -> Value
complexMap2 ({ symbolic, complex, list, lambda, context } as fs) v w =
    case ( v, w ) of
        ( ErrorValue _, _ ) ->
            v

        ( _, ErrorValue _ ) ->
            w

        ( SolutionTreeValue _, _ ) ->
            ErrorValue <|
                UI.L10N.concat
                    [ strings.cannotPerformCalculationsOn
                    , strings.solutionTrees
                    ]

        ( _, SolutionTreeValue _ ) ->
            ErrorValue <|
                UI.L10N.concat
                    [ strings.cannotPerformCalculationsOn
                    , strings.solutionTrees
                    ]

        ( LambdaValue x f, _ ) ->
            case lambda of
                Just ( l, _ ) ->
                    l x f w

                Nothing ->
                    LambdaValue x <|
                        innerValue (Dict.remove x context)
                            (symbolic (toExpression f) (toExpression w))

        ( _, LambdaValue x f ) ->
            case lambda of
                Just ( _, r ) ->
                    r v x f

                Nothing ->
                    LambdaValue x <|
                        innerValue (Dict.remove x context)
                            (symbolic (toExpression v) (toExpression f))

        ( ListValue ls, ListValue rs ) ->
            case list of
                Just lf ->
                    lf ls rs

                Nothing ->
                    ListValue <| List.map2 (complexMap2 fs) ls rs

        ( ComplexValue l, ComplexValue r ) ->
            ComplexValue <| complex l r

        ( SymbolicValue l, _ ) ->
            SymbolicValue <| symbolic l (toExpression w)

        ( _, SymbolicValue r ) ->
            SymbolicValue <| symbolic (toExpression v) r

        ( ListValue l, ComplexValue _ ) ->
            ListValue <| List.map (\le -> complexMap2 fs le w) l

        ( ComplexValue _, ListValue r ) ->
            ListValue <| List.map (\re -> complexMap2 fs v re) r

        ( GraphValue _, _ ) ->
            ErrorValue <|
                UI.L10N.concat
                    [ strings.cannotPerformCalculationsOn
                    , strings.graphs
                    ]

        ( _, GraphValue _ ) ->
            ErrorValue <|
                UI.L10N.concat
                    [ strings.cannotPerformCalculationsOn
                    , strings.graphs
                    ]


toExpression : Value -> Expression
toExpression v =
    case v of
        LambdaValue x f ->
            Lambda x <| toExpression f

        SymbolicValue s ->
            s

        ComplexValue c ->
            complexToSymbolic c

        ListValue l ->
            List <| List.map toExpression l

        GraphValue { graph, axes } ->
            Apply
                (KnownFunction <|
                    if axes then
                        Plot

                    else
                        APlot
                )
                [ Expression.Graph.toExpression graph ]

        ErrorValue _ ->
            Integer 0

        SolutionTreeValue _ ->
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
