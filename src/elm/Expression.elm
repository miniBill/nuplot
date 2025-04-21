module Expression exposing
    ( AssociativeOperation(..)
    , BinaryOperation(..)
    , Context
    , Expression(..)
    , Function1(..)
    , Function2(..)
    , Function3(..)
    , FunctionName(..)
    , KnownFunction(..)
    , RelationOperation(..)
    , SolutionTree(..)
    , TGApply(..)
    , ToGlslExpression(..)
    , TypecheckError(..)
    , UnaryOperation(..)
    , VariableStatus(..)
    , asMatrix
    , defaultContext
    , equals
    , filterContext
    , fullSubstitute
    , functionNameToString
    , genericAsMatrix
    , genericAsSquareMatrix
    , genericDeterminant
    , genericMatrixAddition
    , genericMatrixMultiplication
    , getFreeVariables
    , partialSubstitute
    , pfullSubstitute
    , relationToString
    , solutionTreeToString
    , toDebugTree
    , toString
    , toTeXString
    , typecheck
    , variadicFunctions
    , visit
    )

import Char
import Dict exposing (Dict)
import Element.WithContext as Element exposing (Element)
import Element.WithContext.Border as Border
import Element.WithContext.Font
import Result.Extra
import Set exposing (Set)
import Trie exposing (Trie)
import UI.L10N exposing (L10N, invariant)


type Expression
    = Integer Int
    | Float Float
    | Variable String
    | Lambda String Expression
    | UnaryOperation UnaryOperation Expression
    | BinaryOperation BinaryOperation Expression Expression
    | RelationOperation RelationOperation Expression Expression
    | AssociativeOperation AssociativeOperation Expression Expression (List Expression)
    | Apply FunctionName (List Expression)
    | Replace (Dict String (Maybe Expression)) Expression
    | List (List Expression)


type SolutionTree
    = SolutionForall String
    | SolutionError (L10N String)
    | SolutionNone String
    | SolutionDone Expression
    | SolutionStep Expression SolutionTree
    | SolutionBranch (List SolutionTree)


type FunctionName
    = KnownFunction KnownFunction


type KnownFunction
    = -- Trig
      Sin
    | Cos
    | Tan
    | Asin
    | Acos
    | Atan
    | Atan2
    | Sinh
    | Cosh
    | Tanh
      -- Power
    | Abs
    | Sqrt
    | Cbrt
    | Ln
    | Log10
    | Exp
    | Sign
      -- Complex
    | Re
    | Im
    | Arg
      -- Matrix
    | Gra
    | Det
      -- Derivative
    | Dd
    | Ii
      -- Misc
    | Min
    | Max
    | Round
    | Floor
    | Ceil
    | Piecewise
    | Plot
    | Simplify
    | StepSimplify
    | Solve
    | Mod
    | Mbrot
      -- Loops
    | For


type UnaryOperation
    = Negate


type BinaryOperation
    = Division
    | Power


type RelationOperation
    = LessThan
    | LessThanOrEquals
    | Equals
    | NotEquals
    | GreaterThanOrEquals
    | GreaterThan


relationToString : RelationOperation -> String
relationToString rop =
    case rop of
        LessThan ->
            "<"

        LessThanOrEquals ->
            "⩽"

        Equals ->
            "="

        NotEquals ->
            "≠"

        GreaterThanOrEquals ->
            "⩾"

        GreaterThan ->
            ">"


type AssociativeOperation
    = Addition
    | Multiplication


visit : (Expression -> Maybe Expression) -> Expression -> Expression
visit f expr =
    case f expr of
        Just r ->
            r

        Nothing ->
            case expr of
                Integer i ->
                    Integer i

                UnaryOperation uop c ->
                    UnaryOperation uop <| visit f c

                BinaryOperation bop l r ->
                    BinaryOperation bop (visit f l) (visit f r)

                RelationOperation rop l r ->
                    RelationOperation rop (visit f l) (visit f r)

                AssociativeOperation aop l r o ->
                    AssociativeOperation aop (visit f l) (visit f r) (List.map (visit f) o)

                Apply func args ->
                    Apply func <| List.map (visit f) args

                Variable v ->
                    Variable v

                Lambda x c ->
                    Lambda x <| visit f c

                Float fl ->
                    Float fl

                Replace vars c ->
                    Replace (Dict.map (\_ -> Maybe.map (visit f)) vars) (visit f c)

                List es ->
                    List <| List.map (visit f) es


pvisit : (ToGlslExpression -> Maybe ToGlslExpression) -> ToGlslExpression -> ToGlslExpression
pvisit f expr =
    case f expr of
        Just r ->
            r

        Nothing ->
            case expr of
                TGVariable s ->
                    TGVariable s

                TGInteger i ->
                    TGInteger i

                TGFloat f_ ->
                    TGFloat f_

                TGNegate l ->
                    TGNegate (pvisit f l)

                TGAdd l r ->
                    TGAdd (pvisit f l) (pvisit f r)

                TGBy l r ->
                    TGBy (pvisit f l) (pvisit f r)

                TGDiv l r ->
                    TGDiv (pvisit f l) (pvisit f r)

                TGPower l r ->
                    TGPower (pvisit f l) (pvisit f r)

                TGRel o l r ->
                    TGRel o (pvisit f l) (pvisit f r)

                TGApply (TGApply1 func l) ->
                    TGApply (TGApply1 func (pvisit f l))

                TGApply (TGApply2 func l m) ->
                    TGApply (TGApply2 func (pvisit f l) (pvisit f m))

                TGApply (TGApply3 func l m r) ->
                    TGApply (TGApply3 func (pvisit f l) (pvisit f m) (pvisit f r))

                TGList es ->
                    TGList <| List.map (pvisit f) es


partialSubstitute : String -> Expression -> Expression -> Expression
partialSubstitute var val =
    fullSubstitute <| Dict.singleton var val


fullSubstitute : Dict String Expression -> Expression -> Expression
fullSubstitute dict =
    visit <| \expr ->
    case expr of
        Replace vars e ->
            Just <|
                Replace (Dict.map (\_ -> Maybe.map <| fullSubstitute dict) vars) <|
                    let
                        reduced =
                            Dict.filter (\k _ -> not <| Dict.member k vars) dict
                    in
                    if Dict.isEmpty reduced then
                        e

                    else
                        fullSubstitute dict e

        Variable string ->
            Dict.get string dict
                |> Maybe.withDefault expr
                |> Just

        _ ->
            Nothing


equals : Expression -> Expression -> Bool
equals l r =
    case ( l, r ) of
        ( UnaryOperation lop le, UnaryOperation rop re ) ->
            (lop == rop) && equals le re

        ( UnaryOperation _ _, _ ) ->
            False

        ( BinaryOperation lop ll lr, BinaryOperation rop rl rr ) ->
            (lop == rop)
                && equals ll rl
                && equals lr rr

        ( BinaryOperation _ _ _, _ ) ->
            False

        ( AssociativeOperation lop ll lr lo, AssociativeOperation rop rl rr ro ) ->
            (lop == rop)
                && equals ll rl
                && equals lr rr
                && listEquals lo ro

        ( AssociativeOperation _ _ _ _, _ ) ->
            False

        ( Apply lname largs, Apply rname rargs ) ->
            lname == rname && listEquals largs rargs

        ( Apply _ _, _ ) ->
            False

        ( Variable lv, Variable rv ) ->
            lv == rv

        ( Variable _, _ ) ->
            False

        ( Integer li, Integer ri ) ->
            li == ri

        ( Integer _, _ ) ->
            False

        ( Float lf, Float rf ) ->
            lf == rf

        ( Float _, _ ) ->
            False

        ( Replace ls le, Replace rs re ) ->
            let
                maybeEquals ln rn =
                    case ( ln, rn ) of
                        ( Just lne, Just rne ) ->
                            equals lne rne

                        ( Nothing, Nothing ) ->
                            True

                        _ ->
                            False
            in
            (Dict.size ls == Dict.size rs)
                && equals le re
                && List.all identity
                    (List.map2
                        (\( lc, ln ) ( rc, rn ) -> lc == rc && maybeEquals ln rn)
                        (Dict.toList ls)
                        (Dict.toList rs)
                    )

        ( Replace _ _, _ ) ->
            False

        ( List ls, List rs ) ->
            listEquals ls rs

        ( List _, _ ) ->
            False

        ( RelationOperation lop ll lr, RelationOperation rop rl rr ) ->
            lop == rop && equals ll rl && equals lr rr

        ( RelationOperation _ _ _, _ ) ->
            False

        ( Lambda x f, Lambda y g ) ->
            let
                -- Yes, this is bad.
                -- Yes, this is enough for now
                goat =
                    Variable "GOATFINDER"
            in
            equals (partialSubstitute x goat f) (partialSubstitute y goat g)

        ( Lambda _ _, _ ) ->
            False


listEquals : List Expression -> List Expression -> Bool
listEquals ls rs =
    (List.length ls == List.length rs)
        && List.all identity (List.map2 equals ls rs)


toString : Expression -> String
toString =
    typecheck
        >> Result.map (toStringPrec 0)
        >> Result.withDefault "Typecheck error"


solutionTreeToString : SolutionTree -> L10N String
solutionTreeToString tree =
    case tree of
        SolutionDone e ->
            invariant <| toString e

        SolutionForall v ->
            invariant <| "∀" ++ v ++ "∈ ℝ"

        SolutionError e ->
            { en = "Error: " ++ e.en
            , it = "Errore: " ++ e.it
            }

        SolutionNone v ->
            invariant <| "∄" ++ v ++ "∈ ℝ"

        SolutionStep e n ->
            UI.L10N.map (\p -> toString e ++ ", " ++ p) (solutionTreeToString n)

        SolutionBranch ls ->
            UI.L10N.map (\ps -> "(" ++ String.join ") (" ps ++ ")") (UI.L10N.traverse solutionTreeToString ls)


type ToGlslExpression
    = TGInteger Int
    | TGFloat Float
    | TGVariable String
    | TGAdd ToGlslExpression ToGlslExpression
    | TGNegate ToGlslExpression
    | TGBy ToGlslExpression ToGlslExpression
    | TGDiv ToGlslExpression ToGlslExpression
    | TGRel RelationOperation ToGlslExpression ToGlslExpression
    | TGPower ToGlslExpression ToGlslExpression
    | TGList (List ToGlslExpression)
    | TGApply TGApply


type TGApply
    = TGApply1 Function1 ToGlslExpression
    | TGApply2 Function2 ToGlslExpression ToGlslExpression
    | TGApply3 Function3 ToGlslExpression ToGlslExpression ToGlslExpression


type Function1
    = -- Trig
      PSin
    | PCos
    | PTan
    | PAsin
    | PAcos
    | PAtan
    | PSinh
    | PCosh
    | PTanh
      -- Power
    | PAbs
    | PSqrt
    | PCbrt
    | PLn
    | PLog10
    | PExp
    | PSign
      -- Complex
    | PRe
    | PIm
    | PArg
      -- Matrix
    | PGra
    | PDet
      -- Misc
    | PRound
    | PFloor
    | PCeil


type Function2
    = -- Trig
      PAtan2
      -- Misc
    | PMin
    | PMax
    | PMod
    | PMbrot


type Function3
    = -- Misc
      PPiecewise


type TypecheckError
    = InvalidArgCount KnownFunction Int
    | FoundLambda


typecheck : Expression -> Result TypecheckError ToGlslExpression
typecheck e =
    case e of
        Variable v ->
            Ok (TGVariable v)

        Lambda _ _ ->
            Err FoundLambda

        Integer i ->
            Ok (TGInteger i)

        Float f ->
            Ok (TGFloat f)

        UnaryOperation Negate expression ->
            Result.map TGNegate <| typecheck expression

        BinaryOperation Power l r ->
            Result.map2 TGPower (typecheck l) (typecheck r)

        BinaryOperation Division l r ->
            Result.map2 TGDiv (typecheck l) (typecheck r)

        RelationOperation op l r ->
            Result.map2 (TGRel op) (typecheck l) (typecheck r)

        AssociativeOperation Addition l r o ->
            List.foldl (\el a -> Result.map2 TGAdd a el)
                (typecheck l)
                (List.map typecheck (r :: o))

        AssociativeOperation Multiplication l r o ->
            List.foldl (\el a -> Result.map2 TGBy a el)
                (typecheck l)
                (List.map typecheck (r :: o))

        List es ->
            es
                |> Result.Extra.combineMap typecheck
                |> Result.map TGList

        Apply (KnownFunction Sin) [ a0 ] ->
            Result.map (\t0 -> TGApply (TGApply1 PSin t0)) (typecheck a0)

        Apply (KnownFunction Sin) args ->
            Err <| InvalidArgCount Sin (List.length args)

        Apply (KnownFunction Cos) _ ->
            Debug.todo "branch 'Apply (KnownFunction Cos) _' not implemented"

        Apply (KnownFunction Tan) _ ->
            Debug.todo "branch 'Apply (KnownFunction Tan) _' not implemented"

        Apply (KnownFunction Asin) _ ->
            Debug.todo "branch 'Apply (KnownFunction Asin) _' not implemented"

        Apply (KnownFunction Acos) _ ->
            Debug.todo "branch 'Apply (KnownFunction Acos) _' not implemented"

        Apply (KnownFunction Atan) _ ->
            Debug.todo "branch 'Apply (KnownFunction Atan) _' not implemented"

        Apply (KnownFunction Atan2) _ ->
            Debug.todo "branch 'Apply (KnownFunction Atan2) _' not implemented"

        Apply (KnownFunction Sinh) _ ->
            Debug.todo "branch 'Apply (KnownFunction Sinh) _' not implemented"

        Apply (KnownFunction Cosh) _ ->
            Debug.todo "branch 'Apply (KnownFunction Cosh) _' not implemented"

        Apply (KnownFunction Tanh) _ ->
            Debug.todo "branch 'Apply (KnownFunction Tanh) _' not implemented"

        Apply (KnownFunction Abs) _ ->
            Debug.todo "branch 'Apply (KnownFunction Abs) _' not implemented"

        Apply (KnownFunction Ln) _ ->
            Debug.todo "branch 'Apply (KnownFunction Ln) _' not implemented"

        Apply (KnownFunction Log10) _ ->
            Debug.todo "branch 'Apply (KnownFunction Log10) _' not implemented"

        Apply (KnownFunction Exp) _ ->
            Debug.todo "branch 'Apply (KnownFunction Exp) _' not implemented"

        Apply (KnownFunction Sign) _ ->
            Debug.todo "branch 'Apply (KnownFunction Sign) _' not implemented"

        Apply (KnownFunction Re) _ ->
            Debug.todo "branch 'Apply (KnownFunction Re) _' not implemented"

        Apply (KnownFunction Im) _ ->
            Debug.todo "branch 'Apply (KnownFunction Im) _' not implemented"

        Apply (KnownFunction Arg) _ ->
            Debug.todo "branch 'Apply (KnownFunction Arg) _' not implemented"

        Apply (KnownFunction Gra) _ ->
            Debug.todo "branch 'Apply (KnownFunction Gra) _' not implemented"

        Apply (KnownFunction Det) _ ->
            Debug.todo "branch 'Apply (KnownFunction Det) _' not implemented"

        Apply (KnownFunction Dd) _ ->
            Debug.todo "branch 'Apply (KnownFunction Dd) _' not implemented"

        Apply (KnownFunction Ii) _ ->
            Debug.todo "branch 'Apply (KnownFunction Ii) _' not implemented"

        Apply (KnownFunction Min) _ ->
            Debug.todo "branch 'Apply (KnownFunction Min) _' not implemented"

        Apply (KnownFunction Max) _ ->
            Debug.todo "branch 'Apply (KnownFunction Max) _' not implemented"

        Apply (KnownFunction Round) _ ->
            Debug.todo "branch 'Apply (KnownFunction Round) _' not implemented"

        Apply (KnownFunction Floor) _ ->
            Debug.todo "branch 'Apply (KnownFunction Floor) _' not implemented"

        Apply (KnownFunction Ceil) _ ->
            Debug.todo "branch 'Apply (KnownFunction Ceil) _' not implemented"

        Apply (KnownFunction Piecewise) _ ->
            Debug.todo "branch 'Apply (KnownFunction Piecewise) _' not implemented"

        Apply (KnownFunction Plot) _ ->
            Debug.todo "branch 'Apply (KnownFunction Plot) _' not implemented"

        Apply (KnownFunction Simplify) _ ->
            Debug.todo "branch 'Apply (KnownFunction Simplify) _' not implemented"

        Apply (KnownFunction StepSimplify) _ ->
            Debug.todo "branch 'Apply (KnownFunction StepSimplify) _' not implemented"

        Apply (KnownFunction Solve) _ ->
            Debug.todo "branch 'Apply (KnownFunction Solve) _' not implemented"

        Apply (KnownFunction Mod) _ ->
            Debug.todo "branch 'Apply (KnownFunction Mod) _' not implemented"

        Apply (KnownFunction Mbrot) _ ->
            Debug.todo "branch 'Apply (KnownFunction Mbrot) _' not implemented"

        Apply (KnownFunction For) _ ->
            Debug.todo "branch 'Apply (KnownFunction For) _' not implemented"

        Apply (KnownFunction Sqrt) _ ->
            Debug.todo "branch 'Apply (KnownFunction Sqrt) _' not implemented"

        Apply (KnownFunction Cbrt) _ ->
            Debug.todo "branch 'Apply (KnownFunction Cbrt) _' not implemented"

        Replace _ _ ->
            Debug.todo "branch 'Replace _ _' not implemented"



-- 5 < <= = >= >
-- 6 + -
-- 7 * /


toStringPrec : Int -> ToGlslExpression -> String
toStringPrec p e =
    let
        paren b c =
            if b then
                "(" ++ c ++ ")"

            else
                c

        noninfix op c =
            paren (p > 10) <| op ++ toStringPrec 11 c

        infixl_ n op l r =
            paren (p > n) <| toStringPrec n l ++ op ++ toStringPrec (n + 1) r

        infixr_ n op l r =
            paren (p > n) <| toStringPrec (n + 1) l ++ op ++ toStringPrec n r

        asVector l =
            let
                opened =
                    l
                        |> List.filterMap
                            (\el ->
                                case el of
                                    TGList [ x ] ->
                                        Just x

                                    _ ->
                                        Nothing
                            )
            in
            if List.length opened == List.length l then
                Just opened

            else
                Nothing
    in
    case e of
        TGVariable v ->
            v

        TGFloat f ->
            String.fromFloat f

        TGInteger v ->
            String.fromInt v

        TGNegate expression ->
            noninfix "-" expression

        TGAdd l (TGNegate r) ->
            infixl_ 6 " - " l r

        TGAdd l (TGFloat pf) ->
            if pf < 0 then
                infixl_ 6 " - " l (TGFloat -pf)

            else
                infixl_ 6 " + " l (TGFloat pf)

        TGAdd l (TGInteger pi) ->
            if pi < 0 then
                infixl_ 6 " - " l (TGInteger -pi)

            else
                infixl_ 6 " + " l (TGInteger pi)

        TGAdd l r ->
            infixl_ 6 " + " l r

        TGRel rel l r ->
            infixl_ 5 (" " ++ relationToString rel ++ " ") l r

        TGBy ((TGInteger _) as l) r ->
            case r of
                TGPower _ _ ->
                    infixl_ 7 "" l r

                TGVariable _ ->
                    infixl_ 7 "" l r

                TGApply _ ->
                    infixl_ 7 "" l r

                _ ->
                    infixl_ 7 "*" l r

        TGBy l r ->
            infixl_ 7 "*" l r

        TGDiv l r ->
            infixl_ 7 "/" l r

        TGPower l (TGInteger 2) ->
            paren (p > 8) <| toStringPrec 9 l ++ "²"

        TGPower l (TGInteger 3) ->
            paren (p > 8) <| toStringPrec 9 l ++ "³"

        TGPower l r ->
            infixr_ 8 "^" l r

        TGApply (TGApply1 name ((TGList _) as ex)) ->
            paren (p > 10) <| functionNameToString (toFunctionName1 name) ++ toStringPrec 0 ex

        TGApply (TGApply1 name ex) ->
            paren (p > 10) <| functionNameToString (toFunctionName1 name) ++ "(" ++ toStringPrec 0 ex ++ ")"

        TGList es ->
            case asVector es of
                Nothing ->
                    "{" ++ String.join ", " (List.map (toStringPrec 0) es) ++ "}"

                Just rs ->
                    "(" ++ String.join ", " (List.map (toStringPrec 0) rs) ++ ")"

        TGApply (TGApply2 _ _ _) ->
            Debug.todo "branch 'TGApply (TGApply2 _ _ _)' not implemented"

        TGApply (TGApply3 _ _ _ _) ->
            Debug.todo "branch 'TGApply (TGApply3 _ _ _ _)' not implemented"


toFunctionName1 : Function1 -> FunctionName
toFunctionName1 pf =
    KnownFunction <|
        case pf of
            PSin ->
                Sin

            PCos ->
                Cos

            PTan ->
                Tan

            PAsin ->
                Asin

            PAcos ->
                Acos

            PAtan ->
                Atan

            PSinh ->
                Sinh

            PCosh ->
                Cosh

            PTanh ->
                Tanh

            PAbs ->
                Abs

            PSqrt ->
                Sqrt

            PCbrt ->
                Cbrt

            PLn ->
                Ln

            PLog10 ->
                Log10

            PExp ->
                Exp

            PSign ->
                Sign

            PRe ->
                Re

            PIm ->
                Im

            PArg ->
                Arg

            PGra ->
                Gra

            PDet ->
                Det

            PRound ->
                Round

            PFloor ->
                Floor

            PCeil ->
                Ceil


greeks : Dict String String
greeks =
    Dict.fromList
        [ ( "alpha", "α" )
        , ( "beta", "β" )
        , ( "delta", "δ" )
        , ( "Delta", "Δ" )
        , ( "epsilon", "ɛ" )
        , ( "eta", "η" )
        , ( "gamma", "γ" )
        , ( "Gamma", "Γ" )
        , ( "iota", "ι" )
        , ( "kappa", "κ" )
        , ( "lambda", "λ" )
        , ( "Lambda", "Λ" )
        , ( "mu", "μ" )
        , ( "nu", "ν" )
        , ( "omega", "ω" )
        , ( "Omega", "Ω" )
        , ( "phi", "φ" )
        , ( "Phi", "Φ" )
        , ( "pi", "π" )
        , ( "Pi", "Π" )
        , ( "psi", "ψ" )
        , ( "Psi", "Ψ" )
        , ( "rho", "ρ" )
        , ( "sigma", "σ" )
        , ( "Sigma", "Σ" )
        , ( "tau", "τ" )
        , ( "theta", "ϑ" )
        , ( "Theta", "Θ" )
        , ( "upsilon", "υ" )
        , ( "xi", "ξ" )
        , ( "Xi", "Ξ" )
        , ( "zeta", "ζ" )
        ]


type alias Context =
    { functions : Trie ( FunctionName, Maybe Int )
    , variables : Trie VariableStatus
    }


type VariableStatus
    = Declared
    | Defined


unaryFunctions : List ( String, KnownFunction )
unaryFunctions =
    let
        trig =
            [ ( "sin", Sin )
            , ( "cos", Cos )
            , ( "tan", Tan )
            , ( "asin", Asin )
            , ( "acos", Acos )
            , ( "atan", Atan )
            , ( "sinh", Sinh )
            , ( "cosh", Cosh )
            , ( "tanh", Tanh )
            ]

        power =
            [ ( "abs", Abs )
            , ( "sign", Sign )
            , ( "sqrt", Sqrt )
            , ( "cbrt", Cbrt )
            , ( "ln", Ln )
            , ( "log10", Log10 )
            , ( "exp", Exp )
            ]

        complex =
            [ ( "re", Re )
            , ( "im", Im )
            , ( "arg", Arg )
            ]

        matrix =
            [ ( "gra", Gra )
            , ( "det", Det )
            ]

        other =
            [ ( "plot", Plot )
            , ( "simplify", Simplify )
            , ( "stepsimplify", StepSimplify )
            , ( "floor", Floor )
            , ( "ceil", Ceil )
            , ( "round", Round )
            ]
    in
    List.concat
        [ power
        , trig
        , complex
        , matrix
        , other
        ]


binaryFunctions : List ( String, KnownFunction )
binaryFunctions =
    [ ( "atan2", Atan2 )
    , ( "dd", Dd )
    , ( "solve", Solve )
    , ( "mod", Mod )
    , ( "mbrot", Mbrot )
    ]


ternaryFunctions : List ( String, KnownFunction )
ternaryFunctions =
    [ ( "pw", Piecewise ) ]


quaternaryFunctions : List ( String, KnownFunction )
quaternaryFunctions =
    [ ( "ii", Ii ) ]


variadicFunctions : List ( String, KnownFunction )
variadicFunctions =
    [ ( "min", Min )
    , ( "max", Max )
    , ( "for", For )
    ]


defaultContext : Context
defaultContext =
    let
        letters =
            List.range (Char.toCode 'a') (Char.toCode 'z')
                |> List.map (Char.fromCode >> String.fromChar)

        map i =
            List.map (\( f, n ) -> ( f, ( KnownFunction n, i ) ))
    in
    { functions =
        Trie.fromList <|
            List.concat
                [ map (Just 1) unaryFunctions
                , map (Just 2) binaryFunctions
                , map (Just 3) ternaryFunctions
                , map (Just 4) quaternaryFunctions
                , map Nothing variadicFunctions
                ]
    , variables = Trie.fromList <| List.map (\v -> ( v, Declared )) <| letters ++ Dict.keys greeks
    }


getFreeVariables : Expression -> Set String
getFreeVariables expr =
    let
        concatMap : List Expression -> Set String
        concatMap =
            List.foldl (\e -> Set.union (getFreeVariables e)) Set.empty
    in
    case expr of
        Variable v ->
            Set.singleton v

        UnaryOperation _ e ->
            getFreeVariables e

        BinaryOperation _ l r ->
            Set.union (getFreeVariables l) (getFreeVariables r)

        RelationOperation _ l r ->
            Set.union (getFreeVariables l) (getFreeVariables r)

        AssociativeOperation _ l r o ->
            Set.union (getFreeVariables l) (getFreeVariables r)
                |> Set.union (concatMap o)

        Apply _ args ->
            concatMap args

        Integer _ ->
            Set.empty

        Float _ ->
            Set.empty

        Lambda x f ->
            Set.diff (getFreeVariables f) (Set.singleton x)

        Replace vars e ->
            getFreeVariables <| fullSubstitute (filterContext vars) e

        List es ->
            concatMap es


functionNameToString : FunctionName -> String
functionNameToString name =
    case name of
        KnownFunction Sin ->
            "sin"

        KnownFunction Cos ->
            "cos"

        KnownFunction Tan ->
            "tan"

        KnownFunction Asin ->
            "asin"

        KnownFunction Acos ->
            "acos"

        KnownFunction Atan ->
            "atan"

        KnownFunction Atan2 ->
            "atan2"

        KnownFunction Sinh ->
            "sinh"

        KnownFunction Cosh ->
            "cosh"

        KnownFunction Tanh ->
            "tanh"

        KnownFunction Abs ->
            "abs"

        KnownFunction Sign ->
            "sign"

        KnownFunction Sqrt ->
            "sqrt"

        KnownFunction Cbrt ->
            "cbrt"

        KnownFunction Ln ->
            "ln"

        KnownFunction Log10 ->
            "log10"

        KnownFunction Det ->
            "det"

        KnownFunction Exp ->
            "exp"

        KnownFunction Re ->
            "re"

        KnownFunction Im ->
            "im"

        KnownFunction Arg ->
            "arg"

        KnownFunction Gra ->
            "gra"

        KnownFunction Dd ->
            "dd"

        KnownFunction Ii ->
            "ii"

        KnownFunction Piecewise ->
            "pw"

        KnownFunction Plot ->
            "plot"

        KnownFunction Simplify ->
            "simplify"

        KnownFunction StepSimplify ->
            "stepsimplify"

        KnownFunction Solve ->
            "solve"

        KnownFunction Floor ->
            "floor"

        KnownFunction Ceil ->
            "ceil"

        KnownFunction Round ->
            "round"

        KnownFunction Min ->
            "min"

        KnownFunction Max ->
            "max"

        KnownFunction Mod ->
            "mod"

        KnownFunction Mbrot ->
            "mbrot"

        KnownFunction For ->
            "for"


pfullSubstitute : Dict String (Maybe ToGlslExpression) -> ToGlslExpression -> ToGlslExpression
pfullSubstitute dict =
    pvisit <| \expr ->
    case expr of
        TGVariable string ->
            Dict.get string dict
                |> Maybe.andThen identity
                |> Maybe.withDefault expr
                |> Just

        _ ->
            Nothing


toTeXString : Expression -> String
toTeXString =
    typecheck
        >> Result.withDefault (TGInteger 0)
        >> toTeXStringPrec 0


toDebugTree : (String -> Never) -> Expression -> Element context msg
toDebugTree todo e =
    let
        vr =
            Element.el
                [ Element.height Element.fill
                , Border.widthEach { bottom = 0, left = 1, right = 0, top = 0 }
                ]
            <|
                Element.none

        go n fs =
            Element.column
                [ Element.WithContext.Font.center
                , Element.spacing 2
                , Element.alignTop
                , Element.padding 2
                ]
                [ Element.el
                    [ Element.width Element.fill
                    , Element.WithContext.Font.center
                    , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                    ]
                  <|
                    Element.text n
                , Element.row [ Element.spacing 2 ] <| List.intersperse vr fs
                ]

        goRec n fs =
            go n <| List.map (toDebugTree todo) fs
    in
    case e of
        Integer i ->
            Element.el [ Element.alignTop ] <| Element.text <| String.fromInt i

        Float f ->
            let
                raw =
                    String.fromFloat f
            in
            Element.el [ Element.alignTop ] <|
                Element.text <|
                    if String.contains "." raw then
                        raw

                    else
                        raw ++ ".0"

        Variable v ->
            Element.el [ Element.alignTop ] <| Element.text v

        Lambda x f ->
            go "λ" [ Element.text x, toDebugTree todo f ]

        UnaryOperation uop c ->
            goRec
                (case uop of
                    Negate ->
                        "-"
                )
                [ c ]

        BinaryOperation bop n d ->
            goRec
                (case bop of
                    Division ->
                        "/"

                    Power ->
                        "^"
                )
                [ n, d ]

        RelationOperation relop l r ->
            goRec (relationToString relop) [ l, r ]

        AssociativeOperation aop l m r ->
            goRec
                (case aop of
                    Addition ->
                        "+"

                    Multiplication ->
                        "*"
                )
                (l :: m :: r)

        Apply f x ->
            goRec (functionNameToString f) x

        Replace r ex ->
            let
                replTree =
                    r
                        |> Dict.toList
                        |> List.filterMap (\( k, v ) -> Maybe.map (Tuple.pair k) v)
                        |> List.map
                            (\( k, v ) ->
                                [ Element.text <| k ++ "="
                                , toDebugTree todo v
                                ]
                            )
                        |> List.intersperse [ Element.text " " ]
                        |> List.concat
            in
            go "repl"
                [ Element.row []
                    (Element.text "[" :: replTree ++ [ Element.text "]" ])
                , toDebugTree todo ex
                ]

        List ls ->
            goRec "list" ls


asMatrixPrint : ToGlslExpression -> Maybe (List (List ToGlslExpression))
asMatrixPrint =
    genericAsMatrix
        (\l ->
            case l of
                TGList u ->
                    Just u

                _ ->
                    Nothing
        )


asMatrix : Expression -> Maybe (List (List Expression))
asMatrix =
    genericAsMatrix <| \e ->
    case e of
        List es ->
            Just es

        _ ->
            Nothing


genericAsMatrix : (a -> Maybe (List a)) -> a -> Maybe (List (List a))
genericAsMatrix asList mx =
    mx
        |> asList
        |> Maybe.andThen
            (\ls ->
                let
                    childLists =
                        List.filterMap asList ls

                    lens =
                        List.map List.length childLists
                in
                if List.length ls == List.length childLists && List.minimum lens == List.maximum lens then
                    Just childLists

                else
                    Nothing
            )


genericAsSquareMatrix : (a -> Maybe (List a)) -> a -> Maybe (List (List a))
genericAsSquareMatrix asList =
    genericAsMatrix asList
        >> Maybe.andThen
            (\rows ->
                if Just (List.length rows) == Maybe.map List.length (List.head rows) then
                    Just rows

                else
                    Nothing
            )


matrixSize : List (List a) -> ( Int, Int )
matrixSize m =
    ( List.length m, List.length <| Maybe.withDefault [] <| List.head m )


genericMatrixAddition :
    { plus : List a -> a
    , asList : a -> Maybe (List a)
    , toList : List a -> a
    }
    -> a
    -> a
    -> Maybe a
genericMatrixAddition { plus, asList, toList } l r =
    case Maybe.map2 Tuple.pair (genericAsMatrix asList l) (genericAsMatrix asList r) of
        Just ( lm, rm ) ->
            if matrixSize lm /= matrixSize rm then
                Nothing

            else
                Just <|
                    toList <|
                        List.map2
                            (\le re ->
                                toList <|
                                    List.map2 (\lc rc -> plus [ lc, rc ])
                                        le
                                        re
                            )
                            lm
                            rm

        _ ->
            Nothing


genericMatrixMultiplication :
    { by : List a -> a
    , plus : List a -> a
    , asList : a -> Maybe (List a)
    , toList : List a -> a
    }
    -> a
    -> a
    -> Maybe a
genericMatrixMultiplication { by, plus, asList, toList } l r =
    case Maybe.map2 Tuple.pair (genericAsMatrix asList l) (genericAsMatrix asList r) of
        Just ( lm, rm ) ->
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

                ( lr, lc ) =
                    matrixSize lm

                ( rr, rc ) =
                    matrixSize rm

                dot x y =
                    plus <| List.map2 (\xe ye -> by [ xe, ye ]) x y
            in
            if lc /= rr then
                Nothing

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
                                |> toList
                        )
                    |> toList
                    |> Just

        _ ->
            Nothing


genericDeterminant : { plus : List a -> a, negate : a -> a, by : List a -> a } -> List (List a) -> Maybe a
genericDeterminant { plus, negate, by } mat =
    case mat of
        [] ->
            Just <|
                plus []

        [ [ single ] ] ->
            Just <|
                single

        [ [ a_, b_ ], [ c_, d_ ] ] ->
            Just <|
                plus [ by [ a_, d_ ], negate <| by [ b_, c_ ] ]

        [ [ a_, b_, c_ ], [ d_, e_, f_ ], [ g_, h_, i_ ] ] ->
            Just <|
                plus
                    [ by [ a_, e_, i_ ]
                    , negate <| by [ a_, f_, h_ ]
                    , negate <| by [ b_, d_, i_ ]
                    , by [ b_, f_, g_ ]
                    , by [ c_, d_, h_ ]
                    , negate <| by [ c_, e_, g_ ]
                    ]

        [ [ a_, b_, c_, d_ ], [ e_, f_, g_, h_ ], [ i_, j_, k_, l_ ], [ m_, n_, o_, p_ ] ] ->
            Just <|
                plus
                    [ by [ a_, f_, k_, p_ ]
                    , negate <| by [ a_, f_, l_, o_ ]
                    , negate <| by [ a_, g_, j_, p_ ]
                    , by [ a_, g_, l_, n_ ]
                    , by [ a_, h_, j_, o_ ]
                    , negate <| by [ a_, h_, k_, n_ ]
                    , negate <| by [ b_, e_, k_, p_ ]
                    , by [ b_, e_, l_, o_ ]
                    , by [ b_, g_, i_, p_ ]
                    , negate <| by [ b_, g_, l_, m_ ]
                    , negate <| by [ b_, h_, i_, o_ ]
                    , by [ b_, h_, k_, m_ ]
                    , by [ c_, e_, j_, p_ ]
                    , negate <| by [ c_, e_, l_, n_ ]
                    , negate <| by [ c_, f_, i_, p_ ]
                    , by [ c_, f_, l_, m_ ]
                    , by [ c_, h_, i_, n_ ]
                    , negate <| by [ c_, h_, j_, m_ ]
                    , negate <| by [ d_, e_, j_, o_ ]
                    , by [ d_, e_, k_, n_ ]
                    , by [ d_, f_, i_, o_ ]
                    , negate <| by [ d_, f_, k_, m_ ]
                    , negate <| by [ d_, g_, i_, n_ ]
                    , by [ d_, g_, j_, m_ ]
                    ]

        _ ->
            Nothing


toTeXStringPrec : Int -> ToGlslExpression -> String
toTeXStringPrec p e =
    let
        paren b c =
            if b then
                "(" ++ c ++ ")"

            else
                c

        noninfix op c =
            paren (p > 10) <| op ++ toTeXStringPrec 11 c

        infixl_ n op l r =
            paren (p > n) <| toTeXStringPrec n l ++ op ++ toTeXStringPrec (n + 1) r

        wrp x =
            if p > 0 then
                "{" ++ x ++ "}"

            else
                x
    in
    wrp <|
        case e of
            TGVariable v ->
                if Dict.member v greeks then
                    "\\" ++ v

                else
                    v

            TGFloat f ->
                String.fromFloat f

            TGInteger v ->
                String.fromInt v

            TGNegate expression ->
                noninfix "-" expression

            TGAdd l (TGNegate r) ->
                infixl_ 6 " - " l r

            TGAdd l (TGBy (TGInteger m) r) ->
                if m < 0 then
                    infixl_ 6 " - " l (TGBy (TGInteger -m) r)

                else
                    infixl_ 6 " + " l (TGBy (TGInteger m) r)

            TGAdd l (TGFloat pf) ->
                if pf < 0 then
                    infixl_ 6 " - " l (TGFloat -pf)

                else
                    infixl_ 6 " + " l (TGFloat pf)

            TGAdd l (TGInteger pi) ->
                if pi < 0 then
                    infixl_ 6 " - " l (TGInteger -pi)

                else
                    infixl_ 6 " + " l (TGInteger pi)

            TGAdd l r ->
                infixl_ 6 " + " l r

            TGRel rel l r ->
                infixl_ 5 (" " ++ relationToString rel ++ " ") l r

            TGBy ((TGInteger _) as l) r ->
                case r of
                    TGPower _ _ ->
                        infixl_ 7 "" l r

                    TGVariable _ ->
                        infixl_ 7 "" l r

                    TGApply (TGApply1 PAbs _) ->
                        infixl_ 7 "\\cdot" l r

                    TGApply (TGApply1 _ _) ->
                        infixl_ 7 "" l r

                    TGApply (TGApply2 _ _ _) ->
                        infixl_ 7 "" l r

                    TGApply (TGApply3 _ _ _ _) ->
                        infixl_ 7 "" l r

                    _ ->
                        infixl_ 7 "\\cdot" l r

            TGBy l r ->
                infixl_ 7 "\\cdot" l r

            TGDiv l r ->
                paren (p > 7) <| "\\frac" ++ toTeXStringPrec 7 l ++ toTeXStringPrec 8 r

            TGPower l (TGDiv (TGInteger 1) (TGInteger 2)) ->
                paren (p > 8) <| "\\sqrt{" ++ toTeXStringPrec 0 l ++ "}"

            TGPower l (TGDiv (TGInteger 1) s) ->
                paren (p > 8) <| "\\sqrt[" ++ toTeXStringPrec 0 s ++ "]{" ++ toTeXStringPrec 0 l ++ "}"

            TGPower (TGApply (TGApply1 name arg)) (TGFloat i) ->
                paren (p > 10) <|
                    "\\mathrm{"
                        ++ functionNameToString (toFunctionName1 name)
                        ++ "}^{"
                        ++ String.fromFloat i
                        ++ "}\\left("
                        ++ toTeXStringPrec 0 arg
                        ++ "\\right)"

            TGPower (TGApply (TGApply1 name args)) (TGInteger i) ->
                paren (p > 10) <|
                    "\\mathrm{"
                        ++ functionNameToString (toFunctionName1 name)
                        ++ "}^{"
                        ++ String.fromInt i
                        ++ "}\\left("
                        ++ toTeXStringPrec 0 args
                        ++ "\\right)"

            TGPower l r ->
                paren (p > 8) <| toTeXStringPrec 9 l ++ "^{" ++ toTeXStringPrec 0 r ++ "}"

            TGApply (TGApply1 PAbs ex) ->
                paren (p > 10) <| "\\left|" ++ toTeXStringPrec 0 ex ++ "\\right|"

            TGApply (TGApply1 PSqrt ex) ->
                paren (p > 10) <| "\\sqrt{" ++ toTeXStringPrec 0 ex ++ "}"

            -- TGApply (KnownFunction (Root n)) [ ex ] ->
            --     paren (p > 10) <| "\\sqrt[" ++ String.fromInt n ++ "]{" ++ toTeXStringPrec 0 ex ++ "}"
            TGApply (TGApply1 name ((TGList _) as ex)) ->
                paren (p > 10) <| "\\mathrm{" ++ functionNameToString (toFunctionName1 name) ++ "}" ++ toTeXStringPrec 0 ex

            TGApply (TGApply1 name ex) ->
                paren (p > 10) <|
                    "\\mathrm{"
                        ++ functionNameToString (toFunctionName1 name)
                        ++ "}\\left("
                        ++ toTeXStringPrec 0 ex
                        ++ "\\right)"

            TGList es ->
                case asMatrixPrint e of
                    Just childLists ->
                        let
                            viewRow : List ToGlslExpression -> String
                            viewRow elements =
                                String.join " & " (List.map (\cell -> "{" ++ toTeXStringPrec 0 cell ++ "}") elements)
                        in
                        "\\begin{pmatrix}" ++ String.join " \\\\ " (List.map viewRow childLists) ++ "\\end{pmatrix}"

                    Nothing ->
                        "\\begin{Bmatrix}" ++ String.join ", & " (List.map (\cell -> "{" ++ toTeXStringPrec 0 cell ++ "}") es) ++ " \\end{Bmatrix}"

            TGApply _ ->
                Debug.todo "branch 'TGApply _' not implemented"


filterContext : Dict comparable (Maybe v) -> Dict comparable v
filterContext =
    Dict.toList
        >> List.filterMap (\( k, mv ) -> Maybe.map (Tuple.pair k) mv)
        >> Dict.fromList
