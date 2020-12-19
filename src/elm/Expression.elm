module Expression exposing
    ( AssociativeOperation(..)
    , BinaryOperation(..)
    , Context
    , Expression(..)
    , FunctionName(..)
    , Graph(..)
    , KnownFunction(..)
    , PrintExpression(..)
    , RelationOperation(..)
    , UnaryOperation(..)
    , Value(..)
    , VariableStatus(..)
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
    , graphToString
    , partialSubstitute
    , pfullSubstitute
    , toPrintExpression
    , toString
    , toTeXString
    , visit
    )

import Char
import Complex exposing (Complex(..))
import Dict exposing (Dict)
import List
import Set exposing (Set)
import Trie exposing (Trie)


type Graph
    = Explicit2D Expression
    | Relation2D Expression
    | Implicit2D Expression Expression
    | Implicit3D Expression
    | Contour Expression
    | GraphList (List Graph)


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


type Value
    = ErrorValue String
    | SymbolicValue Expression
    | ComplexValue Complex
    | GraphValue Graph
    | LambdaValue String Value
    | ListValue (List Value)


type FunctionName
    = KnownFunction KnownFunction
    | UserFunction String


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
    | Round
    | Floor
    | Ceiling
    | Pw
    | Plot
    | Simplify


type UnaryOperation
    = Negate


type BinaryOperation
    = Division
    | Power


type RelationOperation
    = LessThan
    | LessThanOrEquals
    | Equals
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

                UnaryOperation uop e ->
                    UnaryOperation uop <| visit f e

                BinaryOperation bop l r ->
                    BinaryOperation bop (visit f l) (visit f r)

                RelationOperation rop l r ->
                    RelationOperation rop (visit f l) (visit f r)

                AssociativeOperation aop l r o ->
                    AssociativeOperation aop (visit f l) (visit f r) (List.map (visit f) o)

                Apply func e ->
                    Apply func <| List.map (visit f) e

                Variable v ->
                    Variable v

                Lambda x e ->
                    Lambda x <| visit f e

                Float fl ->
                    Float fl

                Replace vars e ->
                    Replace (Dict.map (\_ -> Maybe.map (visit f)) vars) (visit f e)

                List es ->
                    List <| List.map (visit f) es


pvisit : (PrintExpression -> Maybe PrintExpression) -> PrintExpression -> PrintExpression
pvisit f expr =
    case f expr of
        Just r ->
            r

        Nothing ->
            case expr of
                PVariable s ->
                    PVariable s

                PInteger i ->
                    PInteger i

                PFloat f_ ->
                    PFloat f_

                PNegate l ->
                    PNegate (pvisit f l)

                PLambda x e ->
                    PLambda x (pvisit f e)

                PAdd l r ->
                    PAdd (pvisit f l) (pvisit f r)

                PBy l r ->
                    PBy (pvisit f l) (pvisit f r)

                PDiv l r ->
                    PDiv (pvisit f l) (pvisit f r)

                PPower l r ->
                    PPower (pvisit f l) (pvisit f r)

                PRel o l r ->
                    PRel o (pvisit f l) (pvisit f r)

                PApply func e ->
                    PApply func <| List.map (pvisit f) e

                PReplace vars e ->
                    PReplace (Dict.map (\_ -> Maybe.map <| pvisit f) vars) (pvisit f e)

                PList es ->
                    PList <| List.map (pvisit f) es


partialSubstitute : String -> Expression -> Expression -> Expression
partialSubstitute var val =
    visit <|
        \expr ->
            case expr of
                Replace vars e ->
                    Just <|
                        Replace (Dict.map (\_ -> Maybe.map (partialSubstitute var val)) vars) <|
                            if Dict.member var vars then
                                e

                            else
                                partialSubstitute var val e

                Variable string ->
                    Just <|
                        if string == var then
                            val

                        else
                            expr

                _ ->
                    Nothing


fullSubstitute : Dict String Expression -> Expression -> Expression
fullSubstitute ctx e =
    List.foldl (\( k, v ) -> partialSubstitute k v) e (Dict.toList ctx)


equals : Expression -> Expression -> Bool
equals l r =
    case ( l, r ) of
        ( UnaryOperation lop le, UnaryOperation rop re ) ->
            (lop == rop) && equals le re

        ( BinaryOperation lop ll lr, BinaryOperation rop rl rr ) ->
            (lop == rop)
                && equals ll rl
                && equals lr rr

        ( AssociativeOperation lop ll lr lo, AssociativeOperation rop rl rr ro ) ->
            (lop == rop)
                && equals ll rl
                && equals lr rr
                && listEquals lo ro

        ( Apply lname largs, Apply rname rargs ) ->
            lname == rname && listEquals largs rargs

        ( Variable lv, Variable rv ) ->
            lv == rv

        ( Integer li, Integer ri ) ->
            li == ri

        ( Float lf, Float rf ) ->
            lf == rf

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

        ( List ls, List rs ) ->
            listEquals ls rs

        _ ->
            False


listEquals : List Expression -> List Expression -> Bool
listEquals ls rs =
    (List.length ls == List.length rs)
        && List.all identity (List.map2 equals ls rs)


toString : Expression -> String
toString =
    toPrintExpression
        >> toStringPrec 0


graphToString : Graph -> String
graphToString g =
    case g of
        Explicit2D e ->
            "Explicit2D " ++ toString e

        Relation2D e ->
            "Relation2D " ++ toString e

        Implicit2D l r ->
            "Implicit2D " ++ toString (RelationOperation Equals l r)

        Implicit3D e ->
            "Implicit3D " ++ toString e

        Contour c ->
            "Contour " ++ toString c

        GraphList gs ->
            "GraphList [" ++ String.join ", " (List.map graphToString gs) ++ "]"


type PrintExpression
    = PInteger Int
    | PFloat Float
    | PVariable String
    | PLambda String PrintExpression
    | PAdd PrintExpression PrintExpression
    | PNegate PrintExpression
    | PBy PrintExpression PrintExpression
    | PDiv PrintExpression PrintExpression
    | PRel String PrintExpression PrintExpression
    | PPower PrintExpression PrintExpression
    | PReplace (Dict String (Maybe PrintExpression)) PrintExpression
    | PList (List PrintExpression)
    | PApply FunctionName (List PrintExpression)


toPrintExpression : Expression -> PrintExpression
toPrintExpression e =
    case e of
        Variable v ->
            PVariable v

        Lambda x f ->
            PLambda x <| toPrintExpression f

        Integer i ->
            PInteger i

        Float f ->
            PFloat f

        UnaryOperation Negate expression ->
            PNegate <| toPrintExpression expression

        BinaryOperation Power l r ->
            PPower (toPrintExpression l) (toPrintExpression r)

        BinaryOperation Division l r ->
            PDiv (toPrintExpression l) (toPrintExpression r)

        RelationOperation op l r ->
            PRel (relationToString op) (toPrintExpression l) (toPrintExpression r)

        AssociativeOperation Addition l r o ->
            List.foldl (\el a -> PAdd a el)
                (PAdd (toPrintExpression l) (toPrintExpression r))
                (List.map toPrintExpression o)

        AssociativeOperation Multiplication l r o ->
            List.foldl (\el a -> PBy a el)
                (PBy (toPrintExpression l) (toPrintExpression r))
                (List.map toPrintExpression o)

        Replace vars expr ->
            PReplace (Dict.map (\_ -> Maybe.map toPrintExpression) vars) <|
                toPrintExpression expr

        List es ->
            PList <| List.map toPrintExpression es

        Apply f es ->
            PApply f <| List.map toPrintExpression es



-- 5 < <= = >= >
-- 6 + -
-- 7 * /


toStringPrec : Int -> PrintExpression -> String
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
                                    PList [ x ] ->
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
        PVariable v ->
            v

        PFloat f ->
            String.fromFloat f

        PInteger v ->
            String.fromInt v

        PNegate expression ->
            noninfix "-" expression

        PAdd l (PNegate r) ->
            infixl_ 6 " - " l r

        PAdd l r ->
            infixl_ 6 " + " l r

        PRel rel l r ->
            infixl_ 5 (" " ++ rel ++ " ") l r

        PBy ((PInteger _) as l) r ->
            case r of
                PPower _ _ ->
                    infixl_ 7 "" l r

                PVariable _ ->
                    infixl_ 7 "" l r

                PApply _ _ ->
                    infixl_ 7 "" l r

                _ ->
                    infixl_ 7 "*" l r

        PBy l r ->
            infixl_ 7 "*" l r

        PDiv l r ->
            infixl_ 7 "/" l r

        PPower l (PInteger 2) ->
            paren (p > 8) <| toStringPrec 9 l ++ "²"

        PPower l (PInteger 3) ->
            paren (p > 8) <| toStringPrec 9 l ++ "³"

        PPower l r ->
            infixr_ 8 "^" l r

        PLambda x ((PApply fn [ PVariable v ]) as f) ->
            if v == x then
                functionNameToString fn

            else
                paren (p > 0) <| x ++ " => " ++ toStringPrec 0 f

        PLambda x f ->
            paren (p > 0) <| x ++ " => " ++ toStringPrec 0 f

        PApply name [ (PList _) as ex ] ->
            paren (p > 10) <| functionNameToString name ++ toStringPrec 0 ex

        PApply name ex ->
            paren (p > 10) <| functionNameToString name ++ "(" ++ String.join ", " (List.map (toStringPrec 0) ex) ++ ")"

        PList es ->
            case asVector es of
                Nothing ->
                    "{" ++ String.join ", " (List.map (toStringPrec 0) es) ++ "}"

                Just rs ->
                    "(" ++ String.join ", " (List.map (toStringPrec 0) rs) ++ ")"

        PReplace var expr ->
            "["
                ++ String.join "; "
                    (List.map
                        (\( k, mv ) ->
                            case mv of
                                Nothing ->
                                    "!" ++ k

                                Just v ->
                                    let
                                        r =
                                            toStringPrec 0 v
                                    in
                                    String.join
                                        (if String.contains " " r then
                                            " "

                                         else
                                            ""
                                        )
                                        [ k, "=", r ]
                        )
                     <|
                        Dict.toList var
                    )
                ++ "] "
                ++ toStringPrec 0 expr


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
    { functions : Trie ( FunctionName, Int )
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
            , ( "floor", Floor )
            , ( "ceil", Ceiling )
            , ( "ceiling", Ceiling )
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
    ]


ternaryFunctions : List ( String, KnownFunction )
ternaryFunctions =
    [ ( "pw", Pw ) ]


quaternaryFunctions : List ( String, KnownFunction )
quaternaryFunctions =
    [ ( "ii", Ii ) ]


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
                [ map 1 unaryFunctions
                , map 2 binaryFunctions
                , map 3 ternaryFunctions
                , map 4 quaternaryFunctions
                ]
    , variables = Trie.fromList <| List.map (\v -> ( v, Declared )) <| letters ++ Dict.keys greeks
    }


getFreeVariables : Expression -> Set String
getFreeVariables expr =
    let
        concatMap =
            List.foldl (Set.union << getFreeVariables) Set.empty
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

        KnownFunction Pw ->
            "pw"

        KnownFunction Plot ->
            "plot"

        KnownFunction Simplify ->
            "simplify"

        KnownFunction Floor ->
            "floor"

        KnownFunction Ceiling ->
            "ceiling"

        KnownFunction Round ->
            "round"

        UserFunction u ->
            u


ppartialSubstitute : String -> PrintExpression -> PrintExpression -> PrintExpression
ppartialSubstitute var val =
    pvisit <|
        \expr ->
            case expr of
                PReplace vars e ->
                    Just <|
                        PReplace (Dict.map (\_ -> Maybe.map <| ppartialSubstitute var val) vars) <|
                            if Dict.member var vars then
                                e

                            else
                                ppartialSubstitute var val e

                PVariable string ->
                    Just <|
                        if string == var then
                            val

                        else
                            expr

                _ ->
                    Nothing


pfullSubstitute : Dict String (Maybe PrintExpression) -> PrintExpression -> PrintExpression
pfullSubstitute ctx e =
    List.foldl
        (\( k, mv ) ->
            case mv of
                Nothing ->
                    identity

                Just v ->
                    ppartialSubstitute k v
        )
        e
        (Dict.toList ctx)


toTeXString : Expression -> String
toTeXString =
    toPrintExpression
        >> toTeXStringPrec 0


asMatrixPrint : PrintExpression -> Maybe (List (List PrintExpression))
asMatrixPrint =
    genericAsMatrix
        (\l ->
            case l of
                PList u ->
                    Just u

                _ ->
                    Nothing
        )


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


toTeXStringPrec : Int -> PrintExpression -> String
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
            PVariable v ->
                if Dict.member v greeks then
                    "\\" ++ v

                else
                    v

            PFloat f ->
                String.fromFloat f

            PInteger v ->
                String.fromInt v

            PNegate expression ->
                noninfix "-" expression

            PAdd l (PNegate r) ->
                infixl_ 6 " - " l r

            PAdd l r ->
                infixl_ 6 " + " l r

            PRel rel l r ->
                infixl_ 5 (" " ++ rel ++ " ") l r

            PLambda x ((PApply fn [ PVariable v ]) as f) ->
                if v == x then
                    functionNameToString fn

                else
                    paren (p > 0) <| x ++ " \\RightArrow " ++ toTeXStringPrec 0 f

            PLambda x f ->
                paren (p > 0) <| x ++ " \\RightArrow " ++ toTeXStringPrec 0 f

            PBy ((PInteger _) as l) r ->
                case r of
                    PPower _ _ ->
                        infixl_ 7 "" l r

                    PVariable _ ->
                        infixl_ 7 "" l r

                    PApply (KnownFunction Abs) _ ->
                        infixl_ 7 "\\cdot" l r

                    PApply _ _ ->
                        infixl_ 7 "" l r

                    _ ->
                        infixl_ 7 "\\cdot" l r

            PBy l r ->
                infixl_ 7 "\\cdot" l r

            PDiv l r ->
                paren (p > 7) <| "\\frac" ++ toTeXStringPrec 7 l ++ toTeXStringPrec 8 r

            PPower l r ->
                paren (p > 8) <| toTeXStringPrec 9 l ++ "^{" ++ toTeXStringPrec 0 r ++ "}"

            PApply (KnownFunction Abs) [ ex ] ->
                paren (p > 10) <| "\\left|" ++ toTeXStringPrec 0 ex ++ "\\right|"

            PApply name [ (PList _) as ex ] ->
                paren (p > 10) <| "\\mathrm{" ++ functionNameToString name ++ "}" ++ toTeXStringPrec 0 ex

            PApply name ex ->
                paren (p > 10) <|
                    "\\mathrm{"
                        ++ functionNameToString name
                        ++ "}\\left("
                        ++ String.join ", " (List.map (toTeXStringPrec 0) ex)
                        ++ "\\right)"

            PList es ->
                case asMatrixPrint e of
                    Just childLists ->
                        let
                            viewRow =
                                String.join " & " << List.map (\cell -> "{" ++ toTeXStringPrec 0 cell ++ "}")
                        in
                        "\\begin{pmatrix}" ++ String.join " \\\\ " (List.map viewRow childLists) ++ "\\end{pmatrix}"

                    Nothing ->
                        "\\begin{Bmatrix}" ++ String.join ", & " (List.map (\cell -> "{" ++ toTeXStringPrec 0 cell ++ "}") es) ++ " \\end{Bmatrix}"

            PReplace var expr ->
                "\\begin{bmatrix}"
                    ++ String.join " \\\\ "
                        (List.map
                            (\( k, mv ) ->
                                case mv of
                                    Nothing ->
                                        "!" ++ k

                                    Just v ->
                                        let
                                            r =
                                                toTeXStringPrec 0 v
                                        in
                                        String.join
                                            (if String.contains " " r then
                                                " "

                                             else
                                                ""
                                            )
                                            [ k, "=", r ]
                            )
                         <|
                            Dict.toList var
                        )
                    ++ "\\end{bmatrix} "
                    ++ toTeXStringPrec 0 expr


filterContext : Dict comparable (Maybe v) -> Dict comparable v
filterContext =
    Dict.toList
        >> List.filterMap (\( k, mv ) -> Maybe.map (Tuple.pair k) mv)
        >> Dict.fromList
