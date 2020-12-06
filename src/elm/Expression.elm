module Expression exposing
    ( AssociativeOperation(..)
    , BinaryOperation(..)
    , Context
    , Expression(..)
    , FunctionName(..)
    , Graph(..)
    , KnownFunction(..)
    , RelationOperation(..)
    , UnaryOperation(..)
    , Value(..)
    , defaultContext
    , equals
    , fullSubstitute
    , functionNameToString
    , getFreeVariables
    , graphToString
    , greeks
    , partialSubstitute
    , relationToString
    , toGLString
    , toString
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
    | Relation2D RelationOperation Expression Expression
    | Implicit2D Expression Expression
    | Contour Expression
    | GraphList (List Graph)


type Expression
    = Integer Int
    | Float Float
    | Variable String
    | UnaryOperation UnaryOperation Expression
    | BinaryOperation BinaryOperation Expression Expression
    | RelationOperation RelationOperation Expression Expression
    | AssociativeOperation AssociativeOperation Expression Expression (List Expression)
    | Apply FunctionName (List Expression)
    | Replace (Dict String Expression) Expression
    | List (List Expression)


type Value
    = ErrorValue String
    | SymbolicValue Expression
    | ComplexValue Complex
    | GraphValue Graph
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
      -- Complex
    | Re
    | Im
    | Arg
      -- Matrix
    | Gra
      -- Derivative
    | Dd
    | Ii
      -- Misc
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

                Float fl ->
                    Float fl

                Replace vars e ->
                    Replace (Dict.map (\_ -> visit f) vars) (visit f e)

                List es ->
                    List <| List.map (visit f) es


pvisit : (PrintExpression -> Maybe PrintExpression) -> PrintExpression -> PrintExpression
pvisit f expr =
    case f expr of
        Just r ->
            r

        Nothing ->
            case expr of
                PAtom s ->
                    PAtom s

                PInteger i ->
                    PInteger i

                PFloat f_ ->
                    PFloat f_

                PNegate l ->
                    PNegate (pvisit f l)

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
                    PReplace (Dict.map (\_ -> pvisit f) vars) (pvisit f e)

                PList es ->
                    PList <| List.map (pvisit f) es


partialSubstitute : String -> Expression -> Expression -> Expression
partialSubstitute var val =
    visit <|
        \expr ->
            case expr of
                Replace vars e ->
                    Just <|
                        Replace (Dict.map (\_ v -> partialSubstitute var val v) vars) <|
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
            (Dict.size ls == Dict.size rs)
                && equals le re
                && List.all identity
                    (List.map2
                        (\( lc, ln ) ( rc, rn ) -> lc == rc && equals ln rn)
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

        Relation2D o l r ->
            "Relation2D " ++ toString (RelationOperation o l r)

        Implicit2D l r ->
            "Implicit2D " ++ toString (RelationOperation Equals l r)

        Contour c ->
            "Contour " ++ toString c

        GraphList gs ->
            "GraphList [" ++ String.join ", " (List.map graphToString gs) ++ "]"


type PrintExpression
    = PInteger Int
    | PFloat Float
    | PAtom String
    | PAdd PrintExpression PrintExpression
    | PNegate PrintExpression
    | PBy PrintExpression PrintExpression
    | PDiv PrintExpression PrintExpression
    | PRel String PrintExpression PrintExpression
    | PPower PrintExpression PrintExpression
    | PReplace (Dict String PrintExpression) PrintExpression
    | PList (List PrintExpression)
    | PApply FunctionName (List PrintExpression)


toPrintExpression : Expression -> PrintExpression
toPrintExpression e =
    case e of
        Variable v ->
            PAtom v

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
            PReplace (Dict.map (\_ -> toPrintExpression) vars) <|
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
        PAtom v ->
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

                PAtom _ ->
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
                        (\( k, v ) ->
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
    , variables : Trie ()
    }


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
            [ ( "gra", Gra ) ]

        other =
            [ ( "plot", Plot )
            , ( "simplify", Simplify )
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
    , variables = Trie.fromList <| List.map (\v -> ( v, () )) <| letters ++ Dict.keys greeks
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

        Replace vars e ->
            let
                efree =
                    getFreeVariables e

                efreeNotBound =
                    Set.diff efree (Set.fromList <| Dict.keys vars)

                usedExprs =
                    Dict.values <| Dict.filter (\k _ -> Set.member k efree) vars
            in
            List.foldl Set.union efreeNotBound <| List.map getFreeVariables usedExprs

        List es ->
            concatMap es


toGLString : Expression -> String
toGLString =
    toPrintExpression
        >> toGLStringPrec 0


toGLStringPrec : Int -> PrintExpression -> String
toGLStringPrec p expr =
    let
        paren b c =
            if b then
                "(" ++ c ++ ")"

            else
                c

        noninfix op c =
            paren (p > 10) <| op ++ toGLStringPrec 11 c

        infixl_ n op l r =
            paren (p > n) <| toGLStringPrec n l ++ op ++ toGLStringPrec (n + 1) r

        apply name ex =
            paren (p > 10) <| name ++ "(" ++ String.join ", " (List.map (toGLStringPrec 0) ex) ++ ")"
    in
    case expr of
        PAtom "i" ->
            "vec2(0,1)"

        PAtom "pi" ->
            "vec2(radians(180.0),0.0)"

        PAtom "e" ->
            "vec2(exp(1.0),0)"

        PAtom v ->
            "vec2(" ++ v ++ ",0)"

        PInteger v ->
            "vec2(" ++ String.fromInt v ++ ",0)"

        PFloat f ->
            "vec2(" ++ String.fromFloat f ++ ",0)"

        PNegate expression ->
            noninfix "-" expression

        PAdd l (PNegate r) ->
            infixl_ 6 " - " l r

        PAdd l r ->
            infixl_ 6 " + " l r

        PRel rel l r ->
            "vec2((" ++ toGLStringPrec 10 l ++ ".x " ++ rel ++ " " ++ toGLStringPrec 10 r ++ ".x) ? 1.0 : 0.0,0.0)"

        PBy l r ->
            apply "by" [ l, r ]

        PDiv l r ->
            apply "div" [ l, r ]

        PPower l r ->
            apply "cpow" [ l, r ]

        PApply name ex ->
            apply ("c" ++ functionNameToString name) ex

        PList es ->
            "vec" ++ String.fromInt (List.length es) ++ "(" ++ String.join ", " (List.map (toGLStringPrec 0) es) ++ ")"

        PReplace var e ->
            toGLStringPrec p (pfullSubstitute var e)


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

        KnownFunction Sqrt ->
            "sqrt"

        KnownFunction Ln ->
            "ln"

        KnownFunction Log10 ->
            "log10"

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

        UserFunction u ->
            u


ppartialSubstitute : String -> PrintExpression -> PrintExpression -> PrintExpression
ppartialSubstitute var val =
    pvisit <|
        \expr ->
            case expr of
                PReplace vars e ->
                    Just <|
                        PReplace (Dict.map (\_ v -> ppartialSubstitute var val v) vars) <|
                            if Dict.member var vars then
                                e

                            else
                                ppartialSubstitute var val e

                PAtom string ->
                    Just <|
                        if string == var then
                            val

                        else
                            expr

                _ ->
                    Nothing


pfullSubstitute : Dict String PrintExpression -> PrintExpression -> PrintExpression
pfullSubstitute ctx e =
    List.foldl (\( k, v ) -> ppartialSubstitute k v) e (Dict.toList ctx)
