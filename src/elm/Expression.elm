module Expression exposing
    ( AssociativeOperation(..)
    , BinaryOperation(..)
    , Expression(..)
    , Graph(..)
    , RelationOperation(..)
    , UnaryOperation(..)
    , defaultContext
    , equals
    , getFreeVariables
    , isFunction
    , partialSubstitute
    , relationToString
    , toString
    , value
    , visit
    )

import Complex exposing (Complex(..))
import Dict exposing (Dict)
import Set exposing (Set)


type Graph
    = Explicit2D Expression
    | Relation2D RelationOperation Expression Expression
    | Implicit2D Expression Expression


type Expression
    = UnaryOperation UnaryOperation Expression
    | BinaryOperation BinaryOperation Expression Expression
    | RelationOperation RelationOperation Expression Expression
    | AssociativeOperation AssociativeOperation Expression Expression (List Expression)
    | Apply String (List Expression)
    | Variable String
    | Integer Int
    | Float Float
    | Replace (Dict String Expression) Expression
    | List (List Expression)


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
    toPrintExpression defaultContext
        >> toStringPrec 0


type PrintExpression
    = PInteger Int
    | PAtom String
    | PAdd PrintExpression PrintExpression
    | PNegate PrintExpression
    | PBy PrintExpression PrintExpression
    | PDiv PrintExpression PrintExpression
    | PRel String PrintExpression PrintExpression
    | PPower PrintExpression PrintExpression
    | PReplace (Dict String PrintExpression) PrintExpression
    | PList (List PrintExpression)
    | PApply String (List PrintExpression)


toPrintExpression : List String -> Expression -> PrintExpression
toPrintExpression context e =
    case e of
        Variable v ->
            PAtom v

        Integer i ->
            PInteger i

        Float f ->
            PAtom <| String.fromFloat f

        UnaryOperation Negate expression ->
            PNegate <| toPrintExpression context expression

        BinaryOperation Power l r ->
            PPower (toPrintExpression context l) (toPrintExpression context r)

        BinaryOperation Division l r ->
            PDiv (toPrintExpression context l) (toPrintExpression context r)

        RelationOperation LessThan l r ->
            PRel "<" (toPrintExpression context l) (toPrintExpression context r)

        RelationOperation LessThanOrEquals l r ->
            PRel "⩽" (toPrintExpression context l) (toPrintExpression context r)

        RelationOperation Equals l r ->
            PRel "=" (toPrintExpression context l) (toPrintExpression context r)

        RelationOperation GreaterThanOrEquals l r ->
            PRel "⩾" (toPrintExpression context l) (toPrintExpression context r)

        RelationOperation GreaterThan l r ->
            PRel ">" (toPrintExpression context l) (toPrintExpression context r)

        AssociativeOperation Addition l r o ->
            List.foldl (\el a -> PAdd a el)
                (PAdd (toPrintExpression context l) (toPrintExpression context r))
                (List.map (toPrintExpression context) o)

        AssociativeOperation Multiplication l r o ->
            List.foldl (\el a -> PBy a el)
                (PBy (toPrintExpression context l) (toPrintExpression context r))
                (List.map (toPrintExpression context) o)

        Replace vars expr ->
            PReplace (Dict.map (\_ -> toPrintExpression context) vars) <|
                toPrintExpression
                    (context ++ Dict.keys (Dict.filter isFunction vars))
                    expr

        List es ->
            PList <| List.map (toPrintExpression context) es

        Apply f es ->
            PApply f <| List.map (toPrintExpression context) es



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
    in
    case e of
        PAtom v ->
            v

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
            paren (p > 10) <| name ++ toStringPrec 0 ex

        PApply name ex ->
            paren (p > 10) <| name ++ "(" ++ String.join ", " (List.map (toStringPrec 0) ex) ++ ")"

        PList es ->
            "{" ++ String.join ", " (List.map (toStringPrec 0) es) ++ "}"

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


defaultContext : List String
defaultContext =
    let
        power =
            [ "abs", "sqrt", "ln" ]

        complex =
            [ "re", "im" ]

        trig =
            [ "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh" ]
    in
    List.concat
        [ power
        , trig
        , complex
        , [ "gra" ]
        ]


isFunction : String -> Expression -> Bool
isFunction _ _ =
    False


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
            Set.diff (getFreeVariables e) (Set.fromList <| Dict.keys vars)

        List es ->
            concatMap es


value : Dict String Complex -> Expression -> Complex
value context expr =
    case expr of
        Variable v ->
            Dict.get v context |> Maybe.withDefault Complex.zero

        UnaryOperation Negate e ->
            Complex.negate <| value context e

        BinaryOperation Division l r ->
            Complex.div (value context l) (value context r)

        BinaryOperation Power l r ->
            Complex.power (value context l) (value context r)

        RelationOperation LessThan l r ->
            Complex.minus (value context r) (value context l)

        RelationOperation LessThanOrEquals l r ->
            Complex.minus (value context r) (value context l)

        RelationOperation Equals l r ->
            Complex.minus (value context r) (value context l)

        RelationOperation GreaterThanOrEquals l r ->
            Complex.minus (value context l) (value context r)

        RelationOperation GreaterThan l r ->
            Complex.minus (value context l) (value context r)

        AssociativeOperation Addition l r o ->
            List.foldl Complex.plus Complex.zero <| List.map (value context) (l :: r :: o)

        AssociativeOperation Multiplication l r o ->
            List.foldl Complex.by Complex.one <| List.map (value context) (l :: r :: o)

        Apply name args ->
            applyValue context name args

        Integer i ->
            Complex.fromReal <| toFloat i

        Float f ->
            Complex.fromReal f

        List _ ->
            Complex.zero

        Replace ctx e ->
            value context <| List.foldl (\( k, v ) -> partialSubstitute k v) e (Dict.toList ctx)


applyValue : Dict String Complex -> String -> List Expression -> Complex
applyValue context name args =
    case ( name, args ) of
        ( "sin", [ e ] ) ->
            Complex.sin <| value context e

        ( "cos", [ e ] ) ->
            Complex.cos <| value context e

        ( "tan", [ e ] ) ->
            Complex.tan <| value context e

        ( "sinh", [ e ] ) ->
            Complex.sinh <| value context e

        ( "cosh", [ e ] ) ->
            Complex.cosh <| value context e

        ( "tanh", [ e ] ) ->
            Complex.tanh <| value context e

        ( "asin", [ e ] ) ->
            Complex.asin <| value context e

        ( "acos", [ e ] ) ->
            Complex.acos <| value context e

        ( "atan", [ e ] ) ->
            Complex.atan <| value context e

        ( "sqrt", [ e ] ) ->
            Complex.sqrt <| value context e

        ( "ln", [ e ] ) ->
            Complex.ln <| value context e

        ( "re", [ e ] ) ->
            Complex.re <| value context e

        ( "im", [ e ] ) ->
            Complex.im <| value context e

        ( "abs", [ e ] ) ->
            Complex.fromReal <| Complex.abs <| value context e

        _ ->
            Complex.zero
