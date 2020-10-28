module Expression exposing
    ( AssociativeOperation(..)
    , BinaryOperation(..)
    , Expression(..)
    , UnaryOperation(..)
    , defaultContext
    , equals
    , isFunction
    , partialSubstitute
    , toString
    )

import Dict exposing (Dict)


type Expression
    = UnaryOperation UnaryOperation Expression
    | BinaryOperation BinaryOperation Expression Expression
    | AssociativeOperation AssociativeOperation Expression Expression (List Expression)
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


type AssociativeOperation
    = Addition
    | Multiplication


partialSubstitute : Expression -> a
partialSubstitute _ =
    Debug.todo "partialSubstitute"


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

        ( Variable lv, Variable rv ) ->
            lv == rv

        ( Integer li, Integer ri ) ->
            li == ri

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
    | PMinus PrintExpression PrintExpression
    | PBy PrintExpression PrintExpression
    | PDiv PrintExpression PrintExpression
    | PPower PrintExpression PrintExpression
    | PFunc String PrintExpression
    | PReplace (Dict String PrintExpression) PrintExpression
    | PList (List PrintExpression)


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

        AssociativeOperation Multiplication ((Variable v) as l) r [] ->
            if List.member v context then
                PFunc v (toPrintExpression context r)

            else
                PBy (toPrintExpression context l) (toPrintExpression context r)

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

        PMinus l r ->
            infixl_ 6 " - " l r

        PBy ((PInteger _) as l) r ->
            infixl_ 7 "" l r

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

        PFunc name ((PList _) as ex) ->
            paren (p > 10) <| name ++ toStringPrec 0 ex

        PFunc name ex ->
            paren (p > 10) <| name ++ "(" ++ toStringPrec 0 ex ++ ")"

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
            [ "abs", "sqrt" ]

        trig =
            [ "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh" ]
    in
    List.concat
        [ power
        , trig
        , [ "gra" ]
        ]


isFunction : String -> Expression -> Bool
isFunction _ _ =
    False
