module Expression.Derivative exposing (derivative)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), UnaryOperation(..), partialSubstitute)
import Expression.Utils exposing (by, div, ipow, ln_, minus, one, plus, pow, square, zero)


derivative : String -> Expression -> Expression
derivative var expr =
    case expr of
        Integer _ ->
            zero

        Float _ ->
            zero

        Variable v ->
            if v == var then
                one

            else
                zero

        List ls ->
            List <| List.map (derivative var) ls

        AssociativeOperation Addition l m rs ->
            plus <| List.map (derivative var) <| l :: m :: rs

        AssociativeOperation Multiplication l r [] ->
            plus [ by [ derivative var l, r ], by [ l, derivative var r ] ]

        AssociativeOperation Multiplication l r rs ->
            derivative var (by [ by [ l, r ], by rs ])

        UnaryOperation Negate e ->
            UnaryOperation Negate (derivative var e)

        BinaryOperation Division f g ->
            let
                fpg =
                    by [ derivative var f, g ]

                fgp =
                    by [ f, derivative var g ]
            in
            div
                (minus fpg fgp)
                (square g)

        BinaryOperation Power (Variable v) ((Integer i) as ex) ->
            if v /= var then
                zero

            else
                by [ ex, ipow (Variable v) (i - 1) ]

        BinaryOperation Power f g ->
            let
                fp =
                    derivative var f

                gp =
                    derivative var g
            in
            by
                [ pow f g
                , plus
                    [ by [ fp, div g f ]
                    , by [ gp, ln_ f ]
                    ]
                ]

        RelationOperation _ _ _ ->
            zero

        Replace ctx e ->
            derivative var <| List.foldl (\( k, v ) -> partialSubstitute k v) e (Dict.toList ctx)

        Apply _ _ ->
            zero
