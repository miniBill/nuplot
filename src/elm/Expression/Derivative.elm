module Expression.Derivative exposing (derivative)

import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), UnaryOperation(..), filterContext, fullSubstitute)
import Expression.Utils exposing (by, byShort, cos_, cosh_, div, e, exp_, ipowShort, ln_, log10_, minus, negate_, one, plus, pow, sign, sin_, sinh_, sqrt_, square, tan_, zero)


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

        Lambda x f ->
            Lambda x f

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
                byShort [ ex, ipowShort (Variable v) (i - 1) ]

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
            derivative var <| fullSubstitute (filterContext ctx) e

        Apply (KnownFunction f) [ x ] ->
            case x of
                Variable xv ->
                    if xv == var then
                        knownDerivative f x

                    else
                        zero

                _ ->
                    by [ knownDerivative f x, derivative var x ]

        Apply (KnownFunction For) args ->
            case Expression.Utils.runForLoop args of
                Just result ->
                    derivative var <| List result

                Nothing ->
                    Variable "TODO"

        Apply _ _ ->
            --TODO
            Variable "TODO"


knownDerivative : KnownFunction -> Expression -> Expression
knownDerivative name x =
    case name of
        Sin ->
            cos_ x

        Cos ->
            negate_ <| sin_ x

        Tan ->
            plus [ one, square <| tan_ x ]

        Asin ->
            div one <| sqrt_ (minus one (square x))

        Acos ->
            negate_ <| div one <| sqrt_ (minus one (square x))

        Atan ->
            div one (plus [ one, square x ])

        Atan2 ->
            --TODO
            Variable "TODO"

        Sinh ->
            cosh_ x

        Cosh ->
            sinh_ x

        Tanh ->
            div one <| square <| cosh_ x

        Abs ->
            sign x

        Root n ->
            let
                ni =
                    Integer n
            in
            div
                (pow x
                    (div
                        (Integer (1 - n))
                        ni
                    )
                )
                ni

        Ln ->
            div one x

        Log10 ->
            by [ div one x, log10_ (Variable "e") ]

        Exp ->
            exp_ x

        Sign ->
            zero

        Re ->
            --TODO
            Variable "TODO"

        Im ->
            --TODO
            Variable "TODO"

        Arg ->
            --TODO
            Variable "TODO"

        Gra ->
            --TODO
            Variable "TODO"

        Det ->
            --TODO
            Variable "TODO"

        Dd ->
            --TODO
            Variable "TODO"

        Ii ->
            --TODO
            Variable "TODO"

        Min ->
            --TODO
            Variable "TODO"

        Max ->
            --TODO
            Variable "TODO"

        Round ->
            zero

        Floor ->
            zero

        Ceiling ->
            zero

        Pw ->
            --TODO
            Variable "TODO"

        Plot ->
            --TODO
            Variable "TODO"

        Simplify ->
            --TODO
            Variable "TODO"

        StepSimplify ->
            --TODO
            Variable "TODO"

        Solve ->
            --TODO
            Variable "TODO"

        Mod ->
            --TODO
            Variable "TODO"

        Mbrot ->
            --TODO
            Variable "TODO"

        For ->
            --TODO
            Variable "TODO"
