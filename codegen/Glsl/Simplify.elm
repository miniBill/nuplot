module Glsl.Simplify exposing (expression, function, statement)

import Glsl.Types exposing (BooleanOperation, Expression(..), Function, Statement(..))


function : Function -> Function
function f =
    { f | body = statement f.body }


statement : Statement -> Statement
statement s =
    case s of
        Nop ->
            Nop

        Expression e n ->
            Expression (expression e) (statement n)

        For v from rel to step inner next ->
            For v (expression from) rel (expression to) step (statement inner) (statement next)

        If cond t f ->
            If (expression cond) (statement t) (statement f)

        Return e ->
            Return (expression e)

        Def t n v next ->
            Def t n (expression v) (statement next)

        Decl t n next ->
            Decl t n (statement next)


expression : Expression -> Expression
expression e =
    case e of
        BooleanOperation op es ->
            es
                |> List.map expression
                |> List.concatMap
                    (\c ->
                        case c of
                            BooleanOperation cop ces ->
                                if op == cop then
                                    ces

                                else
                                    [ c ]

                            _ ->
                                [ c ]
                    )
                |> BooleanOperation op

        _ ->
            e
