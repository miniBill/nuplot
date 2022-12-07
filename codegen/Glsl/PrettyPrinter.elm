module Glsl.PrettyPrinter exposing (function, statement)

import Glsl.Types exposing (BinaryOperation(..), BooleanOperation(..), Expression(..), Function, RelationOperation(..), Statement(..), UnaryOperation(..))
import Parser exposing ((|.), (|=), Step(..), Trailing(..), loop)


function : Function -> String
function fun =
    if fun.hasSuffix then
        "("
            ++ fun.name
            ++ "Decl, "
            ++ fun.name
            ++ ") = fun"
            ++ String.fromInt (List.length fun.args)
            ++ " "
            ++ fun.returnType
            ++ "T "
            ++ "(\""
            ++ fun.name
            ++ "\" ++ suffix)"
            ++ " "
            ++ String.join " " (List.map (\( t, n ) -> "(" ++ t ++ "T \"" ++ n ++ "\")") fun.args)
            ++ " <| \\"
            ++ String.join " " (List.map Tuple.second fun.args)
            ++ " -> "
            ++ (statement fun.body |> String.split "\n" |> String.join " ")

    else
        fun.name
            ++ "Couple = fun"
            ++ String.fromInt (List.length fun.args)
            ++ " "
            ++ fun.returnType
            ++ "T \""
            ++ fun.name
            ++ "\" "
            ++ String.join " " (List.map (\( t, n ) -> "(" ++ t ++ "T \"" ++ n ++ "\")") fun.args)
            ++ " <| \\"
            ++ String.join " " (List.map Tuple.second fun.args)
            ++ " ->\n    "
            ++ (statement fun.body |> String.split "\n" |> String.join "\n    ")
            ++ "\n\n"
            ++ fun.name
            ++ "Decl = Tuple.first "
            ++ fun.name
            ++ "Couple\n"
            ++ fun.name
            ++ " = Tuple.second "
            ++ fun.name
            ++ "Couple"


statement : Statement -> String
statement stat =
    case stat of
        Expression e next ->
            "expr "
                ++ prettyPrintExpression e
                ++ " <| \\_ ->\n"
                ++ statement next

        For var from LessThan to step loop next ->
            (if step then
                "for (\""

             else
                "forDown (\""
            )
                ++ var
                ++ "\", "
                ++ prettyPrintExpression from
                ++ ", "
                ++ prettyPrintExpression to
                ++ ") (\\"
                ++ var
                ++ " ->\n"
                ++ statement loop
                ++ ") <| \\_ ->\n"
                ++ statement next

        For var from LessThanOrEquals to step loop next ->
            (if step then
                "forLeq (\""

             else
                "forLeqDown (\""
            )
                ++ var
                ++ ", "
                ++ prettyPrintExpression from
                ++ ", "
                ++ prettyPrintExpression to
                ++ ") (\\"
                ++ var
                ++ " ->\n"
                ++ statement loop
                ++ ") <| \\_ ->\n"
                ++ statement next

        For var from GreaterThan to step loop next ->
            (if step then
                "for (\""

             else
                "forDown (\""
            )
                ++ var
                ++ "\", "
                ++ prettyPrintExpression from
                ++ ", "
                ++ prettyPrintExpression to
                ++ ") (\\"
                ++ var
                ++ " ->\n"
                ++ statement loop
                ++ ") <| \\_ ->\n"
                ++ statement next

        For var from GreaterThanOrEquals to step loop next ->
            (if step then
                "forLeq (\""

             else
                "forLeqDown (\""
            )
                ++ var
                ++ ", "
                ++ prettyPrintExpression from
                ++ ", "
                ++ prettyPrintExpression to
                ++ ") (\\"
                ++ var
                ++ " ->\n"
                ++ statement loop
                ++ ") <| \\_ ->\n"
                ++ statement next

        If cond ifTrue next ->
            "if_ "
                ++ prettyPrintExpression cond
                ++ "(\n"
                ++ statement ifTrue
                ++ ") <| \\_ ->\n"
                ++ statement next

        Return e ->
            "(return <| " ++ prettyPrintExpression e ++ ")\n"

        Nop ->
            "nop\n"

        Def t n v next ->
            "def "
                ++ t
                ++ "T \""
                ++ n
                ++ "\" ("
                ++ prettyPrintExpression v
                ++ ") <| \\"
                ++ n
                ++ " -> \n"
                ++ statement next

        Decl _ _ _ ->
            "TODO branch 'Decl _ _ _' not implemented"

        For _ _ _ _ _ _ _ ->
            "TODO branch 'For _ _ _ _ _ _ _' not implemented"


prettyPrintExpression : Expression -> String
prettyPrintExpression e =
    let
        lisp args =
            "(" ++ String.join " " args ++ ")"
    in
    case e of
        Float f ->
            if f == 0 then
                "zero"

            else if f == 1 then
                "one"

            else
                lisp [ "float", String.fromFloat f ]

        Int i ->
            lisp [ "int", String.fromInt i ]

        Bool b ->
            lisp
                [ "bool"
                , if b then
                    "True"

                  else
                    "False"
                ]

        Dot v sw ->
            prettyPrintExpression v ++ "." ++ sw

        Variable v ->
            v

        Constant c ->
            case String.split "_" c of
                [] ->
                    "constants.???"

                h :: t ->
                    let
                        toTitle s =
                            case String.uncons s of
                                Nothing ->
                                    ""

                                Just ( sh, st ) ->
                                    String.cons (Char.toUpper sh) (String.toLower st)
                    in
                    "constants." ++ String.toLower h ++ String.concat (List.map toTitle t)

        Call "float" [ arg ] ->
            lisp [ "floatCast", prettyPrintExpression arg ]

        Call n args ->
            if String.startsWith "vec" n then
                if List.all ((==) (Int 0)) args then
                    n ++ "Zero"

                else
                    lisp <|
                        n
                            :: List.map
                                (\c ->
                                    case c of
                                        Int i ->
                                            prettyPrintExpression (Float <| toFloat i)

                                        _ ->
                                            prettyPrintExpression c
                                )
                                args

            else if n == "atan" then
                if List.length args == 2 then
                    lisp <| "atan2_" :: List.map prettyPrintExpression args

                else
                    lisp <| "atan_" :: List.map prettyPrintExpression args

            else if List.member n [ "sin", "cos", "radians", "round", "floor", "ceil", "sqrt" ] then
                lisp <| (n ++ "_") :: List.map prettyPrintExpression args

            else
                lisp <| n :: List.map prettyPrintExpression args

        Arr l r ->
            lisp [ "arr", prettyPrintExpression l, prettyPrintExpression r ]

        Ternary c l r ->
            lisp [ "ternary\n", prettyPrintExpression c, prettyPrintExpression l, prettyPrintExpression r ]

        UnaryOperation Negate ex ->
            lisp [ "negate_", prettyPrintExpression ex ]

        BinaryOperation op l r ->
            lisp [ prettyPrintBinaryOperation op, prettyPrintExpression l, prettyPrintExpression r ]

        BooleanOperation op es ->
            lisp
                [ prettyPrintBooleanOperation op
                , "["
                , String.join ", " <| List.map prettyPrintExpression es
                , "]"
                ]

        RelationOperation rop l r ->
            lisp [ prettyPrintRelationOperation rop, prettyPrintExpression l, prettyPrintExpression r ]

        PostfixIncrement c ->
            lisp [ "postfixIncrement", prettyPrintExpression c ]

        PostfixDecrement c ->
            lisp [ "postfixDecrement", prettyPrintExpression c ]


prettyPrintRelationOperation : RelationOperation -> String
prettyPrintRelationOperation op =
    case op of
        LessThan ->
            "lt"

        LessThanOrEquals ->
            "leq"

        Equals ->
            "eq"

        Assign ->
            "assign"

        NotEquals ->
            "neq"

        GreaterThanOrEquals ->
            "geq"

        GreaterThan ->
            "gt"


prettyPrintBooleanOperation : BooleanOperation -> String
prettyPrintBooleanOperation op =
    case op of
        And ->
            "ands"

        Or ->
            "ors"


prettyPrintBinaryOperation : BinaryOperation -> String
prettyPrintBinaryOperation op =
    case op of
        Add ->
            "add"

        Subtract ->
            "subtract"

        By ->
            "by"

        Div ->
            "div"
