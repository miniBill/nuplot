module Glsl.PrettyPrinter exposing (function, statement)

import Glsl.Types exposing (BinaryOperation(..), BooleanOperation(..), Expression(..), ForDirection(..), Function, RelationOperation(..), Statement(..), Type(..), UnaryOperation(..))
import Parser exposing ((|.), (|=), Step(..), Trailing(..))


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
            ++ prettyPrintType fun.returnType
            ++ "T "
            ++ "(\""
            ++ fun.name
            ++ "\" ++ suffix)"
            ++ " "
            ++ String.join " " (List.map (\( t, n ) -> "(" ++ prettyPrintType t ++ "T \"" ++ n ++ "\")") fun.args)
            ++ " <| \\"
            ++ String.join " " (List.map Tuple.second fun.args)
            ++ " -> "
            ++ (statement fun.stat |> String.split "\n" |> String.join " ")

    else
        fun.name
            ++ "Couple = fun"
            ++ String.fromInt (List.length fun.args)
            ++ " "
            ++ prettyPrintType fun.returnType
            ++ "T \""
            ++ fun.name
            ++ "\" "
            ++ String.join " " (List.map (\( t, n ) -> "(" ++ prettyPrintType t ++ "T \"" ++ n ++ "\")") fun.args)
            ++ " <| \\"
            ++ String.join " " (List.map Tuple.second fun.args)
            ++ " ->\n    "
            ++ (statement fun.stat |> String.split "\n" |> String.join "\n    ")
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

        For { var, from, op, to, step, direction } next ->
            case op of
                LessThan ->
                    (if direction == PlusPlus then
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
                        ++ statement step
                        ++ ") <| \\_ ->\n"
                        ++ statement next

                LessThanOrEquals ->
                    (if direction == PlusPlus then
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
                        ++ statement step
                        ++ ") <| \\_ ->\n"
                        ++ statement next

                GreaterThan ->
                    (if direction == PlusPlus then
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
                        ++ statement step
                        ++ ") <| \\_ ->\n"
                        ++ statement next

                GreaterThanOrEquals ->
                    (if direction == PlusPlus then
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
                        ++ statement step
                        ++ ") <| \\_ ->\n"
                        ++ statement next

                Equals ->
                    "TODO branch 'Equals' not implemented"

                NotEquals ->
                    "TODO branch 'NotEquals' not implemented"

                Assign ->
                    "TODO branch 'Assign' not implemented"

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

        Def { type_, var, val } next ->
            "def "
                ++ prettyPrintType type_
                ++ "T \""
                ++ var
                ++ "\" ("
                ++ prettyPrintExpression val
                ++ ") <| \\"
                ++ var
                ++ " -> \n"
                ++ statement next

        Decl _ _ ->
            "TODO branch 'Decl _ _ _' not implemented"


prettyPrintType : Type -> String
prettyPrintType type_ =
    case type_ of
        TFloat ->
            "float"

        TInt ->
            "int"

        TVec2 ->
            "vec2"

        TIVec2 ->
            "ivec2"

        TIVec3 ->
            "ivec3"

        TIVec4 ->
            "ivec4"

        TVec3 ->
            "vec3"

        TVec4 ->
            "vec4"

        TMat3 ->
            "mat3"

        TVoid ->
            "void"

        TBool ->
            "bool"


prettyPrintExpression : Expression -> String
prettyPrintExpression e =
    let
        parens args =
            "(" ++ String.join " " args ++ ")"
    in
    case e of
        Float f ->
            if f == 0 then
                "zero"

            else if f == 1 then
                "one"

            else
                parens [ "float", String.fromFloat f ]

        Int i ->
            parens [ "int", String.fromInt i ]

        Bool b ->
            parens
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
            parens [ "floatCast", prettyPrintExpression arg ]

        Call n args ->
            if String.startsWith "vec" n then
                if List.all ((==) (Int 0)) args then
                    n ++ "Zero"

                else
                    parens <|
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
                    parens <| "atan2_" :: List.map prettyPrintExpression args

                else
                    parens <| "atan_" :: List.map prettyPrintExpression args

            else if List.member n [ "sin", "cos", "radians", "round", "floor", "ceil", "sqrt" ] then
                parens <| (n ++ "_") :: List.map prettyPrintExpression args

            else
                parens <| n :: List.map prettyPrintExpression args

        Arr l r ->
            parens [ "arr", prettyPrintExpression l, prettyPrintExpression r ]

        Ternary c l r ->
            parens [ "ternary\n", prettyPrintExpression c, prettyPrintExpression l, prettyPrintExpression r ]

        UnaryOperation Negate ex ->
            parens [ "negate_", prettyPrintExpression ex ]

        BinaryOperation op l r ->
            parens [ prettyPrintBinaryOperation op, prettyPrintExpression l, prettyPrintExpression r ]

        BooleanOperation op es ->
            parens
                [ prettyPrintBooleanOperation op
                , "["
                , String.join ", " <| List.map prettyPrintExpression es
                , "]"
                ]

        RelationOperation rop l r ->
            parens [ prettyPrintRelationOperation rop, prettyPrintExpression l, prettyPrintExpression r ]

        PostfixIncrement c ->
            parens [ "postfixIncrement", prettyPrintExpression c ]

        PostfixDecrement c ->
            parens [ "postfixDecrement", prettyPrintExpression c ]


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
