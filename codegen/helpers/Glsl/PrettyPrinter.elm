module Glsl.PrettyPrinter exposing (float, type_)

import Glsl exposing (Type(..))



-- function : Function -> String
-- function fun =
--     if fun.hasSuffix then
--         "("
--             ++ fun.name
--             ++ "Decl, "
--             ++ fun.name
--             ++ ") = fun"
--             ++ String.fromInt (List.length fun.args)
--             ++ " "
--             ++ type_ fun.returnType
--             ++ "T "
--             ++ "(\""
--             ++ fun.name
--             ++ "\" ++ suffix)"
--             ++ " "
--             ++ String.join " " (List.map (\( t, n ) -> "(" ++ type_ t ++ "T \"" ++ n ++ "\")") fun.args)
--             ++ " <| \\"
--             ++ String.join " " (List.map Tuple.second fun.args)
--             ++ " -> "
--             ++ (statement fun.stat |> String.split "\n" |> String.join " ")
--     else
--         fun.name
--             ++ "Couple = fun"
--             ++ String.fromInt (List.length fun.args)
--             ++ " "
--             ++ type_ fun.returnType
--             ++ "T \""
--             ++ fun.name
--             ++ "\" "
--             ++ String.join " " (List.map (\( t, n ) -> "(" ++ type_ t ++ "T \"" ++ n ++ "\")") fun.args)
--             ++ " <| \\"
--             ++ String.join " " (List.map Tuple.second fun.args)
--             ++ " ->\n    "
--             ++ (statement fun.stat |> String.split "\n" |> String.join "\n    ")
--             ++ "\n\n"
--             ++ fun.name
--             ++ "Decl = Tuple.first "
--             ++ fun.name
--             ++ "Couple\n"
--             ++ fun.name
--             ++ " = Tuple.second "
--             ++ fun.name
--             ++ "Couple"
-- statement : Statement -> String
-- statement stat =
--     case stat of
--         Expression e next ->
--             "expr "
--                 ++ prettyToGlslExpression e
--                 ++ " <| \\_ ->\n"
--                 ++ statement next
--         For { var, from, op, to, step, direction } next ->
--             case op of
--                 LessThan ->
--                     (if direction == PlusPlus then
--                         "for (\""
--                      else
--                         "forDown (\""
--                     )
--                         ++ var
--                         ++ "\", "
--                         ++ prettyToGlslExpression from
--                         ++ ", "
--                         ++ prettyToGlslExpression to
--                         ++ ") (\\"
--                         ++ var
--                         ++ " ->\n"
--                         ++ statement step
--                         ++ ") <| \\_ ->\n"
--                         ++ statement next
--                 LessThanOrEquals ->
--                     (if direction == PlusPlus then
--                         "forLeq (\""
--                      else
--                         "forLeqDown (\""
--                     )
--                         ++ var
--                         ++ ", "
--                         ++ prettyToGlslExpression from
--                         ++ ", "
--                         ++ prettyToGlslExpression to
--                         ++ ") (\\"
--                         ++ var
--                         ++ " ->\n"
--                         ++ statement step
--                         ++ ") <| \\_ ->\n"
--                         ++ statement next
--                 GreaterThan ->
--                     (if direction == PlusPlus then
--                         "for (\""
--                      else
--                         "forDown (\""
--                     )
--                         ++ var
--                         ++ "\", "
--                         ++ prettyToGlslExpression from
--                         ++ ", "
--                         ++ prettyToGlslExpression to
--                         ++ ") (\\"
--                         ++ var
--                         ++ " ->\n"
--                         ++ statement step
--                         ++ ") <| \\_ ->\n"
--                         ++ statement next
--                 GreaterThanOrEquals ->
--                     (if direction == PlusPlus then
--                         "forLeq (\""
--                      else
--                         "forLeqDown (\""
--                     )
--                         ++ var
--                         ++ ", "
--                         ++ prettyToGlslExpression from
--                         ++ ", "
--                         ++ prettyToGlslExpression to
--                         ++ ") (\\"
--                         ++ var
--                         ++ " ->\n"
--                         ++ statement step
--                         ++ ") <| \\_ ->\n"
--                         ++ statement next
--                 Equals ->
--                     "TODO branch 'Equals' not implemented"
--                 NotEquals ->
--                     "TODO branch 'NotEquals' not implemented"
--                 Assign ->
--                     "TODO branch 'Assign' not implemented"
--         If cond ifTrue next ->
--             "if_ "
--                 ++ prettyToGlslExpression cond
--                 ++ "(\n"
--                 ++ statement ifTrue
--                 ++ ") <| \\_ ->\n"
--                 ++ statement next
--         Return e ->
--             "(return <| " ++ prettyToGlslExpression e ++ ")\n"
--         Nop ->
--             "nop\n"
--         Def { type_, var, val } next ->
--             "def "
--                 ++ type_ type_
--                 ++ "T \""
--                 ++ var
--                 ++ "\" ("
--                 ++ prettyToGlslExpression val
--                 ++ ") <| \\"
--                 ++ var
--                 ++ " -> \n"
--                 ++ statement next
--         Decl _ _ ->
--             "TODO branch 'Decl _ _ _' not implemented"


type_ : Type -> String
type_ t =
    case t of
        TFloat ->
            "float"

        TInt ->
            "int"

        TVec2 ->
            "vec2"

        TIVec2 ->
            "ivec2"

        TVec3 ->
            "vec3"

        TIVec3 ->
            "ivec3"

        TVec4 ->
            "vec4"

        TIVec4 ->
            "ivec4"

        TMat3 ->
            "mat3"

        TVoid ->
            "void"

        TBool ->
            "bool"

        TIn tt ->
            "in " ++ type_ tt

        TOut tt ->
            "out " ++ type_ tt



-- prettyToGlslExpression : Expression -> String
-- prettyToGlslExpression e =
--     let
--         parens args =
--             "(" ++ String.join " " args ++ ")"
--     in
--     case e of
--         Float f ->
--             if f == 0 then
--                 "zero"
--             else if f == 1 then
--                 "one"
--             else
--                 parens [ "float", String.fromFloat f ]
--         Int i ->
--             parens [ "int", String.fromInt i ]
--         Bool b ->
--             parens
--                 [ "bool"
--                 , if b then
--                     "True"
--                   else
--                     "False"
--                 ]
--         Dot v sw ->
--             prettyToGlslExpression v ++ "." ++ sw
--         Variable v ->
--             v
--         Constant c ->
--             case String.split "_" c of
--                 [] ->
--                     "constants.???"
--                 h :: t ->
--                     let
--                         toTitle s =
--                             case String.uncons s of
--                                 Nothing ->
--                                     ""
--                                 Just ( sh, st ) ->
--                                     String.cons (Char.toUpper sh) (String.toLower st)
--                     in
--                     "constants." ++ String.toLower h ++ String.concat (List.map toTitle t)
--         Call "float" [ arg ] ->
--             parens [ "floatCast", prettyToGlslExpression arg ]
--         Call n args ->
--             if String.startsWith "vec" n then
--                 if List.all ((==) (Int 0)) args then
--                     n ++ "Zero"
--                 else
--                     parens <|
--                         n
--                             :: List.map
--                                 (\c ->
--                                     case c of
--                                         Int i ->
--                                             prettyToGlslExpression (Float <| toFloat i)
--                                         _ ->
--                                             prettyToGlslExpression c
--                                 )
--                                 args
--             else if n == "atan" then
--                 if List.length args == 2 then
--                     parens <| "atan2_" :: List.map prettyToGlslExpression args
--                 else
--                     parens <| "atan_" :: List.map prettyToGlslExpression args
--             else if List.member n [ "sin", "cos", "radians", "round", "floor", "ceil", "sqrt" ] then
--                 parens <| (n ++ "_") :: List.map prettyToGlslExpression args
--             else
--                 parens <| n :: List.map prettyToGlslExpression args
--         Arr l r ->
--             parens [ "arr", prettyToGlslExpression l, prettyToGlslExpression r ]
--         Ternary c l r ->
--             parens [ "ternary\n", prettyToGlslExpression c, prettyToGlslExpression l, prettyToGlslExpression r ]
--         UnaryOperation Negate ex ->
--             parens [ "negate_", prettyToGlslExpression ex ]
--         BinaryOperation op l r ->
--             parens [ prettyPrintBinaryOperation op, prettyToGlslExpression l, prettyToGlslExpression r ]
--         BooleanOperation op es ->
--             parens
--                 [ prettyPrintBooleanOperation op
--                 , "["
--                 , String.join ", " <| List.map prettyToGlslExpression es
--                 , "]"
--                 ]
--         RelationOperation rop l r ->
--             parens [ prettyPrintRelationOperation rop, prettyToGlslExpression l, prettyToGlslExpression r ]
--         PostfixIncrement c ->
--             parens [ "postfixIncrement", prettyToGlslExpression c ]
--         PostfixDecrement c ->
--             parens [ "postfixDecrement", prettyToGlslExpression c ]
-- prettyPrintRelationOperation : RelationOperation -> String
-- prettyPrintRelationOperation op =
--     case op of
--         LessThan ->
--             "lt"
--         LessThanOrEquals ->
--             "leq"
--         Equals ->
--             "eq"
--         Assign ->
--             "assign"
--         NotEquals ->
--             "neq"
--         GreaterThanOrEquals ->
--             "geq"
--         GreaterThan ->
--             "gt"
-- prettyPrintBooleanOperation : BooleanOperation -> String
-- prettyPrintBooleanOperation op =
--     case op of
--         And ->
--             "ands"
--         Or ->
--             "ors"
-- prettyPrintBinaryOperation : BinaryOperation -> String
-- prettyPrintBinaryOperation op =
--     case op of
--         Add ->
--             "add"
--         Subtract ->
--             "subtract"
--         By ->
--             "by"
--         Div ->
--             "div"


float : Float -> String
float f =
    let
        s : String
        s =
            String.fromFloat f
    in
    if String.contains "." s || String.contains "e" s then
        s

    else
        s ++ "."
