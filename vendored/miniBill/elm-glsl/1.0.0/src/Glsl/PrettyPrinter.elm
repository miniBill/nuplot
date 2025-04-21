module Glsl.PrettyPrinter exposing (binaryOperation, declaration, expr, float, stat, type_, unaryOperation)

import Glsl exposing (BinaryOperation(..), Declaration(..), Expr(..), RelationOperation(..), Stat(..), Type(..), UnaryOperation(..))


stat : Int -> Stat -> String
stat i c =
    case c of
        Nop ->
            "{}"

        Block a b children ->
            (indent i "{"
                :: List.map (stat (i + 1)) (a :: b :: children)
                ++ [ indent i "}" ]
            )
                |> String.join "\n"

        If cond t ->
            indent i ("if (" ++ expr cond ++ ") ") ++ String.trimLeft (stat (i + 1) t)

        IfElse cond t ((If _ _) as f) ->
            [ indent i ("if (" ++ expr cond ++ ") {")
            , stat (i + 1) t
            , indent i <| "} else " ++ String.trimLeft (stat i f)
            ]
                |> String.join "\n"

        IfElse cond t ((IfElse _ _ _) as f) ->
            [ indent i ("if (" ++ expr cond ++ ") {")
            , stat (i + 1) t
            , indent i "} else {"
            , stat (i + 1) f
            , indent i "}"
            ]
                |> String.join "\n"

        IfElse cond t f ->
            [ indent i ("if (" ++ expr cond ++ ") {")
            , stat (i + 1) t
            , indent i "} else {"
            , stat (i + 1) f
            , indent i "}"
            ]
                |> String.join "\n"

        For init check step loop ->
            [ indent i ("for ( " ++ maybeStat 0 init ++ "; " ++ expr check ++ "; " ++ expr step ++ ") {")
            , stat (i + 1) loop
            , indent i "}"
            ]
                |> String.join "\n"

        Return e ->
            indent i <| "return " ++ expr e ++ ";"

        Break ->
            indent i "break;"

        Continue ->
            indent i "continue;"

        ExpressionStatement e ->
            indent i (expr e ++ ";")

        Decl t n (Just e) ->
            indent i (type_ t ++ " " ++ n ++ " = " ++ expr e ++ ";")

        Decl t n Nothing ->
            indent i (type_ t ++ " " ++ n ++ ";")


maybeStat : Int -> Maybe Stat -> String
maybeStat i c =
    case c of
        Nothing ->
            ""

        Just s ->
            stat i s


indent : Int -> String -> String
indent i line =
    String.repeat (4 * i) " " ++ line


expr : Expr -> String
expr root =
    let
        showParen : Bool -> String -> String
        showParen show e =
            if show then
                "(" ++ e ++ ")"

            else
                e

        infixl_ : Int -> Int -> Expr -> String -> Expr -> String
        infixl_ n p l op r =
            showParen (p > n) (go n l ++ " " ++ op ++ " " ++ go (n + 1) r)

        infixr_ : Int -> Int -> Expr -> String -> Expr -> String
        infixr_ n p l op r =
            showParen (p > n) (go (n + 1) l ++ " " ++ op ++ " " ++ go n r)

        go : Int -> Expr -> String
        go p tree =
            case tree of
                Bool b ->
                    if b then
                        "true"

                    else
                        "false"

                Float f ->
                    float f

                Double d ->
                    double d

                Int i ->
                    String.fromInt i

                Uint u ->
                    String.fromInt u

                Variable v ->
                    v

                BinaryOperation l ArraySubscript r ->
                    showParen (p > 15) (go 15 l ++ "[" ++ go 16 r ++ "]")

                Call l r ->
                    showParen (p > 15) (go 15 l ++ "(" ++ String.join ", " (List.map (go 16) r) ++ ")")

                Dot l r ->
                    showParen (p > 15) (go 15 l ++ "." ++ r)

                UnaryOperation PostfixIncrement r ->
                    showParen (p > 15) (go 16 r ++ "++")

                UnaryOperation PostfixDecrement r ->
                    showParen (p > 15) (go 16 r ++ "--")

                UnaryOperation PrefixIncrement r ->
                    showParen (p > 14) ("++" ++ go 15 r)

                UnaryOperation PrefixDecrement r ->
                    showParen (p > 14) ("--" ++ go 15 r)

                UnaryOperation Plus r ->
                    showParen (p > 14) ("+" ++ go 15 r)

                UnaryOperation Negate r ->
                    showParen (p > 14) ("-" ++ go 15 r)

                UnaryOperation Invert r ->
                    showParen (p > 14) ("~" ++ go 15 r)

                UnaryOperation Not r ->
                    showParen (p > 14) ("!" ++ go 15 r)

                BinaryOperation l By r ->
                    infixl_ 13 p l "*" r

                BinaryOperation l Div r ->
                    infixl_ 13 p l "/" r

                BinaryOperation l Mod r ->
                    infixl_ 13 p l "%" r

                BinaryOperation l Add r ->
                    infixl_ 12 p l "+" r

                BinaryOperation l Subtract r ->
                    infixl_ 12 p l "-" r

                BinaryOperation l ShiftLeft r ->
                    infixl_ 11 p l "<<" r

                BinaryOperation l ShiftRight r ->
                    infixl_ 11 p l ">>" r

                BinaryOperation l (RelationOperation LessThan) r ->
                    infixl_ 10 p l "<" r

                BinaryOperation l (RelationOperation LessThanOrEquals) r ->
                    infixl_ 10 p l "<=" r

                BinaryOperation l (RelationOperation GreaterThan) r ->
                    infixl_ 10 p l ">" r

                BinaryOperation l (RelationOperation GreaterThanOrEquals) r ->
                    infixl_ 10 p l ">=" r

                BinaryOperation l (RelationOperation Equals) r ->
                    infixl_ 9 p l "==" r

                BinaryOperation l (RelationOperation NotEquals) r ->
                    infixl_ 9 p l "!=" r

                BinaryOperation l BitwiseAnd r ->
                    infixl_ 8 p l "&" r

                BinaryOperation l BitwiseXor r ->
                    infixl_ 7 p l "^" r

                BinaryOperation l BitwiseOr r ->
                    infixl_ 6 p l "|" r

                BinaryOperation l And r ->
                    infixl_ 5 p l "&&" r

                BinaryOperation l Xor r ->
                    infixl_ 4 p l "^^" r

                BinaryOperation l Or r ->
                    infixl_ 3 p l "||" r

                Ternary c t f ->
                    showParen (p > 2) (go 3 c ++ " ? " ++ go 3 t ++ " : " ++ go 2 f)

                BinaryOperation l Assign r ->
                    infixr_ 1 p l "=" r

                BinaryOperation l ComboAdd r ->
                    infixr_ 1 p l "+=" r

                BinaryOperation l ComboSubtract r ->
                    infixr_ 1 p l "-=" r

                BinaryOperation l ComboBy r ->
                    infixr_ 1 p l "*=" r

                BinaryOperation l ComboDiv r ->
                    infixr_ 1 p l "/=" r

                BinaryOperation l ComboMod r ->
                    infixr_ 1 p l "%=" r

                BinaryOperation l ComboLeftShift r ->
                    infixr_ 1 p l "<<=" r

                BinaryOperation l ComboRightShift r ->
                    infixr_ 1 p l ">>=" r

                BinaryOperation l ComboBitwiseAnd r ->
                    infixr_ 1 p l "&=" r

                BinaryOperation l ComboBitwiseXor r ->
                    infixr_ 1 p l "^=" r

                BinaryOperation l ComboBitwiseOr r ->
                    infixr_ 1 p l "|=" r

                BinaryOperation l Comma r ->
                    infixl_ 0 p l "," r
    in
    go 0 root


binaryOperation : BinaryOperation -> String
binaryOperation op =
    case op of
        By ->
            "*"

        Div ->
            "/"

        Mod ->
            "%"

        Add ->
            "+"

        Subtract ->
            "-"

        ShiftLeft ->
            "<<"

        ShiftRight ->
            ">>"

        RelationOperation LessThan ->
            "<"

        RelationOperation LessThanOrEquals ->
            "<="

        RelationOperation GreaterThan ->
            ">"

        RelationOperation GreaterThanOrEquals ->
            ">="

        RelationOperation Equals ->
            "=="

        RelationOperation NotEquals ->
            "!="

        BitwiseAnd ->
            "&"

        BitwiseXor ->
            "^"

        BitwiseOr ->
            "|"

        And ->
            "&&"

        Xor ->
            "^^"

        Or ->
            "||"

        Assign ->
            "="

        ComboAdd ->
            "+="

        ComboSubtract ->
            "-="

        ComboBy ->
            "*="

        ComboDiv ->
            "/="

        ComboMod ->
            "%="

        ComboLeftShift ->
            "<<="

        ComboRightShift ->
            ">>="

        ComboBitwiseAnd ->
            "&="

        ComboBitwiseXor ->
            "^="

        ComboBitwiseOr ->
            "|="

        Comma ->
            ","

        ArraySubscript ->
            "[]"


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

        TBVec2 ->
            "bvec2"

        TBVec3 ->
            "bvec3"

        TBVec4 ->
            "bvec4"

        TUint ->
            "uint"

        TUVec2 ->
            "uvec2"

        TUVec3 ->
            "uvec3"

        TUVec4 ->
            "uvec4"

        TDouble ->
            "double"

        TDVec2 ->
            "dvec2"

        TDVec3 ->
            "dvec3"

        TDVec4 ->
            "dvec4"

        TMat2 ->
            "mat2"

        TMat4 ->
            "mat4"

        TMat23 ->
            "mat2x3"

        TMat24 ->
            "mat2x4"

        TMat32 ->
            "mat3x2"

        TMat34 ->
            "mat3x4"

        TMat42 ->
            "mat4x2"

        TMat43 ->
            "mat4x3"

        TDMat2 ->
            "dmat2"

        TDMat3 ->
            "dmat3"

        TDMat4 ->
            "dmat4"

        TDMat23 ->
            "dmat2x3"

        TDMat24 ->
            "dmat2x4"

        TDMat32 ->
            "dmat3x2"

        TDMat34 ->
            "dmat3x4"

        TDMat42 ->
            "dmat4x2"

        TDMat43 ->
            "dmat4x3"

        TIn tt ->
            "in " ++ type_ tt

        TOut tt ->
            "out " ++ type_ tt


double : Float -> String
double d =
    float d


float : Float -> String
float f =
    if isNaN f then
        "(0./0.)"

    else if isInfinite f then
        if f > 0 then
            "(1./0.)"

        else
            "(-1./0.)"

    else
        let
            s : String
            s =
                String.fromFloat f
        in
        if String.contains "." s || String.contains "e" s then
            s

        else
            s ++ "."


unaryOperation : UnaryOperation -> String
unaryOperation op =
    case op of
        Negate ->
            "-"

        PostfixIncrement ->
            "++"

        PostfixDecrement ->
            "--"

        PrefixIncrement ->
            "(++)"

        PrefixDecrement ->
            "(--)"

        Plus ->
            "+"

        Invert ->
            "~"

        Not ->
            "!"


declaration : Declaration -> String
declaration decl =
    case decl of
        ConstDeclaration const ->
            "const " ++ type_ const.tipe ++ " " ++ const.name ++ " = " ++ expr const.value ++ ";"

        FunctionDeclaration function ->
            let
                argsString : String
                argsString =
                    function.args
                        |> List.map (\( atype, aname ) -> type_ atype ++ " " ++ aname)
                        |> String.join ", "

                head : String
                head =
                    type_ function.returnType ++ " " ++ function.name ++ "(" ++ argsString ++ ") "
            in
            case function.stat of
                Block _ _ _ ->
                    head ++ stat 0 function.stat

                Nop ->
                    head ++ "{}"

                _ ->
                    [ head ++ "{"
                    , stat 1 function.stat
                    , "}"
                    ]
                        |> String.join "\n"

        UniformDeclaration uniform ->
            "uniform " ++ type_ uniform.tipe ++ " " ++ uniform.name ++ ";"
