module UI.Glsl.Generator exposing (Constant, Context, Continue, ErrorValue(..), Expression, Expression1, Expression2, Expression3, Expression33, Expression4, ExpressionX, File, FunDecl, GlslValue(..), Mat3, Statement, TypedName, TypingFunction, Vec2, Vec3, Vec4, abs2, abs4, abs_, acos2, acos_, add, add2, add3, add33, add4, adds2, adds3, adds4, and, ands, arr, assign, assignAdd, assignBy, atan2_, atan_, bool, boolT, break, by, by2, by3, byF, ceil_, constant, continue, cos_, cosh, cross, decl, def, div, div2, divConst, divF, dot, dotted1, dotted2, dotted3, dotted33, dotted4, eq, exp, expr, expressionToGlsl, false, fileToGlsl, float, floatCast, floatT, floatToGlsl, floor_, for, forDown, forLeq, fract, fun0, fun1, fun2, fun3, fun4, fun5, funDeclToGlsl, fwidth, geq, gl_FragColor, gl_FragCoord, gt, hl2rgb, ifElse, if_, in_, int, intCast, intT, interpret, length, leq, log, log2, lt, mat3T, mat3_3_3_3, max3, max4, max_, min_, minusOne, mix, mod, negate2, negateConst, negate_, neq, normalize, normalize3, one, or, ors, out, postfixDecrement, postfixIncrement, pow, radians_, return, round_, sign, sin_, sinh, smoothstep, sqrt_, statementToGlsl, subtract, subtract2, subtract3, subtract4, subtractConst, tan_, ternary, ternary3, true, uniform, unknown, unsafeCall, value, valueToString, vec2, vec2T, vec2Zero, vec3, vec3T, vec3Zero, vec4, vec4T, vec4Zero, vec4_1_3, vec4_3_1, voidT, zero)

import Dict exposing (Dict)
import Expression exposing (RelationOperation(..))
import Set


type alias File =
    List FunDecl


type FunDecl
    = FunDecl { name : String, type_ : String, body : String }


type Stat
    = If Expr Stat Stat
    | IfElse Expr Stat Stat Stat
    | For String Expr RelationOperation Expr Expr Stat Stat
    | Line String
    | Return Expr
    | Break
    | Continue
    | ExpressionStatement Expr Stat
    | Decl String Name (Maybe Expr) Stat
    | Nop


type Statement r
    = Statement Stat


type Expr
    = Bool Bool
    | Int Int
    | Float Float
    | Variable String
    | And Expr Expr
    | Or Expr Expr
    | Comparison RelationOperation Expr Expr
    | Ternary Expr Expr Expr
    | Add Expr Expr
    | Subtract Expr Expr
    | By Expr Expr
    | Div Expr Expr
    | Call String (List Expr)
    | Negate Expr
    | PostfixIncrement Expr
    | PostfixDecrement Expr
    | Unknown String
    | Dot Expr String
    | Array Expr Expr
    | Assign Expr Expr
    | AssignCombo ComboOperation Expr Expr


type ComboOperation
    = ComboAdd
    | ComboSubtract
    | ComboBy
    | ComboDiv


type Expression t
    = Expression Expr


type alias ExpressionX a t =
    { a | base : Expression t }


type alias Expression1 t =
    { base : Expression t
    }


type alias Expression2 =
    { base : Expression Vec2
    , x : Expression1 Float
    , y : Expression1 Float
    , yx : Expression1 Vec2
    }


type alias Expression3 =
    { base : Expression Vec3
    , x : Expression1 Float
    , y : Expression1 Float
    , z : Expression1 Float
    }


type alias Expression4 =
    { base : Expression Vec4
    , x : Expression1 Float
    , y : Expression1 Float
    , z : Expression1 Float
    , w : Expression1 Float
    , xy : Expression2
    , yx : Expression2
    , yzw : Expression3
    }


type alias Expression33 =
    { base : Expression Mat3 }


type alias TypingFunction t r =
    String -> ( TypedName t r, Expression t -> r )


type TypedName t r
    = TypedName Type Name r


type Type
    = Type String


type Name
    = Name String



-- UTILS


fileToGlsl : File -> String
fileToGlsl decls =
    decls
        |> List.foldl
            (\(FunDecl { name, type_, body }) ( found, output ) ->
                if Set.member ( name, type_ ) found then
                    ( found, output )

                else
                    ( Set.insert ( name, type_ ) found, body :: output )
            )
            ( Set.empty, [] )
        |> Tuple.second
        |> List.reverse
        |> String.join "\n\n"


funDeclToGlsl : FunDecl -> String
funDeclToGlsl (FunDecl { body }) =
    body


statementToGlsl : Statement s -> String
statementToGlsl (Statement r) =
    let
        go i c =
            case c of
                If cond t n ->
                    [ indent i ("if (" ++ expressionToGlsl (Expression cond) ++ ") {")
                    , go (i + 1) t
                    , indent i "}"
                    , ""
                    , go i n
                    ]
                        |> String.join "\n"

                IfElse cond t ((If _ _ _) as f) n ->
                    [ indent i ("if (" ++ expressionToGlsl (Expression cond) ++ ") {")
                    , go (i + 1) t
                    , indent i <| "} else " ++ String.trimLeft (go i f)
                    , go i n
                    ]
                        |> String.join "\n"

                IfElse cond t ((IfElse _ _ _ _) as f) n ->
                    [ indent i ("if (" ++ expressionToGlsl (Expression cond) ++ ") {")
                    , go (i + 1) t
                    , indent i <| "} else " ++ String.trimLeft (go i f)
                    , go i n
                    ]
                        |> String.join "\n"

                IfElse cond t f n ->
                    [ indent i ("if (" ++ expressionToGlsl (Expression cond) ++ ") {")
                    , go (i + 1) t
                    , indent i "} else {"
                    , go (i + 1) f
                    , indent i "}"
                    , ""
                    , go i n
                    ]
                        |> String.join "\n"

                For var from rel to step loop next ->
                    [ indent i ("for (int " ++ var ++ " = " ++ expressionToGlsl (Expression from) ++ "; " ++ var ++ " " ++ relationToString rel ++ " " ++ expressionToGlsl (Expression to) ++ "; " ++ expressionToGlsl (Expression step) ++ ") {")
                    , go (i + 1) loop
                    , indent i "}"
                    , ""
                    , go i next
                    ]
                        |> String.join "\n"

                Line l ->
                    indent i l ++ ";"

                Return e ->
                    indent i <| "return " ++ expressionToGlsl (Expression e) ++ ";"

                Break ->
                    indent i "break;"

                Continue ->
                    indent i "continue;"

                ExpressionStatement e Nop ->
                    indent i (expressionToGlsl (Expression e) ++ ";")

                Decl t (Name n) (Just e) Nop ->
                    indent i (t ++ " " ++ n ++ " = " ++ expressionToGlsl (Expression e) ++ ";")

                Decl t (Name n) Nothing Nop ->
                    indent i (t ++ " " ++ n ++ ";")

                ExpressionStatement e next ->
                    indent i (expressionToGlsl (Expression e) ++ ";\n") ++ go i next

                Decl t (Name n) (Just e) next ->
                    indent i (t ++ " " ++ n ++ " = " ++ expressionToGlsl (Expression e) ++ ";\n") ++ go i next

                Decl t (Name n) Nothing next ->
                    indent i (t ++ " " ++ n ++ ";\n") ++ go i next

                Nop ->
                    ""
    in
    go 1 r


relationToString : RelationOperation -> String
relationToString rel =
    case rel of
        LessThan ->
            "<"

        LessThanOrEquals ->
            "<="

        Equals ->
            "=="

        NotEquals ->
            "!="

        GreaterThanOrEquals ->
            ">="

        GreaterThan ->
            ">"


nop : Continue ()
nop () =
    Statement Nop


internalNop : Continue a
internalNop () =
    Statement Nop


expressionToGlsl : Expression t -> String
expressionToGlsl (Expression tree) =
    let
        -- The numbers are precedence numbers from the GLSL spec
        go : Bool -> Expr -> String
        go rec e =
            case e of
                Unknown u ->
                    u

                _ ->
                    go17 rec e

        go17 : Bool -> Expr -> String
        go17 rec e =
            go16 rec e

        go16 : Bool -> Expr -> String
        go16 rec e =
            case e of
                Assign l r ->
                    go15 False l ++ " = " ++ go15 False r

                AssignCombo k l r ->
                    go15 False l ++ " " ++ comboOperationToString k ++ "= " ++ go15 False r

                _ ->
                    go15 rec e

        go15 : Bool -> Expr -> String
        go15 rec e =
            case e of
                Ternary c t f ->
                    go14 False c ++ " ? " ++ go14 False t ++ " : " ++ go15 False f

                _ ->
                    go14 rec e

        go14 : Bool -> Expr -> String
        go14 rec e =
            case e of
                Or l r ->
                    go14 False l ++ " || " ++ go13 False r

                _ ->
                    go13 rec e

        go13 : Bool -> Expr -> String
        go13 rec e =
            go12 rec e

        go12 : Bool -> Expr -> String
        go12 rec e =
            case e of
                And l r ->
                    go12 False l ++ " && " ++ go11 False r

                _ ->
                    go11 rec e

        go11 : Bool -> Expr -> String
        go11 rec e =
            go10 rec e

        go10 : Bool -> Expr -> String
        go10 rec e =
            go9 rec e

        go9 : Bool -> Expr -> String
        go9 rec e =
            go8 rec e

        go8 : Bool -> Expr -> String
        go8 rec e =
            case e of
                Comparison Equals l r ->
                    go7 False l ++ " == " ++ go7 False r

                Comparison NotEquals l r ->
                    go7 False l ++ " != " ++ go7 False r

                _ ->
                    go7 rec e

        go7 : Bool -> Expr -> String
        go7 rec e =
            case e of
                Comparison LessThan l r ->
                    go6 False l ++ " < " ++ go6 False r

                Comparison LessThanOrEquals l r ->
                    go6 False l ++ " <= " ++ go6 False r

                Comparison GreaterThan l r ->
                    go6 False l ++ " > " ++ go6 False r

                Comparison GreaterThanOrEquals l r ->
                    go6 False l ++ " >= " ++ go6 False r

                _ ->
                    go6 rec e

        go6 : Bool -> Expr -> String
        go6 rec e =
            go5 rec e

        go5 : Bool -> Expr -> String
        go5 rec e =
            case e of
                Add l r ->
                    go5 False l ++ " + " ++ go4 False r

                Subtract l r ->
                    go5 False l ++ " - " ++ go4 False r

                _ ->
                    go4 rec e

        go4 : Bool -> Expr -> String
        go4 rec e =
            case e of
                By l r ->
                    go4 False l ++ " * " ++ go3 False r

                Div l r ->
                    go4 False l ++ " / " ++ go3 False r

                _ ->
                    go3 rec e

        go3 : Bool -> Expr -> String
        go3 rec e =
            case e of
                Negate c ->
                    "-" ++ go2 False c

                PostfixIncrement c ->
                    go2 False c ++ "++"

                PostfixDecrement c ->
                    go2 False c ++ "--"

                _ ->
                    go2 rec e

        go2 : Bool -> Expr -> String
        go2 rec e =
            case e of
                Call name args ->
                    name ++ "(" ++ String.join ", " (List.map (go False) args) ++ ")"

                Dot l r ->
                    go2 False l ++ "." ++ r

                Array l r ->
                    go2 False l ++ "[" ++ go False r ++ "]"

                _ ->
                    go1 rec e

        go1 : Bool -> Expr -> String
        go1 rec e =
            case e of
                Bool b ->
                    if b then
                        "true"

                    else
                        "false"

                Float f ->
                    floatToGlsl f

                Int i ->
                    String.fromInt i

                Variable v ->
                    v

                _ ->
                    if rec then
                        "!!!Internal error in toString for Glsl!!!"

                    else
                        "(" ++ go True e ++ ")"
    in
    go False tree


comboOperationToString : ComboOperation -> String
comboOperationToString op =
    case op of
        ComboAdd ->
            "+"

        ComboSubtract ->
            "-"

        ComboBy ->
            "*"

        ComboDiv ->
            "/"


indent : Int -> String -> String
indent i line =
    String.repeat (4 * i) " " ++ line



--EXPRESSIONS


ternary : ExpressionX a Bool -> ExpressionX b t -> ExpressionX c t -> Expression1 t
ternary =
    expr3 Ternary


ternary3 : ExpressionX xa Bool -> ExpressionX xb Vec3 -> ExpressionX xc Vec3 -> Expression3
ternary3 =
    expr33 Ternary


and : ExpressionX a Bool -> ExpressionX b Bool -> Expression1 Bool
and =
    expr2 And


ands : List (Expression1 Bool) -> Expression1 Bool
ands es =
    case es of
        [] ->
            true

        h :: t ->
            List.foldl (\e a -> expr2 And a e) h t


or : ExpressionX a Bool -> ExpressionX b Bool -> Expression1 Bool
or =
    expr2 Or


ors : List (Expression1 Bool) -> Expression1 Bool
ors es =
    case es of
        [] ->
            false

        h :: t ->
            List.foldl (\e a -> expr2 Or a e) h t


adds2 : List Expression2 -> Expression2
adds2 es =
    case es of
        [] ->
            vec2Zero

        h :: t ->
            List.foldl (\e a -> add2 a e) h t


adds3 : List Expression3 -> Expression3
adds3 es =
    case es of
        [] ->
            vec3Zero

        h :: t ->
            List.foldl (\e a -> add3 a e) h t


adds4 : List Expression4 -> Expression4
adds4 es =
    case es of
        [] ->
            vec4Zero

        h :: t ->
            List.foldl (\e a -> add4 a e) h t


true : Expression1 Bool
true =
    dotted1 <| Expression <| Bool True


false : Expression1 Bool
false =
    dotted1 <| Expression <| Bool False


expr1 : (Expr -> Expr) -> ExpressionX a l -> Expression1 t
expr1 f e =
    dotted1Internal (f (unwrapExpression e))


expr2 : (Expr -> Expr -> Expr) -> ExpressionX a l -> ExpressionX b r -> Expression1 t
expr2 f l r =
    dotted1Internal (f (unwrapExpression l) (unwrapExpression r))


expr22 : (Expr -> Expr -> Expr) -> ExpressionX a Vec2 -> ExpressionX b Vec2 -> Expression2
expr22 f l r =
    dotted2Internal (f (unwrapExpression l) (unwrapExpression r))


expr23 : (Expr -> Expr -> Expr) -> ExpressionX a Vec3 -> ExpressionX b Vec3 -> Expression3
expr23 f l r =
    dotted3Internal (f (unwrapExpression l) (unwrapExpression r))


expr24 : (Expr -> Expr -> Expr) -> ExpressionX a Vec4 -> ExpressionX b Vec4 -> Expression4
expr24 f l r =
    dotted4Internal (f (unwrapExpression l) (unwrapExpression r))


expr233 : (Expr -> Expr -> Expr) -> ExpressionX a Mat3 -> ExpressionX b Mat3 -> Expression33
expr233 f l r =
    dotted33Internal (f (unwrapExpression l) (unwrapExpression r))


expr3 : (Expr -> Expr -> Expr -> Expr) -> ExpressionX a l -> ExpressionX b m -> ExpressionX c r -> Expression1 t
expr3 f l m r =
    dotted1Internal (f (unwrapExpression l) (unwrapExpression m) (unwrapExpression r))


expr33 : (Expr -> Expr -> Expr -> Expr) -> ExpressionX xa a -> ExpressionX xb b -> ExpressionX xc c -> Expression3
expr33 f l m r =
    dotted3Internal (f (unwrapExpression l) (unwrapExpression m) (unwrapExpression r))


unwrapExpression : ExpressionX a e -> Expr
unwrapExpression { base } =
    let
        (Expression e) =
            base
    in
    e



------------------------------
-- FUNCTIONS AND OPERATIONS --
------------------------------


add : ExpressionX a t -> ExpressionX b t -> Expression1 t
add =
    expr2 Add


add2 : ExpressionX a Vec2 -> ExpressionX b Vec2 -> Expression2
add2 =
    expr22 Add


add3 : ExpressionX a Vec3 -> ExpressionX b Vec3 -> Expression3
add3 =
    expr23 Add


add4 : ExpressionX a Vec4 -> ExpressionX b Vec4 -> Expression4
add4 =
    expr24 Add


add33 : ExpressionX a Mat3 -> ExpressionX b Mat3 -> Expression33
add33 =
    expr233 Add


subtract : ExpressionX a t -> ExpressionX b t -> Expression1 t
subtract =
    expr2 Subtract


subtractConst : ExpressionX { a | isConstant : Constant } t -> ExpressionX { b | isConstant : Constant } t -> ExpressionX { isConstant : Constant } t
subtractConst l r =
    { base = (expr2 Subtract l r).base
    , isConstant = Constant
    }


subtract2 : ExpressionX a Vec2 -> ExpressionX b Vec2 -> Expression2
subtract2 =
    expr22 Subtract


subtract3 : ExpressionX a Vec3 -> ExpressionX b Vec3 -> Expression3
subtract3 =
    expr23 Subtract


subtract4 : ExpressionX a Vec4 -> ExpressionX b Vec4 -> Expression4
subtract4 =
    expr24 Subtract


negate_ : ExpressionX a t -> Expression1 t
negate_ =
    expr1 Negate


negateConst : ExpressionX { a | isConstant : Constant } t -> ExpressionX { isConstant : Constant } t
negateConst l =
    { base = (expr1 Negate l).base
    , isConstant = Constant
    }


negate2 : ExpressionX a Vec2 -> Expression2
negate2 e =
    dotted2Internal (Negate <| unwrapExpression e)


postfixIncrement : ExpressionX a t -> Expression1 t
postfixIncrement =
    expr1 PostfixIncrement


postfixDecrement : ExpressionX a t -> Expression1 t
postfixDecrement =
    expr1 PostfixDecrement


by : ExpressionX a t -> ExpressionX b t -> Expression1 t
by =
    expr2 By


by2 : ExpressionX a Vec2 -> ExpressionX b Vec2 -> Expression2
by2 =
    expr22 By


by3 : ExpressionX a Vec3 -> ExpressionX b Vec3 -> Expression3
by3 =
    expr23 By


byF : ExpressionX a Float -> ExpressionX b t -> Expression1 t
byF =
    expr2 By


divF : ExpressionX a t -> ExpressionX b Float -> Expression1 t
divF =
    expr2 Div


div : ExpressionX a t -> ExpressionX b t -> Expression1 t
div =
    expr2 Div


div2 : ExpressionX a Vec2 -> ExpressionX b Vec2 -> Expression2
div2 =
    expr22 Div


divConst : ExpressionX { a | isConstant : Constant } t -> ExpressionX { b | isConstant : Constant } t -> ExpressionX { isConstant : Constant } t
divConst l r =
    { base = (expr2 Div l r).base
    , isConstant = Constant
    }


lt : ExpressionX a t -> ExpressionX b t -> Expression1 Bool
lt =
    expr2 (Comparison LessThan)


leq : ExpressionX a t -> ExpressionX b t -> Expression1 Bool
leq =
    expr2 (Comparison LessThanOrEquals)


eq : ExpressionX a t -> ExpressionX b t -> Expression1 Bool
eq =
    expr2 (Comparison Equals)


neq : ExpressionX a t -> ExpressionX b t -> Expression1 Bool
neq =
    expr2 (Comparison NotEquals)


geq : ExpressionX a t -> ExpressionX b t -> Expression1 Bool
geq =
    expr2 (Comparison GreaterThanOrEquals)


gt : ExpressionX a t -> ExpressionX b t -> Expression1 Bool
gt =
    expr2 (Comparison GreaterThan)


abs_ : ExpressionX a t -> Expression1 t
abs_ =
    dotted1 << call1Internal "abs"


abs2 : ExpressionX a Vec2 -> Expression2
abs2 =
    dotted2 << call1Internal "abs"


abs4 : ExpressionX a Vec4 -> Expression4
abs4 =
    dotted4 << call1Internal "abs"


mix : ExpressionX a t -> ExpressionX b t -> ExpressionX c Float -> Expression1 t
mix x y a =
    dotted1 <| call3Internal "mix" x y a


smoothstep : ExpressionX a Float -> ExpressionX b Float -> ExpressionX c Float -> Expression1 t
smoothstep x y a =
    dotted1 <| call3Internal "smoothstep" x y a


exp : ExpressionX a t -> Expression1 t
exp =
    dotted1 << call1Internal "exp"


sqrt_ : ExpressionX a t -> Expression1 t
sqrt_ =
    dotted1 << call1Internal "sqrt"


sin_ : ExpressionX a t -> Expression1 t
sin_ =
    dotted1 << call1Internal "sin"


cos_ : ExpressionX a t -> Expression1 t
cos_ =
    dotted1 << call1Internal "cos"


tan_ : ExpressionX a t -> Expression1 t
tan_ =
    dotted1 << call1Internal "tan"


acos_ : ExpressionX a t -> Expression1 t
acos_ =
    dotted1 << call1Internal "acos"


acos2 : ExpressionX a Vec2 -> Expression2
acos2 =
    dotted2 << call1Internal "acos"


sinh : ExpressionX a t -> Expression1 t
sinh =
    dotted1 << call1Internal "sinh"


cosh : ExpressionX a t -> Expression1 t
cosh =
    dotted1 << call1Internal "cosh"


log : ExpressionX a t -> Expression1 t
log =
    dotted1 << call1Internal "log"


mod : ExpressionX a t -> ExpressionX b t -> Expression1 t
mod l r =
    dotted1 (call2Internal "mod" l r)


min_ : ExpressionX a t -> ExpressionX b t -> Expression1 t
min_ l r =
    dotted1 (call2Internal "min" l r)


max_ : ExpressionX a t -> ExpressionX b t -> Expression1 t
max_ l r =
    dotted1 (call2Internal "max" l r)


max3 : ExpressionX a Vec3 -> ExpressionX b Vec3 -> Expression3
max3 l r =
    dotted3 (call2Internal "max" l r)


max4 : ExpressionX a Vec4 -> ExpressionX b Vec4 -> Expression4
max4 l r =
    dotted4 (call2Internal "max" l r)


hl2rgb : ExpressionX a Float -> ExpressionX b Float -> Expression3
hl2rgb h l =
    dotted3 (call2Internal "hl2rgb" h l)


pow : ExpressionX a Float -> ExpressionX b Float -> Expression1 Float
pow l r =
    dotted1 (call2Internal "pow" l r)


log2 : ExpressionX a Float -> Expression1 Float
log2 =
    dotted1 << call1Internal "log2"


ceil_ : ExpressionX a t -> Expression1 t
ceil_ =
    dotted1 << call1Internal "ceil"


floor_ : ExpressionX a t -> Expression1 t
floor_ =
    dotted1 << call1Internal "floor"


round_ : ExpressionX a t -> Expression1 t
round_ =
    dotted1 << call1Internal "round"


sign : ExpressionX a t -> Expression1 t
sign =
    dotted1 << call1Internal "sign"


radians_ : ExpressionX a Float -> Expression1 Float
radians_ =
    dotted1 << call1Internal "radians"


normalize : ExpressionX a t -> Expression1 t
normalize =
    dotted1 << call1Internal "normalize"


normalize3 : ExpressionX a Vec3 -> Expression3
normalize3 =
    dotted3 << call1Internal "normalize"


length : ExpressionX a t -> Expression1 Float
length =
    dotted1 << call1Internal "length"


fwidth : ExpressionX a t -> Expression1 t
fwidth =
    dotted1 << call1Internal "fwidth"


cross : ExpressionX a Vec3 -> ExpressionX b Vec3 -> Expression3
cross l r =
    dotted3 <| call2Internal "cross" l r


atan_ : ExpressionX a Float -> Expression1 Float
atan_ =
    dotted1 << call1Internal "atan"


atan2_ : ExpressionX a Float -> ExpressionX b Float -> Expression1 Float
atan2_ l r =
    dotted1 <| call2Internal "atan" l r


intCast : ExpressionX a Float -> Expression1 Int
intCast =
    dotted1 << call1Internal "int"


floatCast : ExpressionX a Int -> Expression1 Float
floatCast =
    dotted1 << call1Internal "float"


fract : ExpressionX a Float -> Expression1 Float
fract =
    dotted1 << call1Internal "fract"


arr : ExpressionX a Mat3 -> ExpressionX b Int -> Expression1 Vec3
arr =
    expr2 Array


dot : ExpressionX a t -> ExpressionX b t -> Expression1 Float
dot l r =
    dotted1 (call2Internal "dot" l r)


vec2 : ExpressionX a Float -> ExpressionX b Float -> Expression2
vec2 x y =
    { base = call2Internal "vec2" x y
    , x = { base = x.base }
    , y = { base = y.base }
    , yx = { base = call2Internal "vec2" y x }
    }


vec3 :
    ExpressionX a Float
    -> ExpressionX b Float
    -> ExpressionX c Float
    -> Expression3
vec3 x y z =
    { base = call3Internal "vec3" x y z
    , x = { base = x.base }
    , y = { base = y.base }
    , z = { base = z.base }
    }


vec4 :
    ExpressionX a Float
    -> ExpressionX b Float
    -> ExpressionX c Float
    -> ExpressionX d Float
    -> Expression4
vec4 x y z w =
    { base = call4Internal "vec4" x y z w
    , x = { base = x.base }
    , y = { base = y.base }
    , z = { base = z.base }
    , w = { base = w.base }
    , xy = dotted2 <| call2Internal "vec2" x y
    , yx = dotted2 <| call2Internal "vec2" y x
    , yzw = dotted3 <| call3Internal "vec3" y z w
    }


vec4_3_1 : ExpressionX a Vec3 -> ExpressionX b Float -> Expression4
vec4_3_1 xyz w =
    call2Internal "vec4" xyz w
        |> dotted4


vec4_1_3 : ExpressionX a Float -> ExpressionX b Vec3 -> Expression4
vec4_1_3 x yzw =
    call2Internal "vec4" x yzw
        |> dotted4


mat3_3_3_3 : ExpressionX a Vec3 -> ExpressionX b Vec3 -> ExpressionX c Vec3 -> Expression33
mat3_3_3_3 c0 c1 c2 =
    call3Internal "mat3" c0 c1 c2 |> dotted33



---------------
-- CONSTANTS --
---------------


vec2Zero : Expression2
vec2Zero =
    vec2 zero zero


vec3Zero : Expression3
vec3Zero =
    vec3 zero zero zero


vec4Zero : Expression4
vec4Zero =
    vec4 zero zero zero zero


gl_FragColor : Expression4
gl_FragColor =
    dottedVariable4 "gl_FragColor"


gl_FragCoord : Expression4
gl_FragCoord =
    dottedVariable4 "gl_FragCoord"



----------------
-- CALL UTILS --
----------------


unsafeCall : String -> List (Expression t) -> Expression r
unsafeCall name =
    Expression << Call name << List.map (\(Expression e) -> e)


type Constant
    = Constant


zero : ExpressionX { isConstant : Constant } Float
zero =
    float 0


one : ExpressionX { isConstant : Constant } Float
one =
    float 1


minusOne : ExpressionX { isConstant : Constant } Float
minusOne =
    float -1


float : Float -> ExpressionX { isConstant : Constant } Float
float f =
    { base = Expression (Float f)
    , isConstant = Constant
    }


int : Int -> ExpressionX { isConstant : Constant } Int
int i =
    { base = Expression <| Int i
    , isConstant = Constant
    }


bool : Bool -> ExpressionX { isConstant : Constant } Bool
bool b =
    { base = Expression <| Bool b
    , isConstant = Constant
    }


constant : TypingFunction t r -> String -> ExpressionX { isConstant : Constant } t
constant _ name =
    { base = Expression <| Variable name
    , isConstant = Constant
    }


floatToGlsl : Float -> String
floatToGlsl f =
    let
        s =
            String.fromFloat f
    in
    if String.contains "." s || String.contains "e" s then
        s

    else
        s ++ "."


call0Internal : String -> Expression t
call0Internal name =
    Expression (Call name [])


call1Internal : String -> ExpressionX a l -> Expression t
call1Internal name arg0 =
    Expression (Call name [ unwrapExpression arg0 ])


call2Internal : String -> ExpressionX a l -> ExpressionX b r -> Expression t
call2Internal name arg0 arg1 =
    Expression (Call name [ unwrapExpression arg0, unwrapExpression arg1 ])


call3Internal : String -> ExpressionX a l -> ExpressionX b m -> ExpressionX c r -> Expression t
call3Internal name arg0 arg1 arg2 =
    Expression (Call name [ unwrapExpression arg0, unwrapExpression arg1, unwrapExpression arg2 ])


call4Internal : String -> ExpressionX a l -> ExpressionX b m -> ExpressionX c n -> ExpressionX d r -> Expression t
call4Internal name arg0 arg1 arg2 arg3 =
    Expression (Call name [ unwrapExpression arg0, unwrapExpression arg1, unwrapExpression arg2, unwrapExpression arg3 ])


call5Internal : String -> ExpressionX a l -> ExpressionX b m -> ExpressionX c n -> ExpressionX d r -> ExpressionX e s -> Expression t
call5Internal name arg0 arg1 arg2 arg3 arg4 =
    Expression (Call name [ unwrapExpression arg0, unwrapExpression arg1, unwrapExpression arg2, unwrapExpression arg3, unwrapExpression arg4 ])


dotted1 : Expression t -> Expression1 t
dotted1 (Expression e) =
    dotted1Internal e


dotted1Internal : Expr -> Expression1 t
dotted1Internal e =
    { base = Expression e
    }


dotted2 : Expression Vec2 -> Expression2
dotted2 (Expression e) =
    dotted2Internal e


dotInternal : Expr -> String -> Expression1 t
dotInternal e v =
    dotted1 (Expression (Dot e v))


dotted2Internal : Expr -> Expression2
dotted2Internal e =
    { base = Expression e
    , x = dotInternal e "x"
    , y = dotInternal e "y"
    , yx = dotInternal e "yx"
    }


dotted3 : Expression Vec3 -> Expression3
dotted3 (Expression e) =
    dotted3Internal e


dotted3Internal : Expr -> Expression3
dotted3Internal e =
    { base = Expression e
    , x = dotInternal e "x"
    , y = dotInternal e "y"
    , z = dotInternal e "z"
    }


dotted4 : Expression Vec4 -> Expression4
dotted4 (Expression e) =
    dotted4Internal e


dotted4Internal : Expr -> Expression4
dotted4Internal e =
    { base = Expression e
    , x = dotInternal e "x"
    , y = dotInternal e "y"
    , z = dotInternal e "z"
    , w = dotInternal e "w"
    , xy = dotted2 <| Expression <| Dot e "xy"
    , yx = dotted2 <| Expression <| Dot e "yx"
    , yzw = dotted3 <| Expression <| Dot e "yzw"
    }


dotted33 : Expression Mat3 -> Expression33
dotted33 (Expression e) =
    dotted33Internal e


dotted33Internal : Expr -> Expression33
dotted33Internal e =
    { base = Expression e }


uniform : TypingFunction t c -> String -> c
uniform typeF name =
    let
        ( TypedName _ _ e, _ ) =
            typeF name
    in
    e


unknown : String -> Expression t
unknown =
    let
        _ =
            Debug.todo
    in
    Expression << Unknown



-- STATEMENTS


funInternal : TypedName t c -> List ( String, String ) -> Statement t -> FunDecl
funInternal (TypedName (Type rt) (Name name) _) args body =
    let
        argsList =
            String.join ", " (List.map (\( t, n ) -> t ++ " " ++ n) args)
    in
    FunDecl
        { name = name
        , type_ =
            (args ++ [ ( rt, "" ) ])
                |> List.map Tuple.first
                |> String.join " -> "
        , body =
            String.join "\n" <|
                [ rt ++ " " ++ name ++ "(" ++ argsList ++ ") {"
                , statementToGlsl body
                , "}"
                ]
        }


argToString : TypedName t c -> ( String, String )
argToString (TypedName (Type t) (Name n) _) =
    ( t, n )


toVar : TypedName t c -> c
toVar (TypedName _ _ e) =
    e


fun0 :
    TypingFunction t c
    -> String
    -> (Continue () -> Statement t)
    -> ( FunDecl, c )
fun0 typeF name body =
    let
        ( typed, dotter ) =
            typeF name
    in
    ( funInternal typed [] (body nop)
    , dotter <| call0Internal name
    )


fun1 :
    TypingFunction tr r
    -> String
    -> ( TypedName ta a, Expression ta -> a )
    -> (a -> Continue () -> Statement tr)
    ->
        ( FunDecl
        , ExpressionX xa ta -> r
        )
fun1 typeF name ( arg0, _ ) body =
    let
        ( typed, dotter ) =
            typeF name
    in
    ( funInternal typed [ argToString arg0 ] <|
        body (toVar arg0) nop
    , dotter << call1Internal name
    )


fun2 :
    TypingFunction tr r
    -> String
    -> ( TypedName ta a, Expression ta -> a )
    -> ( TypedName tb b, Expression tb -> b )
    -> (a -> b -> Continue () -> Statement tr)
    ->
        ( FunDecl
        , ExpressionX xa ta -> ExpressionX xb tb -> r
        )
fun2 typeF name ( arg0, _ ) ( arg1, _ ) body =
    let
        ( typed, dotter ) =
            typeF name
    in
    ( funInternal typed [ argToString arg0, argToString arg1 ] <|
        body (toVar arg0) (toVar arg1) nop
    , \l r -> dotter (call2Internal name l r)
    )


fun3 :
    TypingFunction tr r
    -> String
    -> ( TypedName ta a, Expression ta -> a )
    -> ( TypedName tb b, Expression tb -> b )
    -> ( TypedName tc c, Expression tc -> c )
    -> (a -> b -> c -> Continue () -> Statement tr)
    ->
        ( FunDecl
        , ExpressionX xa ta -> ExpressionX xb tb -> ExpressionX xc tc -> r
        )
fun3 typeF name ( arg0, _ ) ( arg1, _ ) ( arg2, _ ) body =
    let
        ( typed, dotter ) =
            typeF name
    in
    ( funInternal typed [ argToString arg0, argToString arg1, argToString arg2 ] <|
        body (toVar arg0) (toVar arg1) (toVar arg2) nop
    , \l m r -> dotter (call3Internal name l m r)
    )


fun4 :
    TypingFunction tr r
    -> String
    -> ( TypedName ta a, Expression ta -> a )
    -> ( TypedName tb b, Expression tb -> b )
    -> ( TypedName tc c, Expression tc -> c )
    -> ( TypedName td d, Expression td -> d )
    -> (a -> b -> c -> d -> Continue () -> Statement tr)
    ->
        ( FunDecl
        , ExpressionX xa ta -> ExpressionX xb tb -> ExpressionX xc tc -> ExpressionX xd td -> r
        )
fun4 typeF name ( arg0, _ ) ( arg1, _ ) ( arg2, _ ) ( arg3, _ ) body =
    let
        ( typed, dotter ) =
            typeF name
    in
    ( funInternal typed [ argToString arg0, argToString arg1, argToString arg2, argToString arg3 ] <|
        body (toVar arg0) (toVar arg1) (toVar arg2) (toVar arg3) nop
    , \l m n r -> dotter (call4Internal name l m n r)
    )


fun5 :
    TypingFunction tr r
    -> String
    -> ( TypedName ta a, Expression ta -> a )
    -> ( TypedName tb b, Expression tb -> b )
    -> ( TypedName tc c, Expression tc -> c )
    -> ( TypedName td d, Expression td -> d )
    -> ( TypedName te e, Expression te -> e )
    -> (a -> b -> c -> d -> e -> Continue () -> Statement tr)
    ->
        ( FunDecl
        , ExpressionX xa ta -> ExpressionX xb tb -> ExpressionX xc tc -> ExpressionX xd td -> ExpressionX xe te -> r
        )
fun5 typeF name ( arg0, _ ) ( arg1, _ ) ( arg2, _ ) ( arg3, _ ) ( arg4, _ ) body =
    let
        ( typed, dotter ) =
            typeF name
    in
    ( funInternal typed [ argToString arg0, argToString arg1, argToString arg2, argToString arg3, argToString arg4 ] <|
        body (toVar arg0) (toVar arg1) (toVar arg2) (toVar arg3) (toVar arg4) nop
    , \l m c n r -> dotter (call5Internal name l m c n r)
    )


dottedVariable1 : String -> Expression1 t
dottedVariable1 v =
    dotted1 (Expression (Variable v))


dottedVariable2 : String -> Expression2
dottedVariable2 v =
    dotted2 (Expression (Variable v))


dottedVariable3 : String -> Expression3
dottedVariable3 v =
    dotted3 (Expression (Variable v))


dottedVariable4 : String -> Expression4
dottedVariable4 v =
    dotted4 (Expression (Variable v))


type alias Continue s =
    () -> Statement s


if_ : Expression1 Bool -> (Continue r -> Statement r) -> Continue r -> Statement r
if_ cond ifTrue next =
    Statement <| If (unwrapExpression cond) (unwrapStatement <| ifTrue internalNop) (unwrapLazyStatement next)


ifElse : Expression1 Bool -> (Continue s -> Statement s) -> (Continue s -> Statement s) -> Continue s -> Statement s
ifElse cond ifTrue ifFalse next =
    Statement <| IfElse (unwrapExpression cond) (unwrapStatement <| ifTrue internalNop) (unwrapStatement <| ifFalse internalNop) (unwrapLazyStatement next)


for :
    ( String, ExpressionX { a | isConstant : Constant } Int, ExpressionX { b | isConstant : Constant } Int )
    -> (Expression1 Int -> Continue r -> Statement r)
    -> Continue r
    -> Statement r
for ( var, from, to ) loop next =
    Statement <|
        For var
            (unwrapExpression from)
            LessThan
            (unwrapExpression to)
            (PostfixIncrement (Variable var))
            (unwrapStatement <| loop (dottedVariable1 var) internalNop)
            (unwrapLazyStatement next)


forLeq :
    ( String, ExpressionX { a | isConstant : Constant } Int, ExpressionX { b | isConstant : Constant } Int )
    -> (Expression1 Int -> Continue r -> Statement r)
    -> Continue r
    -> Statement r
forLeq ( var, from, to ) loop next =
    Statement <|
        For var
            (unwrapExpression from)
            LessThanOrEquals
            (unwrapExpression to)
            (PostfixIncrement (Variable var))
            (unwrapStatement <| loop (dottedVariable1 var) internalNop)
            (unwrapLazyStatement next)


forDown :
    ( String, ExpressionX { a | isConstant : Constant } Int, ExpressionX { b | isConstant : Constant } Int )
    -> (Expression1 Int -> Continue r -> Statement r)
    -> Continue r
    -> Statement r
forDown ( var, from, to ) loop next =
    Statement <|
        For var
            (unwrapExpression from)
            GreaterThan
            (unwrapExpression to)
            (PostfixDecrement (Variable var))
            (unwrapStatement <| loop (dottedVariable1 var) internalNop)
            (unwrapLazyStatement next)


return : ExpressionX a r -> Continue x -> Statement r
return e _ =
    Statement <| Return <| unwrapExpression e


break : Continue a -> Statement a
break _ =
    Statement Break


continue : Continue a -> Statement a
continue _ =
    Statement Continue


decl : TypingFunction tv v -> String -> (v -> Statement tr) -> Statement tr
decl typeF name k =
    let
        ( TypedName (Type t) n e, _ ) =
            typeF name

        (Statement ks) =
            k e
    in
    Statement <| Decl t n Nothing ks


def : TypingFunction tv v -> String -> ExpressionX xv tv -> (v -> Statement tr) -> Statement tr
def typeF name v k =
    let
        ( TypedName (Type t) n e, _ ) =
            typeF name

        (Statement ks) =
            k e
    in
    Statement <| Decl t n (Just <| unwrapExpression v) ks


assign : ExpressionX a t -> ExpressionX b t -> Expression1 t
assign name e =
    dotted1Internal <| Assign (unwrapExpression name) (unwrapExpression e)


unwrapLazyStatement : Continue r -> Stat
unwrapLazyStatement f =
    let
        (Statement next) =
            f ()
    in
    next


unwrapStatement : Statement r -> Stat
unwrapStatement (Statement next) =
    next


expr : ExpressionX a t -> Continue q -> Statement q
expr e next =
    Statement <| ExpressionStatement (unwrapExpression e) (unwrapLazyStatement next)


assignAdd : ExpressionX a t -> ExpressionX b t -> Continue q -> Statement q
assignAdd name e next =
    Statement <| ExpressionStatement (AssignCombo ComboAdd (unwrapExpression name) (unwrapExpression e)) (unwrapLazyStatement next)


assignBy : ExpressionX a t -> ExpressionX b t -> Continue q -> Statement q
assignBy name e next =
    Statement <| ExpressionStatement (AssignCombo ComboBy (unwrapExpression name) (unwrapExpression e)) (unwrapLazyStatement next)



-- TYPES


type Vec2
    = Vec2 Vec2


type Vec3
    = Vec3 Vec3


type Vec4
    = Vec4 Vec4


type Mat3
    = Mat3 Mat3


voidT : TypingFunction () (Never -> a)
voidT n =
    ( TypedName (Type "void") (Name n) never, always never )


boolT : TypingFunction Bool (Expression1 Bool)
boolT n =
    ( TypedName (Type "bool") (Name n) (dottedVariable1 n), dotted1 )


intT : TypingFunction Int (Expression1 Int)
intT n =
    ( TypedName (Type "int") (Name n) (dottedVariable1 n), dotted1 )


floatT : TypingFunction Float (Expression1 Float)
floatT n =
    ( TypedName (Type "float") (Name n) (dottedVariable1 n), dotted1 )


vec2T : TypingFunction Vec2 Expression2
vec2T n =
    ( TypedName (Type "vec2") (Name n) (dottedVariable2 n), dotted2 )


vec3T : TypingFunction Vec3 Expression3
vec3T n =
    ( TypedName (Type "vec3") (Name n) (dottedVariable3 n), dotted3 )


vec4T : TypingFunction Vec4 Expression4
vec4T n =
    ( TypedName (Type "vec4") (Name n) (dottedVariable4 n), dotted4 )


mat3T : TypingFunction Mat3 (Expression1 Mat3)
mat3T n =
    ( TypedName (Type "mat3") (Name n) (dottedVariable1 n), dotted1 )


out : TypingFunction t r -> TypingFunction t r
out inner n =
    let
        ( TypedName (Type t) name v, c ) =
            inner n
    in
    ( TypedName (Type <| "out " ++ t) name v, c )


in_ : TypingFunction t r -> TypingFunction t r
in_ inner n =
    let
        ( TypedName (Type t) name v, c ) =
            inner n
    in
    ( TypedName (Type <| "in " ++ t) name v, c )



--------------------
-- TESTING HELPER --
--------------------


type GlslValue
    = VFloat Float
    | VBool Bool
    | VInt Int
    | VVec2 Float Float
    | VVec3 Float Float Float
    | VMat3 ( Float, Float, Float ) ( Float, Float, Float ) ( Float, Float, Float )
    | VVoid -- Used for void return


type ErrorValue
    = MissingVariable String
    | InvalidTypes String


type alias Context =
    Dict String GlslValue


valueToString : GlslValue -> String
valueToString v =
    case v of
        VFloat f ->
            String.fromFloat f

        VBool b ->
            if b then
                "true"

            else
                "false"

        VInt i ->
            String.fromInt i

        VVoid ->
            "void"

        VVec2 x y ->
            "vec2(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"

        VVec3 x y z ->
            "vec3(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ "," ++ String.fromFloat z ++ ")"

        VMat3 _ _ _ ->
            Debug.todo "branch 'VMat3 _ _ _' not implemented"


value : Context -> Expression t -> Result ErrorValue ( Context, GlslValue )
value initialContext (Expression input) =
    innerValue initialContext input


innerValue : Context -> Expr -> Result ErrorValue ( Context, GlslValue )
innerValue ctx e =
    case e of
        Float f ->
            Ok ( ctx, VFloat f )

        Bool b ->
            Ok ( ctx, VBool b )

        Int i ->
            Ok ( ctx, VInt i )

        Variable v ->
            case Dict.get v ctx of
                Just f ->
                    Ok ( ctx, f )

                Nothing ->
                    Err <| MissingVariable v

        And l r ->
            innerValue2 ctx l r <| \ctx2 vl vr ->
            case ( vl, vr ) of
                ( VBool bl, VBool br ) ->
                    Ok ( ctx2, VBool <| bl && br )

                _ ->
                    Err <|
                        InvalidTypes
                            ("Cannot calculate `and` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        Or l r ->
            innerValue2 ctx l r <| \ctx2 vl vr ->
            case ( vl, vr ) of
                ( VBool bl, VBool br ) ->
                    Ok ( ctx2, VBool <| bl || br )

                _ ->
                    Err <|
                        InvalidTypes
                            ("Cannot calculate `or` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        Comparison k l r ->
            innerValue2 ctx l r <| \ctx2 vl vr ->
            case ( vl, vr, k ) of
                ( VFloat fl, VFloat fr, LessThan ) ->
                    Ok ( ctx2, VBool (fl < fr) )

                ( VFloat fl, VFloat fr, LessThanOrEquals ) ->
                    Ok ( ctx2, VBool (fl <= fr) )

                ( VFloat fl, VFloat fr, Equals ) ->
                    Ok ( ctx2, VBool (fl == fr) )

                ( VFloat fl, VFloat fr, GreaterThanOrEquals ) ->
                    Ok ( ctx2, VBool (fl >= fr) )

                ( VFloat fl, VFloat fr, GreaterThan ) ->
                    Ok ( ctx2, VBool (fl > fr) )

                _ ->
                    Err <|
                        InvalidTypes
                            ("Cannot compare " ++ valueToString vl ++ " and " ++ valueToString vr)

        Ternary _ _ _ ->
            Debug.todo "branch 'Ternary _ _ _' not implemented"

        Add _ _ ->
            Debug.todo "branch 'Add _ _' not implemented"

        Subtract _ _ ->
            Debug.todo "branch 'Subtract _ _' not implemented"

        By l r ->
            innerValue2 ctx l r <| \ctx2 vl vr ->
            case ( vl, vr ) of
                ( VFloat fl, VFloat fr ) ->
                    Ok ( ctx2, VFloat <| fl * fr )

                _ ->
                    Err <|
                        InvalidTypes
                            ("Cannot calculate `*` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        Div _ _ ->
            Debug.todo "branch 'Div _ _' not implemented"

        Call "vec2" [ l, r ] ->
            innerValue2 ctx l r <| \ctx2 vl vr ->
            case ( vl, vr ) of
                ( VFloat fl, VFloat fr ) ->
                    Ok ( ctx2, VVec2 fl fr )

                _ ->
                    Err <|
                        InvalidTypes
                            ("Cannot calculate `vec2` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        Call "exp" [ l ] ->
            autovectorizingFloatOp ctx "exp" (\fv -> Basics.e ^ fv) l

        Call "cos" [ l ] ->
            autovectorizingFloatOp ctx "cos" Basics.cos l

        Call "sin" [ l ] ->
            autovectorizingFloatOp ctx "sin" Basics.sin l

        Call name args ->
            Debug.todo <| "branch 'Call \"" ++ name ++ "\" [" ++ String.join ", " (List.map (Debug.toString >> (++) " ") args) ++ " ]' not implemented"

        Negate _ ->
            Debug.todo "branch 'Negate _' not implemented"

        Unknown _ ->
            Debug.todo "branch 'Unknown _' not implemented"

        Dot _ _ ->
            Debug.todo "branch 'Dot _ _' not implemented"

        Array _ _ ->
            Debug.todo "branch 'Array _ _' not implemented"

        Assign _ _ ->
            Debug.todo "branch 'Assign _ _' not implemented"

        AssignCombo _ _ _ ->
            Debug.todo "branch 'AssignCombo _ _' not implemented"

        PostfixIncrement _ ->
            Debug.todo "branch 'PostfixIncrement _' not implemented"

        PostfixDecrement _ ->
            Debug.todo "branch 'PostfixDecrement _' not implemented"


innerValue2 :
    Context
    -> Expr
    -> Expr
    -> (Context -> GlslValue -> GlslValue -> Result ErrorValue ( Context, GlslValue ))
    -> Result ErrorValue ( Context, GlslValue )
innerValue2 ctx l r k =
    innerValue ctx l
        |> Result.andThen
            (\( ctx2, vl ) ->
                innerValue ctx2 r
                    |> Result.andThen
                        (\( ctx3, vr ) ->
                            k ctx3 vl vr
                        )
            )


autovectorizingFloatOp : Context -> String -> (Float -> Float) -> Expr -> Result ErrorValue ( Dict String GlslValue, GlslValue )
autovectorizingFloatOp ctx name inner e =
    innerValue ctx e
        |> Result.andThen
            (\( ctx2, v ) ->
                case v of
                    VFloat fv ->
                        Ok ( ctx2, VFloat <| inner fv )

                    VVec2 x y ->
                        Ok ( ctx2, VVec2 (inner x) (inner y) )

                    VVec3 x y z ->
                        Ok ( ctx2, VVec3 (inner x) (inner y) (inner z) )

                    _ ->
                        Err <|
                            InvalidTypes
                                ("Cannot calculate `" ++ name ++ "` for " ++ valueToString v)
            )


interpret : Context -> Statement a -> Result ErrorValue ( Context, GlslValue )
interpret ctx (Statement s) =
    case s of
        Return e ->
            Expression e
                |> value ctx
                |> Result.map (\( ctx2, val ) -> ( ctx2, val ))

        If c t n ->
            Expression c
                |> value ctx
                |> Result.andThen
                    (\( ctx2, cval ) ->
                        case cval of
                            VBool True ->
                                interpret ctx2 (Statement t)
                                    |> Result.andThen
                                        (\( ctx3, res ) ->
                                            case res of
                                                -- TODO: Fix this
                                                VVoid ->
                                                    interpret ctx3 (Statement n)

                                                _ ->
                                                    Ok ( ctx3, res )
                                        )

                            VBool False ->
                                interpret ctx2 <| Statement n

                            _ ->
                                Err <| InvalidTypes <| "Condition of if evaluated to " ++ Debug.toString cval
                    )

        IfElse _ _ _ _ ->
            Debug.todo "branch 'IfElse _' not implemented"

        Nop ->
            Ok ( ctx, VVoid )

        Line _ ->
            Debug.todo "branch 'Line _' not implemented"

        ExpressionStatement _ _ ->
            Debug.todo "branch 'ExpressionStatement _' not implemented"

        Decl _ _ _ _ ->
            Debug.todo "branch 'Decl _ _ _' not implemented"

        For _ _ _ _ _ _ _ ->
            Debug.todo "branch 'For _ _ _ _ _ _ _' not implemented"

        Break ->
            Debug.todo "branch 'Break' not implemented"

        Continue ->
            Debug.todo "branch 'Continue' not implemented"
