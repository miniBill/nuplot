module UI.Glsl.Generator exposing (Constant, Context, Continue, ErrorValue(..), File, FunDecl, GlslValue(..), Mat3, Vec2, Vec3, Vec4, abs2, abs4, abs_, acos2, acos_, add, add2, add3, add33, add4, adds2, adds3, adds4, and, ands, arr, assign, assignAdd, assignBy, atan2_, atan_, bool, boolT, break, by, by2, by3, byF, ceil_, constant, continue, cos_, cosh, cross, decl, def, div, div2, divF, dot, eq, exp, expr, expressionToGlsl, false, fileToGlsl, float, floatCast, floatT, floatToGlsl, floor_, for, forDown, forLeq, fract, fun0, fun1, fun2, fun3, fun4, fun5, funDeclToGlsl, fwidth, geq, gl_FragColor, gl_FragCoord, gt, hl2rgb, ifElse, if_, in_, int, intCast, intT, interpret, length, leq, log, log2, lt, mat3T, max3, max4, max_, min_, minusOne, mix, mod, negate2, negateConst, negate_, neq, normalize, normalize3, one, or, ors, out, postfixDecrement, postfixIncrement, pow, radians_, return, round_, sign, sin_, sinh, smoothstep, sqrt_, statementToGlsl, subtract, subtract2, subtract3, subtract4, subtractConst, tan_, ternary, ternary3, true, uniform, unknown, unsafeCall, value, valueToString, vec2, vec2T, vec2Zero, vec3, vec3T, vec3Zero, vec4, vec4T, vec4Zero, vec4_1_3, vec4_3_1, voidT, zero)

import Dict exposing (Dict)
import Glsl.Helper exposing (BinaryOperation(..), ComboOperation(..), Expr(..), Expression(..), Name(..), RelationOperation(..), Stat(..), Statement(..), Type(..), TypedName(..), TypingFunction)
import Set


type alias File =
    List FunDecl


type FunDecl
    = FunDecl { name : String, type_ : String, body : String }



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
    statToGlsl 1 r


statToGlsl : Int -> Stat -> String
statToGlsl i c =
    case c of
        If cond t n ->
            [ indent i ("if (" ++ exprToGlsl cond ++ ") {")
            , statToGlsl (i + 1) t
            , indent i "}"
            , ""
            , statToGlsl i n
            ]
                |> String.join "\n"

        IfElse cond t ((If _ _ _) as f) n ->
            [ indent i ("if (" ++ exprToGlsl cond ++ ") {")
            , statToGlsl (i + 1) t
            , indent i <| "} else " ++ String.trimLeft (statToGlsl i f)
            , statToGlsl i n
            ]
                |> String.join "\n"

        IfElse cond t ((IfElse _ _ _ _) as f) n ->
            [ indent i ("if (" ++ exprToGlsl cond ++ ") {")
            , statToGlsl (i + 1) t
            , indent i <| "} else " ++ String.trimLeft (statToGlsl i f)
            , statToGlsl i n
            ]
                |> String.join "\n"

        IfElse cond t f n ->
            [ indent i ("if (" ++ exprToGlsl cond ++ ") {")
            , statToGlsl (i + 1) t
            , indent i "} else {"
            , statToGlsl (i + 1) f
            , indent i "}"
            , ""
            , statToGlsl i n
            ]
                |> String.join "\n"

        For var from rel to step loop next ->
            [ indent i ("for (int " ++ var ++ " = " ++ exprToGlsl (Expression from) ++ "; " ++ var ++ " " ++ relationToString rel ++ " " ++ exprToGlsl (Expression to) ++ "; " ++ exprToGlsl (Expression step) ++ ") {")
            , statToGlsl (i + 1) loop
            , indent i "}"
            , ""
            , statToGlsl i next
            ]
                |> String.join "\n"

        Line l ->
            indent i l ++ ";"

        Return e ->
            indent i <| "return " ++ exprToGlsl (Expression e) ++ ";"

        Break ->
            indent i "break;"

        Continue ->
            indent i "continue;"

        ExpressionStatement e Nop ->
            indent i (exprToGlsl (Expression e) ++ ";")

        Decl t (Name n) (Just e) Nop ->
            indent i (t ++ " " ++ n ++ " = " ++ exprToGlsl (Expression e) ++ ";")

        Decl t (Name n) Nothing Nop ->
            indent i (t ++ " " ++ n ++ ";")

        ExpressionStatement e next ->
            indent i (exprToGlsl (Expression e) ++ ";\n") ++ statToGlsl i next

        Decl t (Name n) (Just e) next ->
            indent i (t ++ " " ++ n ++ " = " ++ exprToGlsl (Expression e) ++ ";\n") ++ statToGlsl i next

        Decl t (Name n) Nothing next ->
            indent i (t ++ " " ++ n ++ ";\n") ++ statToGlsl i next

        Nop ->
            ""


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
    exprToGlsl tree.expr


exprToGlsl : Expr -> String
exprToGlsl tree =
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


ternary : ExpressionXBool -> Expression t -> Expression t -> Expression t
ternary =
    expr3 Ternary


ternary3 : ExpressionX xa Bool -> ExpressionX xb Vec3 -> ExpressionX xc Vec3 -> Expression Vec3
ternary3 =
    expr33 Ternary


and : Expression Bool -> Expression Bool -> Expression Bool
and =
    expr2 And


ands : List (Expression Bool) -> Expression Bool
ands es =
    case es of
        [] ->
            true

        h :: t ->
            List.foldl (\e a -> expr2 And a e) h t


or : Expression Bool -> Expression Bool -> Expression Bool
or =
    expr2 Or


ors : List (Expression Bool) -> Expression Bool
ors es =
    case es of
        [] ->
            false

        h :: t ->
            List.foldl (\e a -> expr2 Or a e) h t


adds2 : List (Expression Vec2) -> Expression Vec2
adds2 es =
    case es of
        [] ->
            vec2Zero

        h :: t ->
            List.foldl (\e a -> add2 a e) h t


adds3 : List (Expression Vec3) -> Expression Vec3
adds3 es =
    case es of
        [] ->
            vec3Zero

        h :: t ->
            List.foldl (\e a -> add3 a e) h t


adds4 : List (Expression Vec4) -> Expression Vec4
adds4 es =
    case es of
        [] ->
            vec4Zero

        h :: t ->
            List.foldl (\e a -> add4 a e) h t


pure : Expr -> Expression t
pure e =
    Expression { expr = e, deps = Set.empty }


true : Expression Bool
true =
    pure <| Bool True


false : Expression Bool
false =
    pure <| Bool False



------------------------------
-- FUNCTIONS AND OPERATIONS --
------------------------------


add : Expression t -> Expression t -> Expression t
add =
    expr2 Add


add2 : Expression Vec2 -> Expression Vec2 -> Expression Vec2
add2 =
    expr22 Add


add3 : Expression Vec3 -> Expression Vec3 -> Expression Vec3
add3 =
    expr23 Add


add4 : Expression Vec4 -> Expression Vec4 -> Expression Vec4
add4 =
    expr24 Add


add33 : Expression Mat3 -> Expression Mat3 -> Expression Mat3
add33 =
    expr233 Add


subtract : Expression t -> Expression t -> Expression t
subtract =
    expr2 Subtract


subtractConst : ExpressionX { a | isConstant : Constant } t -> ExpressionX { b | isConstant : Constant } t -> ExpressionX { isConstant : Constant } t
subtractConst l r =
    { base = (expr2 Subtract l r).base
    , isConstant = Constant
    }


subtract2 : ExpressionXVec2 -> Expression Vec2 -> Expression Vec2
subtract2 =
    expr22 Subtract


subtract3 : Expression Vec3 -> Expression Vec3 -> Expression Vec3
subtract3 =
    expr23 Subtract


subtract4 : Expression Vec4 -> Expression Vec4 -> Expression Vec4
subtract4 =
    expr24 Subtract


negate_ : Expression t -> Expression t
negate_ =
    expr1 Negate


negateConst : ExpressionX { a | isConstant : Constant } t -> ExpressionX { isConstant : Constant } t
negateConst l =
    { base = (expr1 Negate l).base
    , isConstant = Constant
    }


negate2 : Expression Vec2 -> Expression Vec2
negate2 e =
    dotted2Internal (Negate <| unwrapExpression e)


postfixIncrement : Expression t -> Expression t
postfixIncrement =
    expr1 PostfixIncrement


postfixDecrement : Expression t -> Expression t
postfixDecrement =
    expr1 PostfixDecrement


by : Expression t -> Expression t -> Expression t
by =
    expr2 By


by2 : ExpressionXVec2 -> Expression Vec2 -> Expression Vec2
by2 =
    expr22 By


by3 : Expression Vec3 -> Expression Vec3 -> Expression Vec3
by3 =
    expr23 By


byF : Expression Float -> Expression t -> Expression t
byF =
    expr2 By


divF : Expression t -> Expression Float -> Expression t
divF =
    expr2 Div


div : Expression t -> Expression t -> Expression t
div =
    expr2 Div


div2 : Expression Vec2 -> Expression Vec2 -> Expression Vec2
div2 =
    expr22 Div


lt : Expression t -> Expression t -> Expression Bool
lt =
    expr2 (Comparison LessThan)


leq : Expression t -> Expression t -> Expression Bool
leq =
    expr2 (Comparison LessThanOrEquals)


eq : Expression t -> Expression t -> Expression Bool
eq =
    expr2 (Comparison Equals)


neq : Expression t -> Expression t -> Expression Bool
neq =
    expr2 (Comparison NotEquals)


geq : Expression t -> Expression t -> Expression Bool
geq =
    expr2 (Comparison GreaterThanOrEquals)


gt : Expression t -> Expression t -> Expression Bool
gt =
    expr2 (Comparison GreaterThan)


abs_ : Expression t -> Expression t
abs_ =
    call1Internal "abs"


abs2 : Expression Vec2 -> Expression Vec2
abs2 =
    dotted2 << call1Internal "abs"


abs4 : Expression Vec4 -> Expression Vec4
abs4 =
    dotted4 << call1Internal "abs"


mix : Expression t -> Expression t -> Expression Float -> Expression t
mix x y a =
    dotted1 <| call3Internal "mix" x y a


smoothstep : Expression Float -> Expression Float -> Expression Float -> Expression t
smoothstep x y a =
    dotted1 <| call3Internal "smoothstep" x y a


exp : Expression t -> Expression t
exp =
    call1Internal "exp"


sqrt_ : Expression t -> Expression t
sqrt_ =
    call1Internal "sqrt"


sin_ : Expression t -> Expression t
sin_ =
    call1Internal "sin"


cos_ : Expression t -> Expression t
cos_ =
    call1Internal "cos"


tan_ : Expression t -> Expression t
tan_ =
    call1Internal "tan"


acos_ : Expression t -> Expression t
acos_ =
    call1Internal "acos"


acos2 : Expression Vec2 -> Expression Vec2
acos2 =
    dotted2 << call1Internal "acos"


sinh : Expression t -> Expression t
sinh =
    call1Internal "sinh"


cosh : Expression t -> Expression t
cosh =
    call1Internal "cosh"


log : Expression t -> Expression t
log =
    call1Internal "log"


mod : Expression t -> Expression t -> Expression t
mod l r =
    dotted1 (call2Internal "mod" l r)


min_ : Expression t -> Expression t -> Expression t
min_ l r =
    dotted1 (call2Internal "min" l r)


max_ : Expression t -> Expression t -> Expression t
max_ l r =
    dotted1 (call2Internal "max" l r)


max3 : Expression Vec3 -> Expression Vec3 -> Expression Vec3
max3 l r =
    dotted3 (call2Internal "max" l r)


max4 : Expression Vec4 -> Expression Vec4 -> Expression Vec4
max4 l r =
    dotted4 (call2Internal "max" l r)


hl2rgb : ExpressionXFloat -> Expression Float -> Expression Vec3
hl2rgb h l =
    dotted3 (call2Internal "hl2rgb" h l)


pow : Expression Float -> Expression Float -> Expression Float
pow l r =
    dotted1 (call2Internal "pow" l r)


log2 : ExpressionXFloat -> Expression Float
log2 =
    call1Internal "log2"


ceil_ : Expression t -> Expression t
ceil_ =
    call1Internal "ceil"


floor_ : Expression t -> Expression t
floor_ =
    call1Internal "floor"


round_ : Expression t -> Expression t
round_ =
    call1Internal "round"


sign : Expression t -> Expression t
sign =
    call1Internal "sign"


radians_ : Expression Float -> Expression Float
radians_ =
    call1Internal "radians"


normalize : Expression t -> Expression t
normalize =
    call1Internal "normalize"


normalize3 : ExpressionXVec3 -> Expression Vec3
normalize3 =
    dotted3 << call1Internal "normalize"


length : Expression t -> Expression Float
length =
    call1Internal "length"


fwidth : Expression t -> Expression t
fwidth =
    call1Internal "fwidth"


cross : Expression Vec3 -> Expression Vec3 -> Expression Vec3
cross l r =
    dotted3 <| call2Internal "cross" l r


atan_ : Expression Float -> Expression Float
atan_ =
    call1Internal "atan"


atan2_ : ExpressionXFloat -> Expression Float -> Expression Float
atan2_ l r =
    dotted1 <| call2Internal "atan" l r


intCast : Expression Float -> Expression Int
intCast =
    call1Internal "int"


floatCast : ExpressionXInt -> Expression Float
floatCast =
    call1Internal "float"


fract : Expression Float -> Expression Float
fract =
    call1Internal "fract"


arr : ExpressionXMat3 -> Expression Int -> Expression Vec3
arr =
    expr2 Array


dot : Expression t -> Expression t -> Expression Float
dot l r =
    dotted1 (call2Internal "dot" l r)



---------------
-- CONSTANTS --
---------------


vec2Zero : Expression Vec2
vec2Zero =
    vec2 zero zero


vec3Zero : Expression Vec3
vec3Zero =
    vec3 zero zero zero


vec4Zero : Expression Vec4
vec4Zero =
    vec4 zero zero zero zero


gl_FragColor : Expression Vec4
gl_FragColor =
    dottedVariable4 "gl_FragColor"


gl_FragCoord : Expression Vec4
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


zero : Expression Float
zero =
    float 0


one : Expression Float
one =
    float 1


minusOne : Expression Float
minusOne =
    float -1


float : Float -> Expression Float
float f =
    { base = Expression (Float f)
    , isConstant = Constant
    }


int : Int -> Expression Int
int i =
    { base = Expression <| Int i
    , isConstant = Constant
    }


bool : Bool -> Expression Bool
bool b =
    { base = Expression <| Bool b
    , isConstant = Constant
    }


constant : TypingFunction t r -> String -> Expression t
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



-- STATEMENTS


funInternal : TypedName t -> List ( String, String ) -> Statement t -> FunDecl
funInternal (TypedName (Type rt) (Name name)) args body =
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


argToString : TypedName t -> ( String, String )
argToString (TypedName (Type t) (Name n)) =
    ( t, n )


toVar : TypedName t -> Expression t
toVar (TypedName _ (Name n)) =
    pure <| Variable n


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


dottedVariable1 : String -> Expression t
dottedVariable1 v =
    dotted1 (Expression (Variable v))


dottedVariable2 : String -> Expression Vec2
dottedVariable2 v =
    dotted2 (Expression (Variable v))


dottedVariable3 : String -> Expression Vec3
dottedVariable3 v =
    dotted3 (Expression (Variable v))


dottedVariable4 : String -> Expression Vec4
dottedVariable4 v =
    dotted4 (Expression (Variable v))


type alias Continue s =
    () -> Statement s


if_ : Expression Bool -> (Continue r -> Statement r) -> Continue r -> Statement r
if_ cond ifTrue next =
    Statement <| If (unwrapExpression cond) (unwrapStatement <| ifTrue internalNop) (unwrapLazyStatement next)


ifElse : Expression Bool -> (Continue s -> Statement s) -> (Continue s -> Statement s) -> Continue s -> Statement s
ifElse cond ifTrue ifFalse next =
    Statement <| IfElse (unwrapExpression cond) (unwrapStatement <| ifTrue internalNop) (unwrapStatement <| ifFalse internalNop) (unwrapLazyStatement next)


for :
    ( String, ExpressionX { a | isConstant : Constant } Int, ExpressionX { b | isConstant : Constant } Int )
    -> (Expression Int -> Continue r -> Statement r)
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
    -> (Expression Int -> Continue r -> Statement r)
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
    -> (Expression Int -> Continue r -> Statement r)
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


return : ExpressionXr -> Continue x -> Statement r
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


assign : Expression t -> Expression t -> Expression t
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


expr : Expression t -> Continue q -> Statement q
expr e next =
    Statement <| ExpressionStatement (unwrapExpression e) (unwrapLazyStatement next)


assignAdd : Expression t -> Expression t -> Continue q -> Statement q
assignAdd name e next =
    Statement <| ExpressionStatement (AssignCombo ComboAdd (unwrapExpression name) (unwrapExpression e)) (unwrapLazyStatement next)


assignBy : Expression t -> Expression t -> Continue q -> Statement q
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


voidT : TypingFunction ()
voidT n =
    TypedName (Type "void") (Name n)


boolT : TypingFunction Bool
boolT n =
    TypedName (Type "bool") (Name n)


intT : TypingFunction Int
intT n =
    TypedName (Type "int") (Name n)


floatT : TypingFunction Float
floatT n =
    TypedName (Type "float") (Name n)


vec2T : TypingFunction Vec2
vec2T n =
    TypedName (Type "vec2") (Name n)


vec3T : TypingFunction Vec3
vec3T n =
    TypedName (Type "vec3") (Name n)


vec4T : TypingFunction Vec4
vec4T n =
    TypedName (Type "vec4") (Name n)


mat3T : TypingFunction Mat3
mat3T n =
    TypedName (Type "mat3") (Name n)


out : TypingFunction t -> TypingFunction t
out inner n =
    let
        (TypedName (Type t) name) =
            inner n
    in
    TypedName (Type <| "out " ++ t) name


in_ : TypingFunction t -> TypingFunction t
in_ inner n =
    let
        (TypedName (Type t) name) =
            inner n
    in
    TypedName (Type <| "in " ++ t) name



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
    innerValue initialContext input.expr


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
            innerValue2 ctx l r <|
                \ctx2 vl vr ->
                    case ( vl, vr ) of
                        ( VBool bl, VBool br ) ->
                            Ok ( ctx2, VBool <| bl && br )

                        _ ->
                            Err <|
                                InvalidTypes
                                    ("Cannot calculate `and` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        Or l r ->
            innerValue2 ctx l r <|
                \ctx2 vl vr ->
                    case ( vl, vr ) of
                        ( VBool bl, VBool br ) ->
                            Ok ( ctx2, VBool <| bl || br )

                        _ ->
                            Err <|
                                InvalidTypes
                                    ("Cannot calculate `or` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        Comparison k l r ->
            innerValue2 ctx l r <|
                \ctx2 vl vr ->
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

        BinaryOperation Add _ _ ->
            Debug.todo "branch 'Add _ _' not implemented"

        BinaryOperation Subtract _ _ ->
            Debug.todo "branch 'Subtract _ _' not implemented"

        BinaryOperation By l r ->
            innerValue2 ctx l r <|
                \ctx2 vl vr ->
                    case ( vl, vr ) of
                        ( VFloat fl, VFloat fr ) ->
                            Ok ( ctx2, VFloat <| fl * fr )

                        _ ->
                            Err <|
                                InvalidTypes
                                    ("Cannot calculate `*` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        BinaryOperation Div _ _ ->
            Debug.todo "branch 'Div _ _' not implemented"

        Call "vec2" [ l, r ] ->
            innerValue2 ctx l r <|
                \ctx2 vl vr ->
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
