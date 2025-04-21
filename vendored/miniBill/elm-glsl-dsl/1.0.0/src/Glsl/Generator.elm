module Glsl.Generator exposing (File, FunDecl, adds2, adds3, adds4, and, ands, assign, assignAdd, assignBy, assignOut, boolT, break, continue, decl, def, def1, def2, def3, expr, expressionToGlsl, fileToGlsl, float, floatT, for, forDown, forLeq, fun0, fun1, fun2, fun3, fun4, fun5, funDeclToGlsl, gl_FragColor, gl_FragCoord, ifElse, if_, in_, intT, main_, mat3, mat3T, minusOne, nop, one, or, ors, out, return, statementToGlsl, ternary, ternary3, vec2, vec2T, vec2Zero, vec3, vec3T, vec3Zero, vec4, vec4T, vec4Zero, voidT, zero)

import Glsl exposing (BinaryOperation(..), ComboOperation(..), Expr(..), Expression(..), ForDirection(..), In, Mat3, Out, RelationOperation(..), Stat(..), Statement(..), Type(..), TypedName(..), TypingFunction, UnaryOperation(..), Vec2, Vec3, Vec4, build, buildStatement, false, float1, true, unsafeCall0, unsafeCall1, unsafeCall2, unsafeCall3, unsafeCall4, unsafeCall5, unsafeMap2, unsafeMap3, var, withExpression, withStatement)
import Glsl.Functions exposing (vec211, vec3111, vec41111)
import Glsl.Operations exposing (add22, add33, add44)
import Glsl.PrettyPrinter
import Set
import SortedSet


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
    statToGlsl 1 r.stat


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

        For var from rel to direction loop next ->
            [ indent i ("for (int " ++ var ++ " = " ++ exprToGlsl from ++ "; " ++ var ++ " " ++ relationToString rel ++ " " ++ exprToGlsl to ++ "; " ++ var ++ directionToGlsl direction ++ ") {")
            , statToGlsl (i + 1) loop
            , indent i "}"
            , ""
            , statToGlsl i next
            ]
                |> String.join "\n"

        Return e ->
            indent i <| "return " ++ exprToGlsl e ++ ";"

        Break ->
            indent i "break;"

        Continue ->
            indent i "continue;"

        ExpressionStatement e Nop ->
            indent i (exprToGlsl e ++ ";")

        Decl t n (Just e) Nop ->
            indent i (typeToGlsl t ++ " " ++ n ++ " = " ++ exprToGlsl e ++ ";")

        Decl t n Nothing Nop ->
            indent i (typeToGlsl t ++ " " ++ n ++ ";")

        ExpressionStatement e next ->
            indent i (exprToGlsl e ++ ";\n") ++ statToGlsl i next

        Decl t n (Just e) next ->
            indent i (typeToGlsl t ++ " " ++ n ++ " = " ++ exprToGlsl e ++ ";\n") ++ statToGlsl i next

        Decl t n Nothing next ->
            indent i (typeToGlsl t ++ " " ++ n ++ ";\n") ++ statToGlsl i next

        Nop ->
            ""


directionToGlsl : ForDirection -> String
directionToGlsl direction =
    case direction of
        PlusPlus ->
            "++"

        MinusMinus ->
            "--"


typeToGlsl : Type -> String
typeToGlsl type_ =
    case type_ of
        TVoid ->
            "void"

        TFloat ->
            "float"

        TInt ->
            "int"

        TBool ->
            "bool"

        TVec2 ->
            "vec2"

        TVec3 ->
            "vec3"

        TVec4 ->
            "vec4"

        TIVec2 ->
            "ivec2"

        TIVec3 ->
            "ivec3"

        TIVec4 ->
            "ivec4"

        TMat3 ->
            "mat3"

        TIn t ->
            "in " ++ typeToGlsl t

        TOut t ->
            "out " ++ typeToGlsl t


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

        Assign ->
            "="


expressionToGlsl : Expression t -> String
expressionToGlsl (Expression tree) =
    exprToGlsl tree.expr


exprToGlsl : Expr -> String
exprToGlsl tree =
    let
        -- The numbers are precedence numbers from the GLSL spec
        go : Bool -> Expr -> String
        go rec e =
            -- case e of
            --     Unknown u ->
            --         u
            --     _ ->
            go17 rec e

        go17 : Bool -> Expr -> String
        go17 rec e =
            go16 rec e

        go16 : Bool -> Expr -> String
        go16 rec e =
            case e of
                Comparison Assign l r ->
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
                BinaryOperation Or l r ->
                    go14 False l ++ " || " ++ go13 False r

                _ ->
                    go13 rec e

        go13 : Bool -> Expr -> String
        go13 rec e =
            go12 rec e

        go12 : Bool -> Expr -> String
        go12 rec e =
            case e of
                BinaryOperation And l r ->
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
                BinaryOperation Add l r ->
                    go5 False l ++ " + " ++ go4 False r

                BinaryOperation Subtract l r ->
                    go5 False l ++ " - " ++ go4 False r

                _ ->
                    go4 rec e

        go4 : Bool -> Expr -> String
        go4 rec e =
            case e of
                BinaryOperation By l r ->
                    go4 False l ++ " * " ++ go3 False r

                BinaryOperation Div l r ->
                    go4 False l ++ " / " ++ go3 False r

                _ ->
                    go3 rec e

        go3 : Bool -> Expr -> String
        go3 rec e =
            case e of
                UnaryOperation Negate c ->
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
                    Glsl.PrettyPrinter.float f

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


ternary : Expression Bool -> Expression t -> Expression t -> Expression t
ternary =
    unsafeMap3 Ternary


ternary3 : Expression Bool -> Expression Vec3 -> Expression Vec3 -> Expression Vec3
ternary3 =
    unsafeMap3 Ternary


and : Expression Bool -> Expression Bool -> Expression Bool
and =
    unsafeMap2 (BinaryOperation And)


ands : List (Expression Bool) -> Expression Bool
ands es =
    case es of
        [] ->
            true

        h :: t ->
            List.foldl (\e a -> and a e) h t


or : Expression Bool -> Expression Bool -> Expression Bool
or =
    unsafeMap2 (BinaryOperation Or)


ors : List (Expression Bool) -> Expression Bool
ors es =
    case es of
        [] ->
            false

        h :: t ->
            List.foldl (\e a -> or a e) h t


adds2 : List (Expression Vec2) -> Expression Vec2
adds2 es =
    case es of
        [] ->
            vec2Zero

        h :: t ->
            List.foldl (\e a -> add22 a e) h t


adds3 : List (Expression Vec3) -> Expression Vec3
adds3 es =
    case es of
        [] ->
            vec3Zero

        h :: t ->
            List.foldl (\e a -> add33 a e) h t


adds4 : List (Expression Vec4) -> Expression Vec4
adds4 es =
    case es of
        [] ->
            vec4Zero

        h :: t ->
            List.foldl (\e a -> add44 a e) h t



---------------
-- CONSTANTS --
---------------


vec2Zero : Expression Vec2
vec2Zero =
    vec211 zero zero


vec3Zero : Expression Vec3
vec3Zero =
    vec3111 zero zero zero


vec4Zero : Expression Vec4
vec4Zero =
    vec41111 zero zero zero zero


gl_FragColor : Expression Vec4
gl_FragColor =
    var "gl_FragColor"


gl_FragCoord : Expression Vec4
gl_FragCoord =
    var "gl_FragCoord"



----------------
-- CALL UTILS --
----------------


zero : Expression Float
zero =
    float1 0


one : Expression Float
one =
    float1 1


minusOne : Expression Float
minusOne =
    float1 -1



-- STATEMENTS


functionToGlsl : TypedName t -> List ( Type, String ) -> Statement t -> String
functionToGlsl (TypedName rt name) args body =
    let
        argsList =
            String.join ", " (List.map (\( t, n ) -> typeToGlsl t ++ " " ++ n) args)
    in
    String.join "\n" <|
        [ typeToGlsl rt ++ " " ++ name ++ "(" ++ argsList ++ ") {"
        , statementToGlsl body
        , "}"
        ]


argToString : TypedName t -> ( String, String )
argToString (TypedName t n) =
    ( typeToGlsl t, n )


toVar : TypedName t -> Expression t
toVar (TypedName _ n) =
    --pure <| Variable n
    Debug.todo "toVar"


funX :
    (String
     -> List String
     -> a
    )
    -> TypingFunction t
    -> String
    -> Statement t
    -> List ( Type, String )
    -> a
funX call typeF name body args =
    let
        typed =
            typeF name

        (Statement s) =
            body

        funGlsl =
            functionToGlsl typed args body
    in
    call name
        (s.deps
            |> SortedSet.insert funGlsl
            |> SortedSet.toList
        )


main_ : (Statement () -> Statement ()) -> Expression ()
main_ stat =
    fun0 voidT "main" <| \_ -> stat nop


fun0 :
    TypingFunction t
    -> String
    -> (() -> Statement t)
    -> Expression t
fun0 typeF name body =
    funX unsafeCall0
        typeF
        name
        (body ())
        []


fun1 :
    TypingFunction t
    -> String
    -> TypedName a
    -> (Expression a -> Statement t)
    -> Expression a
    -> Expression r
fun1 typeF name (TypedName t0 arg0) body =
    funX unsafeCall1
        typeF
        name
        (body (var arg0))
        [ ( t0, arg0 ) ]


fun2 :
    TypingFunction t
    -> String
    -> TypedName a
    -> TypedName b
    -> (Expression a -> Expression b -> Statement t)
    -> Expression a
    -> Expression b
    -> Expression t
fun2 typeF name (TypedName t0 arg0) (TypedName t1 arg1) body =
    funX unsafeCall2
        typeF
        name
        (body (var arg0) (var arg1))
        [ ( t0, arg0 ), ( t1, arg1 ) ]


fun3 :
    TypingFunction t
    -> String
    -> TypedName a
    -> TypedName b
    -> TypedName c
    -> (Expression a -> Expression b -> Expression c -> Statement t)
    -> Expression a
    -> Expression b
    -> Expression c
    -> Expression t
fun3 typeF name (TypedName t0 arg0) (TypedName t1 arg1) (TypedName t2 arg2) body =
    funX unsafeCall3
        typeF
        name
        (body (var arg0) (var arg1) (var arg2))
        [ ( t0, arg0 ), ( t1, arg1 ), ( t2, arg2 ) ]


fun4 :
    TypingFunction t
    -> String
    -> TypedName a
    -> TypedName b
    -> TypedName c
    -> TypedName d
    -> (Expression a -> Expression b -> Expression c -> Expression d -> Statement t)
    -> Expression a
    -> Expression b
    -> Expression c
    -> Expression d
    -> Expression t
fun4 typeF name (TypedName t0 arg0) (TypedName t1 arg1) (TypedName t2 arg2) (TypedName t3 arg3) body =
    funX unsafeCall4
        typeF
        name
        (body (var arg0) (var arg1) (var arg2) (var arg3))
        [ ( t0, arg0 ), ( t1, arg1 ), ( t2, arg2 ), ( t3, arg3 ) ]


fun5 :
    TypingFunction t
    -> String
    -> TypedName a
    -> TypedName b
    -> TypedName c
    -> TypedName d
    -> TypedName e
    -> (Expression a -> Expression b -> Expression c -> Expression d -> Expression e -> Statement t)
    -> Expression a
    -> Expression b
    -> Expression c
    -> Expression d
    -> Expression e
    -> Expression t
fun5 typeF name (TypedName t0 arg0) (TypedName t1 arg1) (TypedName t2 arg2) (TypedName t3 arg3) (TypedName t4 arg4) body =
    funX unsafeCall5
        typeF
        name
        (body (var arg0) (var arg1) (var arg2) (var arg3) (var arg4))
        [ ( t0, arg0 ), ( t1, arg1 ), ( t2, arg2 ), ( t3, arg3 ), ( t4, arg4 ) ]


if_ : Expression Bool -> Statement r -> (() -> Statement r) -> Statement r
if_ cond ifTrue next =
    build If
        |> withExpression cond
        |> withStatement ifTrue
        |> withStatement (next ())
        |> buildStatement


ifElse : Expression Bool -> Statement s -> Statement s -> (() -> Statement s) -> Statement s
ifElse cond ifTrue ifFalse next =
    build IfElse
        |> withExpression cond
        |> withStatement ifTrue
        |> withStatement ifFalse
        |> withStatement (next ())
        |> buildStatement


for :
    ( String, Expression Int, Expression Int )
    -> (Expression Int -> Statement r -> Statement r)
    -> Statement r
    -> Statement r
for ( varName, from, to ) loop next =
    build (\f t -> For varName f LessThan t PlusPlus)
        |> withExpression from
        |> withExpression to
        |> withStatement (loop (var varName) unsafeNop)
        |> withStatement next
        |> buildStatement


forLeq :
    ( String, Expression Int, Expression Int )
    -> (Expression Int -> Statement r -> Statement r)
    -> Statement r
    -> Statement r
forLeq ( varName, from, to ) loop next =
    build (\f t -> For varName f LessThanOrEquals t PlusPlus)
        |> withExpression from
        |> withExpression to
        |> withStatement (loop (var varName) unsafeNop)
        |> withStatement next
        |> buildStatement


forDown :
    ( String, Expression Int, Expression Int )
    -> (Expression Int -> Statement r -> Statement r)
    -> Statement r
    -> Statement r
forDown ( varName, from, to ) loop next =
    build (\f t -> For varName f GreaterThan t MinusMinus)
        |> withExpression from
        |> withExpression to
        |> withStatement (loop (var varName) unsafeNop)
        |> withStatement next
        |> buildStatement


return : Expression r -> Statement r
return e =
    build Return
        |> withExpression e
        |> buildStatement


break : Statement a
break =
    build Break
        |> buildStatement


continue : Statement a
continue =
    build Continue
        |> buildStatement


decl : TypingFunction t -> String -> (Expression t -> Statement r) -> Statement r
decl typeF name k =
    let
        (TypedName t n) =
            typeF name
    in
    build (Decl t n Nothing)
        |> withStatement (k (var n))
        |> buildStatement


def : TypingFunction t -> String -> Expression t -> (Expression t -> Statement r) -> Statement r
def typeF name val k =
    def1 ( typeF name, val ) k


float : String -> Expression Float -> (Expression Float -> Statement r) -> Statement r
float =
    def floatT


vec2 : String -> Expression Vec2 -> (Expression Vec2 -> Statement r) -> Statement r
vec2 =
    def vec2T


vec3 : String -> Expression Vec3 -> (Expression Vec3 -> Statement r) -> Statement r
vec3 =
    def vec3T


vec4 : String -> Expression Vec4 -> (Expression Vec4 -> Statement r) -> Statement r
vec4 =
    def vec4T


mat3 : String -> Expression Mat3 -> (Expression Mat3 -> Statement r) -> Statement r
mat3 =
    def mat3T


def1 :
    ( TypedName a, Expression a )
    -> (Expression a -> Statement r)
    -> Statement r
def1 ( tn0, val0 ) k =
    let
        (TypedName t0 n0) =
            tn0
    in
    build
        (\v0 k0 -> Decl t0 n0 (Just v0) k0)
        |> withExpression val0
        |> withStatement (k (var n0))
        |> buildStatement


def2 :
    ( TypedName a, Expression a )
    -> ( TypedName b, Expression b )
    -> (Expression a -> Expression b -> Statement r)
    -> Statement r
def2 ( tn0, val0 ) ( tn1, val1 ) k =
    def1 ( tn0, val0 )
        (\v0 ->
            def1 ( tn1, val1 )
                (\v1 -> k v0 v1)
        )


def3 :
    ( TypedName a, Expression a )
    -> ( TypedName b, Expression b )
    -> ( TypedName c, Expression c )
    -> (Expression a -> Expression b -> Expression c -> Statement r)
    -> Statement r
def3 ( tn0, val0 ) ( tn1, val1 ) ( tn2, val2 ) k =
    def1 ( tn0, val0 )
        (\v0 ->
            def1 ( tn1, val1 )
                (\v1 ->
                    def1 ( tn2, val2 )
                        (\v2 -> k v0 v1 v2)
                )
        )


assign : Expression t -> Expression t -> Expression t
assign name e =
    unsafeMap2 (Comparison Assign) name e


assignOut : Expression (Out t) -> Expression t -> Expression t
assignOut name e =
    unsafeMap2 (Comparison Assign) name e


expr : Expression t -> (() -> Statement r) -> Statement r
expr e s =
    build ExpressionStatement
        |> withExpression e
        |> withStatement (s ())
        |> buildStatement


nop : Statement ()
nop =
    Statement { stat = Nop, deps = SortedSet.empty }


unsafeNop : Statement r
unsafeNop =
    Statement { stat = Nop, deps = SortedSet.empty }


assignAdd : Expression t -> Expression t -> (() -> Statement q) -> Statement q
assignAdd name val =
    expr <| unsafeMap2 (AssignCombo ComboAdd) name val


assignBy : Expression t -> Expression t -> (() -> Statement q) -> Statement q
assignBy name val =
    expr <| unsafeMap2 (AssignCombo ComboBy) name val



-- TYPES


voidT : TypingFunction ()
voidT n =
    TypedName TVoid n


boolT : TypingFunction Bool
boolT n =
    TypedName TBool n


intT : TypingFunction Int
intT n =
    TypedName TInt n


floatT : TypingFunction Float
floatT n =
    TypedName TFloat n


vec2T : TypingFunction Vec2
vec2T n =
    TypedName TVec2 n


vec3T : TypingFunction Vec3
vec3T n =
    TypedName TVec3 n


vec4T : TypingFunction Vec4
vec4T n =
    TypedName TVec4 n


mat3T : TypingFunction Mat3
mat3T n =
    TypedName TMat3 n


out : TypingFunction t -> TypingFunction (Out t)
out inner n =
    let
        (TypedName t name) =
            inner n
    in
    TypedName (TOut t) name


in_ : TypingFunction t -> TypingFunction (In t)
in_ inner n =
    let
        (TypedName t name) =
            inner n
    in
    TypedName (TIn t) name
