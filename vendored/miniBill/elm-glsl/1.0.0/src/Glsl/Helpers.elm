module Glsl.Helpers exposing (File, FunDecl, assign, assignAdd, assignBy, assignOut, boolT, break, const, const_, continue, decl, def, def1, expr, expressionToGlsl, fileToGlsl, float, floatT, for, forDown, forLeq, fun0, fun1, fun1_, fun2, fun2_, fun3, fun3_, fun4, fun4_, fun5, fun5_, funDeclToGlsl, ifElse, if_, in_, intT, main_, mat2T, mat3, mat3T, nop, out, return, statementToGlsl, vec2, vec2T, vec3, vec3T, vec4, vec4T, voidT)

import Glsl exposing (BinaryOperation(..), Bool_, Declaration(..), Expr(..), Expression(..), Float_, Int_, Mat2, Mat3, Out, RelationOperation(..), Stat(..), Statement(..), Type(..), TypedName(..), TypingFunction, UnaryOperation(..), Vec2, Vec3, Vec4, build, buildStatement, unsafeCall0, unsafeCall1, unsafeCall2, unsafeCall3, unsafeCall4, unsafeCall5, unsafeMap2, unsafeVar, withContinuation, withExpression, withStatement)
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
    r.stat
        |> Glsl.PrettyPrinter.stat 1


expressionToGlsl : Expression t -> String
expressionToGlsl (Expression tree) =
    Glsl.PrettyPrinter.expr tree.expr



-- STATEMENTS


functionToGlsl : TypedName t -> List ( Type, String ) -> Statement t -> String
functionToGlsl (TypedName rt name) args (Statement body) =
    Glsl.PrettyPrinter.declaration
        (FunctionDeclaration
            { args = args
            , returnType = rt
            , name = name
            , stat = body.stat
            }
        )


funX :
    (String
     -> List String
     -> a
    )
    -> TypingFunction t
    -> String
    -> Statement t
    -> List ( Type, String )
    -> { declaration : Declaration, call : a }
funX call typeF name body args =
    let
        ((TypedName returnType _) as typed) =
            typeF name

        (Statement s) =
            body

        funGlsl : String
        funGlsl =
            functionToGlsl typed args body
    in
    { declaration =
        FunctionDeclaration
            { returnType = returnType
            , name = name
            , args = args
            , stat = s.stat
            }
    , call =
        call name
            (s.deps
                |> SortedSet.insert funGlsl
                |> SortedSet.toList
            )
    }


const :
    (String -> TypedName t)
    -> String
    -> Expression t
    -> Expression t
const typeF name value =
    (const_ typeF name value).value


const_ :
    (String -> TypedName t)
    -> String
    -> Expression t
    ->
        { declaration : Declaration
        , value : Expression t
        }
const_ typeF name value =
    let
        (TypedName constType _) =
            typeF name

        (Expression e) =
            value

        constGlsl : String
        constGlsl =
            "const "
                ++ Glsl.PrettyPrinter.type_ constType
                ++ " "
                ++ name
                ++ " = "
                ++ Glsl.PrettyPrinter.expr e.expr
                ++ ";"
    in
    { declaration =
        ConstDeclaration
            { tipe = constType
            , name = name
            , value = e.expr
            }
    , value =
        Expression
            { deps =
                e.deps
                    |> SortedSet.insert constGlsl
            , expr = Variable name
            }
    }


main_ :
    (Statement () -> Statement ())
    ->
        { declaration : Declaration
        , call : Expression ()
        }
main_ stat =
    fun0 voidT "main" <| \_ -> stat nop


fun0 :
    TypingFunction t
    -> String
    -> (() -> Statement t)
    -> { declaration : Declaration, call : Expression t }
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
    -> Expression t
fun1 typeF name arg0 body =
    (fun1_ typeF name arg0 body).call


fun1_ :
    TypingFunction t
    -> String
    -> TypedName a
    -> (Expression a -> Statement t)
    ->
        { declaration : Declaration
        , call : Expression a -> Expression t
        }
fun1_ typeF name (TypedName t0 arg0) body =
    funX unsafeCall1
        typeF
        name
        (body (unsafeVar arg0))
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
fun2 typeF name arg0 arg1 body =
    (fun2_ typeF name arg0 arg1 body).call


fun2_ :
    TypingFunction t
    -> String
    -> TypedName a
    -> TypedName b
    -> (Expression a -> Expression b -> Statement t)
    ->
        { declaration : Declaration
        , call : Expression a -> Expression b -> Expression t
        }
fun2_ typeF name (TypedName t0 arg0) (TypedName t1 arg1) body =
    funX unsafeCall2
        typeF
        name
        (body (unsafeVar arg0) (unsafeVar arg1))
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
fun3 typeF name arg0 arg1 arg2 body =
    (fun3_ typeF name arg0 arg1 arg2 body).call


fun3_ :
    TypingFunction t
    -> String
    -> TypedName a
    -> TypedName b
    -> TypedName c
    -> (Expression a -> Expression b -> Expression c -> Statement t)
    ->
        { declaration : Declaration
        , call : Expression a -> Expression b -> Expression c -> Expression t
        }
fun3_ typeF name (TypedName t0 arg0) (TypedName t1 arg1) (TypedName t2 arg2) body =
    funX unsafeCall3
        typeF
        name
        (body (unsafeVar arg0) (unsafeVar arg1) (unsafeVar arg2))
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
fun4 typeF name arg0 arg1 arg2 arg3 body =
    (fun4_ typeF name arg0 arg1 arg2 arg3 body).call


fun4_ :
    TypingFunction t
    -> String
    -> TypedName a
    -> TypedName b
    -> TypedName c
    -> TypedName d
    -> (Expression a -> Expression b -> Expression c -> Expression d -> Statement t)
    ->
        { declaration : Declaration
        , call : Expression a -> Expression b -> Expression c -> Expression d -> Expression t
        }
fun4_ typeF name (TypedName t0 arg0) (TypedName t1 arg1) (TypedName t2 arg2) (TypedName t3 arg3) body =
    funX unsafeCall4
        typeF
        name
        (body (unsafeVar arg0) (unsafeVar arg1) (unsafeVar arg2) (unsafeVar arg3))
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
fun5 typeF name arg0 arg1 arg2 arg3 arg4 body =
    (fun5_ typeF name arg0 arg1 arg2 arg3 arg4 body).call


fun5_ :
    TypingFunction t
    -> String
    -> TypedName a
    -> TypedName b
    -> TypedName c
    -> TypedName d
    -> TypedName e
    -> (Expression a -> Expression b -> Expression c -> Expression d -> Expression e -> Statement t)
    ->
        { declaration : Declaration
        , call : Expression a -> Expression b -> Expression c -> Expression d -> Expression e -> Expression t
        }
fun5_ typeF name (TypedName t0 arg0) (TypedName t1 arg1) (TypedName t2 arg2) (TypedName t3 arg3) (TypedName t4 arg4) body =
    funX unsafeCall5
        typeF
        name
        (body (unsafeVar arg0) (unsafeVar arg1) (unsafeVar arg2) (unsafeVar arg3) (unsafeVar arg4))
        [ ( t0, arg0 ), ( t1, arg1 ), ( t2, arg2 ), ( t3, arg3 ), ( t4, arg4 ) ]


if_ : Expression Bool_ -> Statement r -> (() -> Statement r) -> Statement r
if_ cond ifTrue next =
    build If
        |> withExpression cond
        |> withStatement ifTrue
        |> withContinuation next


ifElse : Expression Bool_ -> Statement s -> Statement s -> (() -> Statement s) -> Statement s
ifElse cond ifTrue ifFalse next =
    build IfElse
        |> withExpression cond
        |> withStatement ifTrue
        |> withStatement ifFalse
        |> withContinuation next


for :
    ( String, Expression Int, Expression Int )
    -> (Expression Int -> Statement () -> Statement ())
    -> (() -> Statement r)
    -> Statement r
for ( name, from, to ) loop next =
    build (\f t -> For (Just <| Decl TInt name (Just f)) (BinaryOperation (Variable name) (RelationOperation LessThan) t) (UnaryOperation PostfixIncrement (Variable name)))
        |> withExpression from
        |> withExpression to
        |> withStatement (loop (unsafeVar name) nop)
        |> withContinuation next


forLeq :
    ( String, Expression Int, Expression Int )
    -> (Expression Int -> Statement () -> Statement ())
    -> (() -> Statement r)
    -> Statement r
forLeq ( name, from, to ) loop next =
    build (\f t -> For (Just <| Decl TInt name (Just f)) (BinaryOperation (Variable name) (RelationOperation LessThanOrEquals) t) (UnaryOperation PostfixIncrement (Variable name)))
        |> withExpression from
        |> withExpression to
        |> withStatement (loop (unsafeVar name) nop)
        |> withContinuation next


forDown :
    ( String, Expression Int, Expression Int )
    -> (Expression Int -> Statement () -> Statement ())
    -> (() -> Statement r)
    -> Statement r
forDown ( name, from, to ) loop next =
    build (\f t -> For (Just <| Decl TInt name (Just f)) (BinaryOperation (Variable name) (RelationOperation GreaterThan) t) (UnaryOperation PostfixDecrement (Variable name)))
        |> withExpression from
        |> withExpression to
        |> withStatement (loop (unsafeVar name) nop)
        |> withContinuation next


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
        |> withContinuation (\_ -> k (unsafeVar n))


def : TypingFunction t -> String -> Expression t -> (Expression t -> Statement r) -> Statement r
def typeF name val k =
    let
        (TypedName t0 n0) =
            typeF name
    in
    build
        (\v0 k0 ->
            Glsl.block [ Decl t0 n0 (Just v0), k0 ]
        )
        |> withExpression val
        |> withStatement (k (unsafeVar n0))
        |> buildStatement


float : String -> Expression Float_ -> (Expression Float_ -> Statement r) -> Statement r
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
        (\v0 k0 ->
            Glsl.block [ Decl t0 n0 (Just v0), k0 ]
        )
        |> withExpression val0
        |> withStatement (k (unsafeVar n0))
        |> buildStatement


assign : Expression t -> Expression t -> Expression t
assign name e =
    unsafeMap2 (\l r -> BinaryOperation l Assign r) name e


assignOut : Expression (Out t) -> Expression t -> Expression t
assignOut name e =
    unsafeMap2 (\l r -> BinaryOperation l Assign r) name e


expr : Expression t -> (() -> Statement r) -> Statement r
expr e s =
    build ExpressionStatement
        |> withExpression e
        |> withContinuation s


nop : Statement ()
nop =
    Statement { stat = Nop, deps = SortedSet.empty }


assignAdd : Expression t -> Expression t -> (() -> Statement q) -> Statement q
assignAdd name val =
    expr <| unsafeMap2 (\l r -> BinaryOperation l ComboAdd r) name val


assignBy : Expression t -> Expression t -> (() -> Statement q) -> Statement q
assignBy name val =
    expr <| unsafeMap2 (\l r -> BinaryOperation l ComboBy r) name val



-- TYPES


voidT : TypingFunction ()
voidT n =
    TypedName TVoid n


boolT : TypingFunction Bool_
boolT n =
    TypedName TBool n


intT : TypingFunction Int_
intT n =
    TypedName TInt n


floatT : TypingFunction Float_
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


mat2T : TypingFunction Mat2
mat2T n =
    TypedName TMat2 n


mat3T : TypingFunction Mat3
mat3T n =
    TypedName TMat3 n


out : TypingFunction t -> TypingFunction t
out inner n =
    let
        (TypedName t name) =
            inner n
    in
    TypedName (TOut t) name


in_ : TypingFunction t -> TypingFunction t
in_ inner n =
    let
        (TypedName t name) =
            inner n
    in
    TypedName (TIn t) name
