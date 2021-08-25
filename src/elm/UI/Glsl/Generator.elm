module UI.Glsl.Generator exposing (Expression, Expression1, Expression2, Expression3, Expression4, ExpressionX, File, FunDecl, Mat3, Name, Statement(..), TypedName, TypingFunction, Vec2, Vec3, Vec4, abs2, abs4, abs_, add, add2, add4, ands, arr, assign, by, by2, by3, byF, call0, call1, call2, call3, call4, ceil_, cos_, cosh, decl, def, div, div2, dot, dotted1, dotted2, dotted3, dotted4, eq, exp, expressionToGlsl, false, fileToGlsl, float, floatT, floatToGlsl, fun0, fun1, fun2, fun3, funDeclToGlsl, geq, gl_FragColor, gl_FragCoord, gt, hl2rgb, if_, int, leq, log, lt, mat3T, max3, max4, max_, min_, mod, negate2, negate_, normalize, one, pow, radians_, return, sign, sin_, sinh, statementToGlsl, subtract, subtract2, subtract4, ternary, ternary2, ternary3, true, uniform, unknown, unknownFun0, unknownFun1, unknownFun2, unknownFunDecl, unsafeCall, vec2, vec2T, vec2Zero, vec3, vec3T, vec3Zero, vec4, vec4T, vec4Zero, vec4_3_1, voidT, zero)

import Expression exposing (RelationOperation(..))
import Set


type alias File =
    List FunDecl


type FunDecl
    = FunDecl { name : String, type_ : String, body : String }


type Statement r
    = Block String (List (Statement r))
    | Line String
    | Return (Expression r)
    | ExpressionStatement Expr
    | Decl String Name (Maybe Expr)


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
    | Unknown String
    | Dot Expr String
    | Array Expr Expr
    | Assign Expr Expr


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
    , yzw : Expression3
    }


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
statementToGlsl =
    let
        go i c =
            case c of
                Block h b ->
                    String.join "\n" <|
                        (indent i h ++ " {")
                            :: List.map (go (i + 1)) b
                            ++ [ indent i "}" ]

                Line l ->
                    indent i l ++ ";"

                Return e ->
                    indent i <| "return " ++ expressionToGlsl e ++ ";"

                ExpressionStatement e ->
                    indent i <| expressionToGlsl (Expression e) ++ ";"

                Decl t (Name n) (Just e) ->
                    indent i <| t ++ " " ++ n ++ " = " ++ expressionToGlsl (Expression e) ++ ";"

                Decl t (Name n) Nothing ->
                    indent i <| t ++ " " ++ n ++ ";"
    in
    go 1


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

                _ ->
                    go2 rec e

        go2 : Bool -> Expr -> String
        go2 rec e =
            case e of
                Call name args ->
                    name ++ "(" ++ String.join "," (List.map (go False) args) ++ ")"

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


indent : Int -> String -> String
indent i line =
    String.repeat (4 * i) " " ++ line



--EXPRESSIONS


ternary : ExpressionX a Bool -> ExpressionX b t -> ExpressionX c t -> Expression1 t
ternary =
    expr3 Ternary


ternary2 : ExpressionX xa Bool -> ExpressionX xb Vec2 -> ExpressionX xc Vec2 -> Expression2
ternary2 =
    expr32 Ternary


ternary3 : ExpressionX xa Bool -> ExpressionX xb Vec3 -> ExpressionX xc Vec3 -> Expression3
ternary3 =
    expr33 Ternary


ands : List (Expression1 Bool) -> Expression1 Bool
ands es =
    case es of
        [] ->
            true

        h :: t ->
            List.foldl (\e a -> expr2 And a e) h t


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


expr3 : (Expr -> Expr -> Expr -> Expr) -> ExpressionX a l -> ExpressionX b m -> ExpressionX c r -> Expression1 t
expr3 f l m r =
    dotted1Internal (f (unwrapExpression l) (unwrapExpression m) (unwrapExpression r))


expr32 : (Expr -> Expr -> Expr -> Expr) -> ExpressionX xa a -> ExpressionX xb b -> ExpressionX xc c -> Expression2
expr32 f l m r =
    dotted2Internal (f (unwrapExpression l) (unwrapExpression m) (unwrapExpression r))


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


add : ExpressionX a t -> ExpressionX a t -> Expression1 t
add =
    expr2 Add


add2 : ExpressionX a Vec2 -> ExpressionX b Vec2 -> Expression2
add2 =
    expr22 Add


add4 : ExpressionX a Vec4 -> ExpressionX b Vec4 -> Expression4
add4 =
    expr24 Add


subtract : ExpressionX a t -> ExpressionX a t -> Expression1 t
subtract =
    expr2 Subtract


subtract2 : ExpressionX a Vec2 -> ExpressionX b Vec2 -> Expression2
subtract2 =
    expr22 Subtract


subtract4 : ExpressionX a Vec4 -> ExpressionX b Vec4 -> Expression4
subtract4 =
    expr24 Subtract


negate_ : ExpressionX a t -> Expression1 t
negate_ =
    expr1 Negate


negate2 : ExpressionX a Vec2 -> Expression2
negate2 e =
    dotted2Internal (Negate <| unwrapExpression e)


by : ExpressionX a t -> ExpressionX a t -> Expression1 t
by =
    expr2 By


by2 : ExpressionX a Vec2 -> ExpressionX a Vec2 -> Expression2
by2 =
    expr22 By


by3 : ExpressionX a Vec3 -> ExpressionX a Vec3 -> Expression3
by3 =
    expr23 By


byF : ExpressionX a Float -> ExpressionX b t -> Expression1 t
byF =
    expr2 By


div : ExpressionX a t -> ExpressionX a t -> Expression1 t
div =
    expr2 Div


div2 : ExpressionX a Vec2 -> ExpressionX a Vec2 -> Expression2
div2 =
    expr22 Div


lt : ExpressionX a t -> ExpressionX a t -> Expression1 Bool
lt =
    expr2 (Comparison LessThan)


leq : ExpressionX a t -> ExpressionX a t -> Expression1 Bool
leq =
    expr2 (Comparison LessThanOrEquals)


eq : ExpressionX a t -> ExpressionX a t -> Expression1 Bool
eq =
    expr2 (Comparison Equals)


geq : ExpressionX a t -> ExpressionX a t -> Expression1 Bool
geq =
    expr2 (Comparison GreaterThanOrEquals)


gt : ExpressionX a t -> ExpressionX a t -> Expression1 Bool
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


exp : ExpressionX a t -> Expression1 t
exp =
    dotted1 << call1Internal "exp"


sin_ : ExpressionX a t -> Expression1 t
sin_ =
    dotted1 << call1Internal "sin"


cos_ : ExpressionX a t -> Expression1 t
cos_ =
    dotted1 << call1Internal "cos"


sinh : ExpressionX a t -> Expression1 t
sinh =
    dotted1 << call1Internal "sinh"


cosh : ExpressionX a t -> Expression1 t
cosh =
    dotted1 << call1Internal "cosh"


log : ExpressionX a t -> Expression1 t
log =
    dotted1 << call1Internal "log"


mod : ExpressionX a t -> ExpressionX a t -> Expression1 t
mod l r =
    dotted1 (call2Internal "mod" l r)


min_ : ExpressionX a t -> ExpressionX a t -> Expression1 t
min_ l r =
    dotted1 (call2Internal "min" l r)


max_ : ExpressionX a t -> ExpressionX a t -> Expression1 t
max_ l r =
    dotted1 (call2Internal "max" l r)


max3 : ExpressionX a Vec3 -> ExpressionX a Vec3 -> Expression3
max3 l r =
    dotted3 (call2Internal "max" l r)


max4 : ExpressionX a Vec4 -> ExpressionX a Vec4 -> Expression4
max4 l r =
    dotted4 (call2Internal "max" l r)


hl2rgb : Expression1 Float -> Expression1 Float -> Expression3
hl2rgb h l =
    dotted3 (call2Internal "hl2rgb" h l)


pow : Expression1 Float -> Expression1 Float -> Expression1 Float
pow l r =
    dotted1 (call2Internal "pow" l r)


ceil_ : ExpressionX a Float -> Expression1 Float
ceil_ =
    dotted1 << call1Internal "ceil"


sign : ExpressionX a t -> Expression1 t
sign =
    dotted1 << call1Internal "sign"


radians_ : ExpressionX a Float -> Expression1 Float
radians_ =
    dotted1 << call1Internal "radians"


normalize : ExpressionX a t -> Expression1 t
normalize =
    dotted1 << call1Internal "normalize"


arr : ExpressionX a Mat3 -> ExpressionX b Int -> Expression1 Vec3
arr =
    expr2 Array


dot : ExpressionX a t -> ExpressionX a t -> Expression1 Float
dot l r =
    dotted1 (call2Internal "dot" l r)


vec2 : Expression1 Float -> Expression1 Float -> Expression2
vec2 x y =
    { base = call2Internal "vec2" x y
    , x = x
    , y = y
    }


vec3 :
    Expression1 Float
    -> Expression1 Float
    -> Expression1 Float
    -> Expression3
vec3 x y z =
    { base = call3Internal "vec3" x y z
    , x = x
    , y = y
    , z = z
    }


vec4 :
    Expression1 Float
    -> Expression1 Float
    -> Expression1 Float
    -> Expression1 Float
    -> Expression4
vec4 x y z w =
    { base = call4Internal "vec4" x y z w
    , x = x
    , y = y
    , z = z
    , w = w
    , xy = dotted2 <| call2Internal "vec2" x y
    , yzw = dotted3 <| call3Internal "vec3" y z w
    }


vec4_3_1 : ExpressionX a Vec3 -> ExpressionX b Float -> Expression4
vec4_3_1 xyz w =
    call2Internal "vec4" xyz w
        |> dotted4



---------------
-- CONSTANTS --
---------------


vec2Zero : Expression2
vec2Zero =
    { base = call2Internal "vec2" (int 0) (int 0)
    , x = zero
    , y = zero
    }


vec3Zero : Expression3
vec3Zero =
    { base = call3Internal "vec3" (int 0) (int 0) (int 0)
    , x = zero
    , y = zero
    , z = zero
    }


vec4Zero : Expression4
vec4Zero =
    { base = call4Internal "vec4" (int 0) (int 0) (int 0) (int 0)
    , x = zero
    , y = zero
    , z = zero
    , w = zero
    , xy = dotted2 <| call2Internal "vec2" zero zero
    , yzw = dotted3 <| call3Internal "vec3" zero zero zero
    }


gl_FragColor : Expression4
gl_FragColor =
    dotted4 (Expression (Variable "gl_FragColor"))


gl_FragCoord : Expression4
gl_FragCoord =
    dotted4 (Expression (Variable "gl_FragCoord"))



----------------
-- CALL UTILS --
----------------


unsafeCall : String -> List (Expression t) -> Expression r
unsafeCall name =
    Expression << Call name << List.map (\(Expression e) -> e)


call0 : TypedName t r -> r
call0 (TypedName _ _ expr) =
    expr


call1 : TypedName t (a -> r) -> a -> r
call1 (TypedName _ _ expr) =
    expr


call2 : TypedName t (a -> b -> r) -> a -> b -> r
call2 (TypedName _ _ expr) =
    expr


call3 : TypedName t (a -> b -> c -> r) -> a -> b -> c -> r
call3 (TypedName _ _ expr) =
    expr


call4 : TypedName t (a -> b -> c -> d -> r) -> a -> b -> c -> d -> r
call4 (TypedName _ _ expr) =
    expr


zero : Expression1 Float
zero =
    float 0


one : Expression1 Float
one =
    float 1


float : Float -> Expression1 Float
float f =
    dotted1Internal (Float f)


int : Int -> Expression1 Int
int i =
    dotted1Internal (Int i)


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
    , xy = dotted2 (Expression (Dot e "xy"))
    , yzw = dotted3 (Expression (Dot e "yzw"))
    }


uniform : TypingFunction t c -> String -> c
uniform typeF name =
    let
        ( TypedName _ _ expr, _ ) =
            typeF name
    in
    expr


unknown : String -> Expression t
unknown =
    let
        _ =
            Debug.todo
    in
    Expression << Unknown


unknownFun0 : String -> TypedName t (Expression1 t)
unknownFun0 n =
    let
        _ =
            Debug.todo
    in
    TypedName (Type "") (Name n) (dotted1 <| call0Internal n)


unknownFun1 : String -> TypedName (l -> t) (ExpressionX a l -> Expression1 t)
unknownFun1 n =
    let
        _ =
            Debug.todo
    in
    TypedName (Type "") (Name n) (\l -> dotted1 <| call1Internal n l)


unknownFun2 : String -> TypedName (l -> r -> t) (ExpressionX a l -> ExpressionX b r -> Expression1 t)
unknownFun2 n =
    let
        _ =
            Debug.todo
    in
    TypedName (Type "") (Name n) (\l r -> dotted1 (call2Internal n l r))



-- STATEMENTS


funInternal : TypedName t c -> List ( String, String ) -> List (Statement t) -> FunDecl
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
                (rt ++ " " ++ name ++ "(" ++ argsList ++ ") {")
                    :: List.map statementToGlsl body
                    ++ [ "}" ]
        }


argToString : TypedName t c -> ( String, String )
argToString (TypedName (Type t) (Name n) _) =
    ( t, n )


toVar : TypedName t c -> c
toVar (TypedName _ _ expr) =
    expr


fun0 :
    TypingFunction t c
    -> String
    -> List (Statement t)
    -> ( FunDecl, c )
fun0 typeF name body =
    let
        ( typed, dotter ) =
            typeF name
    in
    ( funInternal typed [] <|
        body
    , dotter <| call0Internal name
    )


fun1 :
    TypingFunction tr r
    -> String
    -> ( TypedName ta a, Expression ta -> a )
    -> (a -> List (Statement tr))
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
        body (toVar arg0)
    , dotter << call1Internal name
    )


fun2 :
    TypingFunction tr r
    -> String
    -> ( TypedName ta a, Expression ta -> a )
    -> ( TypedName tb b, Expression tb -> b )
    -> (a -> b -> List (Statement tr))
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
        body (toVar arg0) (toVar arg1)
    , \l r -> dotter (call2Internal name l r)
    )


fun3 :
    TypingFunction tr r
    -> String
    -> ( TypedName ta a, Expression ta -> a )
    -> ( TypedName tb b, Expression tb -> b )
    -> ( TypedName tc c, Expression tc -> c )
    -> (a -> b -> c -> List (Statement tr))
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
        body (toVar arg0) (toVar arg1) (toVar arg2)
    , \l m r -> dotter (call3Internal name l m r)
    )


if_ : Expression1 Bool -> Statement r -> Statement r
if_ cond ifTrue =
    Block ("if(" ++ expressionToGlsl cond.base ++ ")") [ ifTrue ]


return : ExpressionX a r -> Statement r
return { base } =
    Return base


decl : TypingFunction tv v -> String -> (v -> List (Statement tr)) -> List (Statement tr)
decl typeF name k =
    let
        ( TypedName (Type t) n e, _ ) =
            typeF name

        go =
            Decl t n Nothing
    in
    go :: k e


def : TypingFunction tv v -> String -> ExpressionX xv tv -> (v -> List (Statement tr)) -> List (Statement tr)
def typeF name value k =
    let
        ( TypedName (Type t) n e, _ ) =
            typeF name

        go =
            Decl t n (Just <| unwrapExpression value)
    in
    go :: k e


assign : ExpressionX a t -> ExpressionX a t -> Statement q
assign name e =
    ExpressionStatement (Assign (unwrapExpression name) (unwrapExpression e))


unknownFunDecl : { name : String, type_ : String, body : String } -> FunDecl
unknownFunDecl =
    let
        _ =
            Debug.todo
    in
    FunDecl



-- TYPES


type Vec2
    = Vec2 Vec2


type Vec3
    = Vec3 Vec3


type Vec4
    = Vec4 Vec4


type Mat3
    = Mat3 Mat3


voidT : TypingFunction (Never -> a) (Never -> a)
voidT n =
    ( TypedName (Type "void") (Name n) never, always never )


floatT : TypingFunction Float (Expression1 Float)
floatT n =
    ( TypedName (Type "float") (Name n) (dotted1Internal (Variable n)), dotted1 )


vec2T : TypingFunction Vec2 Expression2
vec2T n =
    ( TypedName (Type "vec2") (Name n) (dotted2Internal (Variable n)), dotted2 )


vec3T : TypingFunction Vec3 Expression3
vec3T n =
    ( TypedName (Type "vec3") (Name n) (dotted3Internal (Variable n)), dotted3 )


vec4T : TypingFunction Vec4 Expression4
vec4T n =
    ( TypedName (Type "vec4") (Name n) (dotted4Internal (Variable n)), dotted4 )


mat3T : TypingFunction Mat3 (Expression1 Mat3)
mat3T n =
    ( TypedName (Type "mat3") (Name n) (dotted1Internal (Variable n)), dotted1 )
