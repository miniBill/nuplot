module UI.Glsl.Generator exposing (Expression, File, FunDecl, Mat3, Name, Statement(..), Type, TypedName, TypingFunction, Vec2, Vec3, Vec4, abs_, add, ands, arr, assign, by, byF, call0, call1, call2, call4, ceil_, decl, def, div, dot2, dot3, dotted2, dotted3, dotted4, eq, exp, expressionToGlsl, false, fileToGlsl, float, floatT, floatToGlsl, fun0, fun1, fun2, fun3, funDeclToGlsl, geq, gl_FragColor, gl_FragCoord, gt, if_, int, leq, log, lt, mat3T, max_, min_, mod, negate_, normalize, one, pow, radians_, return, sign, statementToGlsl, subtract, ternary, true, uniform, unknown, unknownFunDecl, unknownName, unknownTypedName, unsafeCall, vec2, vec2T, vec2Zero, vec3, vec3T, vec3Zero, vec4, vec4T, vec4Zero, vec4_3_1, voidT, zero)

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
    | Expression (Expression Never)
    | Decl String Name (Maybe (Expression Never))


type Expression t
    = Bool Bool
    | Int Int
    | Float Float
    | Variable String
    | And (Expression Bool) (Expression Bool)
    | Or (Expression Bool) (Expression Bool)
    | Comparison RelationOperation (Expression Never) (Expression Never)
    | Ternary (Expression Bool) (Expression t) (Expression t)
    | Add (Expression t) (Expression t)
    | Subtract (Expression t) (Expression t)
    | By (Expression t) (Expression t)
    | Div (Expression t) (Expression t)
    | Call String (List (Expression Never))
    | Negate (Expression t)
    | Unknown String
    | Dot (Expression Never) String
    | Array (Expression Never) (Expression Int)
    | Assign (Expression t) (Expression t)


type alias TypingFunction t =
    String -> TypedName t


type alias TypedName t =
    ( Type t, Name )


type Type t
    = Type String


type Name
    = Name String



-- Internal-only functions


{-| UNSAFE
-}
typecastExpression : Expression f -> Expression t
typecastExpression e =
    case e of
        Unknown u ->
            Unknown u

        Bool b ->
            Bool b

        Float f ->
            Float f

        Int i ->
            Int i

        Variable v ->
            Variable v

        And l r ->
            And l r

        Or l r ->
            Or l r

        Comparison rel l r ->
            Comparison rel l r

        Ternary c t f ->
            Ternary c (typecastExpression t) (typecastExpression f)

        Negate l ->
            Negate <| typecastExpression l

        Add l r ->
            Add (typecastExpression l) (typecastExpression r)

        Subtract l r ->
            Subtract (typecastExpression l) (typecastExpression r)

        By l r ->
            By (typecastExpression l) (typecastExpression r)

        Div l r ->
            Div (typecastExpression l) (typecastExpression r)

        Dot l r ->
            Dot l r

        Call n args ->
            Call n args

        Array l r ->
            Array l r

        Assign l r ->
            Assign (typecastExpression l) (typecastExpression r)



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

                Expression e ->
                    indent i <| expressionToGlsl e ++ ";"

                Decl t (Name n) (Just e) ->
                    indent i <| t ++ " " ++ n ++ " = " ++ expressionToGlsl e ++ ";"

                Decl t (Name n) Nothing ->
                    indent i <| t ++ " " ++ n ++ ";"
    in
    go 1


expressionToGlsl : Expression t -> String
expressionToGlsl =
    let
        -- The numbers are precedence numbers from the GLSL spec
        go : Bool -> Expression a -> String
        go rec e =
            case e of
                Unknown u ->
                    u

                _ ->
                    go17 rec e

        go17 : Bool -> Expression b -> String
        go17 rec e =
            go16 rec e

        go16 : Bool -> Expression c -> String
        go16 rec e =
            case e of
                Assign l r ->
                    go15 False l ++ " = " ++ go15 False r

                _ ->
                    go15 rec e

        go15 : Bool -> Expression d -> String
        go15 rec e =
            case e of
                Ternary c t f ->
                    go14 False c ++ " ? " ++ go14 False t ++ " : " ++ go15 False f

                _ ->
                    go14 rec e

        go14 : Bool -> Expression e -> String
        go14 rec e =
            case e of
                Or l r ->
                    go14 False (typecastExpression l) ++ " || " ++ go13 False r

                _ ->
                    go13 rec e

        go13 : Bool -> Expression f -> String
        go13 rec e =
            go12 rec e

        go12 : Bool -> Expression g -> String
        go12 rec e =
            case e of
                And l r ->
                    go12 False (typecastExpression l) ++ " && " ++ go11 False r

                _ ->
                    go11 rec e

        go11 : Bool -> Expression h -> String
        go11 rec e =
            go10 rec e

        go10 : Bool -> Expression i -> String
        go10 rec e =
            go9 rec e

        go9 : Bool -> Expression j -> String
        go9 rec e =
            go8 rec e

        go8 : Bool -> Expression k -> String
        go8 rec e =
            case e of
                Comparison Equals l r ->
                    go7 False l ++ " == " ++ go7 False r

                _ ->
                    go7 rec e

        go7 : Bool -> Expression l -> String
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

        go6 : Bool -> Expression m -> String
        go6 rec e =
            go5 rec e

        go5 : Bool -> Expression n -> String
        go5 rec e =
            case e of
                Add l r ->
                    go5 False l ++ " + " ++ go4 False r

                Subtract l r ->
                    go5 False l ++ " - " ++ go4 False r

                _ ->
                    go4 rec e

        go4 : Bool -> Expression o -> String
        go4 rec e =
            case e of
                By l r ->
                    go4 False l ++ " * " ++ go3 False r

                Div l r ->
                    go4 False l ++ " / " ++ go3 False r

                _ ->
                    go3 rec e

        go3 : Bool -> Expression p -> String
        go3 rec e =
            case e of
                Negate c ->
                    "-" ++ go2 False c

                _ ->
                    go2 rec e

        go2 : Bool -> Expression q -> String
        go2 rec e =
            case e of
                Call name args ->
                    name ++ "(" ++ String.join "," (List.map (go False) args) ++ ")"

                Dot l r ->
                    go2 False (typecastExpression l) ++ "." ++ r

                Array l r ->
                    go2 False (typecastExpression l) ++ "[" ++ go False r ++ "]"

                _ ->
                    go1 rec e

        go1 : Bool -> Expression r -> String
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
                        "!!!Error in toString for Glsl!!!"

                    else
                        "(" ++ go True e ++ ")"
    in
    go False


indent : Int -> String -> String
indent i line =
    String.repeat (4 * i) " " ++ line



--EXPRESSIONS


ternary : Expression Bool -> Expression t -> Expression t -> Expression t
ternary =
    Ternary


ands : List (Expression Bool) -> Expression Bool
ands es =
    case es of
        [] ->
            true

        h :: t ->
            List.foldl (\e a -> And a e) h t


true : Expression Bool
true =
    Bool True


false : Expression Bool
false =
    Bool False


add : Expression t -> Expression t -> Expression t
add =
    Add


subtract : Expression t -> Expression t -> Expression t
subtract =
    Subtract


negate_ : Expression t -> Expression t
negate_ =
    Negate


by : Expression t -> Expression t -> Expression t
by =
    By


byF : Expression Float -> Expression t -> Expression t
byF l =
    By (typecastExpression l)


div : Expression t -> Expression t -> Expression t
div =
    Div


lt : Expression t -> Expression t -> Expression Bool
lt l r =
    Comparison LessThan (typecastExpression l) (typecastExpression r)


leq : Expression t -> Expression t -> Expression Bool
leq l r =
    Comparison LessThanOrEquals (typecastExpression l) (typecastExpression r)


eq : Expression t -> Expression t -> Expression Bool
eq l r =
    Comparison Equals (typecastExpression l) (typecastExpression r)


geq : Expression t -> Expression t -> Expression Bool
geq l r =
    Comparison GreaterThanOrEquals (typecastExpression l) (typecastExpression r)


gt : Expression t -> Expression t -> Expression Bool
gt l r =
    Comparison GreaterThan (typecastExpression l) (typecastExpression r)


unsafeCall : String -> List (Expression t) -> Expression r
unsafeCall name =
    Call name << List.map typecastExpression


call0 : TypedName r -> Expression r
call0 ( _, Name fname ) =
    Call fname []


call1 : TypedName (a -> r) -> Expression a -> Expression r
call1 ( _, Name fname ) arg0 =
    Call fname [ typecastExpression arg0 ]


call2 : TypedName (a -> b -> r) -> Expression a -> Expression b -> Expression r
call2 ( _, Name fname ) arg0 arg1 =
    Call fname [ typecastExpression arg0, typecastExpression arg1 ]


call3 : TypedName (a -> b -> c -> r) -> Expression a -> Expression b -> Expression c -> Expression r
call3 ( _, Name fname ) arg0 arg1 arg2 =
    Call fname [ typecastExpression arg0, typecastExpression arg1, typecastExpression arg2 ]


call4 : TypedName (a -> b -> c -> d -> r) -> Expression a -> Expression b -> Expression c -> Expression d -> Expression r
call4 ( _, Name fname ) arg0 arg1 arg2 arg3 =
    Call fname [ typecastExpression arg0, typecastExpression arg1, typecastExpression arg2, typecastExpression arg3 ]


zero : Expression Float
zero =
    Float 0


one : Expression Float
one =
    Float 1


float : Float -> Expression Float
float =
    Float


int : Int -> Expression Int
int =
    Int


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


abs_ : Expression t -> Expression t
abs_ =
    call1 ( Type "", Name "abs" )


exp : Expression t -> Expression t
exp =
    call1 ( Type "", Name "exp" )


log : Expression t -> Expression t
log =
    call1 ( Type "", Name "log" )


mod : Expression t -> Expression t -> Expression t
mod =
    call2 ( Type "", Name "mod" )


dotted2 :
    Expression Vec2
    ->
        { x : Expression Float
        , y : Expression Float
        }
dotted2 e =
    let
        tc =
            typecastExpression e
    in
    { x = Dot tc "x"
    , y = Dot tc "y"
    }


dotted3 :
    Expression Vec3
    ->
        { x : Expression Float
        , y : Expression Float
        , z : Expression Float
        }
dotted3 e =
    let
        tc =
            typecastExpression e
    in
    { x = Dot tc "x"
    , y = Dot tc "y"
    , z = Dot tc "z"
    }


dotted4 :
    Expression Vec4
    ->
        { x : Expression Float
        , y : Expression Float
        , z : Expression Float
        , xy : Expression Vec2
        , yzw : Expression Vec3
        }
dotted4 e =
    let
        tc =
            typecastExpression e
    in
    { x = Dot tc "x"
    , y = Dot tc "y"
    , z = Dot tc "z"
    , xy = Dot tc "xy"
    , yzw = Dot tc "yzw"
    }


arr : Expression Mat3 -> Expression Int -> Expression Vec3
arr l r =
    Array (typecastExpression l) r


dot2 : Expression Vec2 -> Expression Vec2 -> Expression Float
dot2 =
    call2 ( Type "", Name "dot" )


dot3 : Expression Vec3 -> Expression Vec3 -> Expression Float
dot3 =
    call2 ( Type "", Name "dot" )


vec2 : Expression Float -> Expression Float -> Expression Vec2
vec2 =
    call2 ( Type "", Name "vec2" )


vec2Zero : Expression Vec2
vec2Zero =
    call1 ( Type "", Name "vec2" ) (int 0)


vec3 : Expression Float -> Expression Float -> Expression Float -> Expression Vec3
vec3 =
    call3 ( Type "", Name "vec3" )


vec3Zero : Expression Vec3
vec3Zero =
    call1 ( Type "", Name "vec3" ) (int 0)


vec4 : Expression Float -> Expression Float -> Expression Float -> Expression Float -> Expression Vec4
vec4 =
    call4 ( Type "", Name "vec4" )


vec4Zero : Expression Vec4
vec4Zero =
    call1 ( Type "", Name "vec4" ) (int 0)


vec4_3_1 : Expression Vec3 -> Expression Float -> Expression Vec4
vec4_3_1 =
    call2 ( Type "", Name "vec4" )


min_ : Expression t -> Expression t -> Expression t
min_ =
    call2 ( Type "", Name "min" )


max_ : Expression t -> Expression t -> Expression t
max_ =
    call2 ( Type "", Name "max" )


pow : Expression Float -> Expression Float -> Expression Float
pow =
    call2 ( Type "", Name "pow" )


ceil_ : Expression Float -> Expression Float
ceil_ =
    call1 ( Type "", Name "ceil" )


sign : Expression t -> Expression t
sign =
    call1 ( Type "", Name "sign" )


radians_ : Expression Float -> Expression Float
radians_ =
    call1 ( Type "", Name "radians" )


normalize : Expression t -> Expression t
normalize =
    call1 ( Type "", Name "normalize" )


unknown : String -> Expression t
unknown =
    -- let
    --     _ =
    --         Debug.todo
    -- in
    Unknown


gl_FragColor : Expression Vec3
gl_FragColor =
    Variable "gl_FragColor"


gl_FragCoord : Expression Vec4
gl_FragCoord =
    Variable "gl_FragCoord"


uniform : TypingFunction t -> String -> Expression t
uniform _ name =
    Variable name


unknownName : String -> Name
unknownName =
    -- let
    --     _ =
    --         Debug.todo
    -- in
    Name


unknownTypedName : String -> TypedName t
unknownTypedName n =
    -- let
    --     _ =
    --         Debug.todo
    -- in
    ( Type "", Name n )



-- STATEMENTS


fun : TypingFunction t -> String -> List ( String, String ) -> List (Statement t) -> FunDecl
fun typeF name args body =
    let
        ( Type rt, _ ) =
            typeF name

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


argToString : ( Type t, Name ) -> ( String, String )
argToString ( Type t, Name n ) =
    ( t, n )


toVar : TypedName t -> Expression t
toVar ( _, Name n ) =
    Variable n


fun0 : TypingFunction r -> String -> List (Statement r) -> FunDecl
fun0 typeF name body =
    fun typeF name [] <|
        body


fun1 : TypingFunction r -> String -> TypedName a -> (Expression a -> List (Statement r)) -> ( FunDecl, Expression a -> Expression r )
fun1 typeF name arg0 body =
    ( fun typeF name [ argToString arg0 ] <|
        body (toVar arg0)
    , call1 ( Type "", Name name )
    )


fun2 : TypingFunction r -> String -> TypedName a -> TypedName b -> (Expression a -> Expression b -> List (Statement r)) -> ( FunDecl, Expression a -> Expression b -> Expression r )
fun2 typeF name arg0 arg1 body =
    ( fun typeF name [ argToString arg0, argToString arg1 ] <|
        body (toVar arg0) (toVar arg1)
    , call2 ( Type "", Name name )
    )


fun3 : TypingFunction r -> String -> TypedName a -> TypedName b -> TypedName c -> (Expression a -> Expression b -> Expression c -> List (Statement r)) -> ( FunDecl, Expression a -> Expression b -> Expression c -> Expression r )
fun3 typeF name arg0 arg1 arg2 body =
    ( fun typeF name [ argToString arg0, argToString arg1, argToString arg2 ] <|
        body (toVar arg0) (toVar arg1) (toVar arg2)
    , call3 ( Type "", Name name )
    )


if_ : Expression Bool -> Statement r -> Statement r
if_ cond ifTrue =
    Block ("if(" ++ expressionToGlsl cond ++ ")") [ ifTrue ]


return : Expression r -> Statement r
return =
    Return


decl : TypingFunction t -> String -> (Expression t -> List (Statement a)) -> List (Statement a)
decl typeF name k =
    let
        ( Type t, _ ) =
            typeF name

        go =
            Decl t (Name name) Nothing
    in
    go :: k (Variable name)


def : TypingFunction t -> String -> Expression t -> (Expression t -> List (Statement a)) -> List (Statement a)
def typeF name value k =
    let
        ( Type t, _ ) =
            typeF name

        go =
            Decl t (Name name) (Just <| typecastExpression value)
    in
    go :: k (Variable name)


assign : Expression t -> Expression t -> Statement a
assign name value =
    Expression <| Assign (typecastExpression name) (typecastExpression value)


unknownFunDecl : { name : String, type_ : String, body : String } -> FunDecl
unknownFunDecl =
    let
        _ =
            Debug.todo
    in
    FunDecl



-- TYPES


buildType : String -> String -> ( Type t, Name )
buildType t n =
    ( Type t, Name n )


voidT : TypingFunction ()
voidT =
    buildType "void"


floatT : TypingFunction Float
floatT =
    buildType "float"


type Vec2
    = Vec2 Vec2


vec2T : TypingFunction Vec2
vec2T =
    buildType "vec2"


type Vec3
    = Vec3 Vec3


vec3T : TypingFunction Vec3
vec3T =
    buildType "vec3"


type Vec4
    = Vec4 Vec4


vec4T : TypingFunction Vec4
vec4T =
    buildType "vec4"


type Mat3
    = Mat3 Mat3


mat3T : TypingFunction Mat3
mat3T =
    buildType "mat3"
