module UI.Glsl.Generator exposing (Code, ands, call, decl, floatT, fun, mat3T, return, ternary, toGlsl, vec2, vec2T, vec3, vec3T, vec4T)


type Code
    = Block String (List Code)
    | Line Statement


type alias Statement =
    String


type alias Expression =
    String


fun : Type -> Name -> List ( Type, Name ) -> List Code -> Code
fun (Type type_) name args body =
    Block
        (type_
            ++ " "
            ++ name
            ++ "("
            ++ String.join ", " (List.map (\( Type t, n ) -> t ++ " " ++ n) args)
            ++ ")"
        )
        body


type Type
    = Type String


type alias Name =
    String


decl : Type -> Name -> Expression -> Code
decl (Type type_) name value =
    Line (type_ ++ " " ++ name ++ " = " ++ value)


toGlsl : Code -> String
toGlsl =
    let
        go i c =
            case c of
                Block h b ->
                    indent i h
                        ++ "{\n"
                        ++ String.join "\n" (List.map (go (i + 1)) b)
                        ++ "\n}"

                Line l ->
                    indent i l ++ ";"
    in
    go 0


indent : Int -> Statement -> String
indent i line =
    String.repeat (4 * i) " " ++ line


return : Expression -> Code
return v =
    Line <| "return " ++ v


ternary : Expression -> Expression -> Expression -> Expression
ternary c t f =
    c ++ " ? " ++ t ++ " : " ++ f


ands : List Expression -> Expression
ands =
    String.join " && "


call : String -> List Expression -> Statement
call fname args =
    fname ++ "(" ++ String.join ", " args ++ ")"


floatT : Type
floatT =
    Type "float"


vec2T : Type
vec2T =
    Type "vec2"


vec2 : Expression -> Expression -> Expression
vec2 x y =
    call "vec2" [ x, y ]


vec3T : Type
vec3T =
    Type "vec3"


vec3 : Expression -> Expression -> Expression -> Expression
vec3 x y z =
    call "vec3" [ x, y, z ]


vec4T : Type
vec4T =
    Type "vec4"


mat3T : Type
mat3T =
    Type "mat3"
