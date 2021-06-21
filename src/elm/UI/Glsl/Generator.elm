module UI.Glsl.Generator exposing (call, decl, float, fun, mat3, return, toGlsl, vec2, vec3, vec4)


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


return : String -> Code
return v =
    Line <| "return " ++ v


call : String -> List Expression -> Statement
call fname args =
    fname ++ "(" ++ String.join ", " args ++ ")"


float : Type
float =
    Type "float"


vec2 : Type
vec2 =
    Type "vec2"


vec3 : Type
vec3 =
    Type "vec3"


vec4 : Type
vec4 =
    Type "vec4"


mat3 : Type
mat3 =
    Type "mat3"
