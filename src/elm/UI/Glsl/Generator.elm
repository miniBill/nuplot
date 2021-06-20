module UI.Glsl.Generator exposing (call, decl, fun, return, toGlsl)


type Code
    = Block String (List Code)
    | Line Statement


type alias Statement =
    String


type alias Expression =
    String


type alias TypedName =
    ( String, String )


fun : TypedName -> List TypedName -> List Code -> Code
fun nameAndType args body =
    Block
        (typedNameToString nameAndType
            ++ "("
            ++ String.join ", " (List.map typedNameToString args)
            ++ ")"
        )
        body


typedNameToString : TypedName -> String
typedNameToString ( type_, name ) =
    type_ ++ " " ++ name


decl : TypedName -> Expression -> Code
decl nameAndType value =
    Line (typedNameToString nameAndType ++ " = " ++ value)


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
