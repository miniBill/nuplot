module Generate exposing (main)

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import Gen.Debug
import Gen.Dict
import Gen.Glsl.Helper
import Glsl.Parser
import Glsl.Types exposing (BinaryOperation(..), Expression(..), Function, Statement(..), Type(..))
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Parser.Workaround
import Result.Extra
import Set exposing (Set)


main : Program String () ()
main =
    Generate.fromText
        (\glsl ->
            [ Elm.file [ "Glsl" ] (generate glsl)
            ]
        )


generate : String -> List Elm.Declaration
generate glsl =
    case parseFile glsl of
        Err e ->
            Gen.Debug.todo "Error parsing file"
                |> Elm.declaration "err"
                |> Elm.withDocumentation (errorToString e)
                |> List.singleton

        Ok functions ->
            let
                env : Env
                env =
                    List.foldl
                        (\( { name, returnType, args }, _ ) ->
                            functionHasType
                                name
                                (List.map Tuple.first args)
                                returnType
                        )
                        { functionsEnv = builtinFunctions
                        , variablesEnv = builtinUniforms
                        }
                        functions

                maybeDecls : Result String (List (List Elm.Declaration))
                maybeDecls =
                    Result.Extra.combineMap
                        (functionToDeclarations env)
                        functions
            in
            case maybeDecls of
                Ok decls ->
                    declarationsDictionary functions :: List.concat decls

                Err e ->
                    "Error generating file"
                        |> Gen.Debug.todo
                        |> Elm.declaration "err"
                        |> Elm.withDocumentation e
                        |> List.singleton


functionHasType : String -> List Type -> Type -> Env -> Env
functionHasType baseName argTypes returnType env =
    { env | functionsEnv = Dict.insert (fullName baseName argTypes) returnType env.functionsEnv }


declarationsDictionary : List ( Function, String ) -> Elm.Declaration
declarationsDictionary functions =
    functions
        |> List.map
            (\( { name }, _ ) ->
                Elm.tuple
                    (Elm.string name)
                    (Elm.withType Type.string <|
                        Elm.val (name ++ "Body")
                    )
            )
        |> Gen.Dict.fromList
        |> Elm.declaration "declarationsDictionary"
        |> Elm.expose


errorToString : List Parser.DeadEnd -> String
errorToString deadEnds =
    deadEnds
        |> List.map deadEndToString
        |> String.join "\n"


deadEndToString : Parser.DeadEnd -> String
deadEndToString { row, col, problem } =
    "R " ++ String.fromInt row ++ " C " ++ String.fromInt col ++ " " ++ problemToString problem


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Parser.Expecting e ->
            "Expecting " ++ e

        Parser.ExpectingInt ->
            "Expecting Int"

        Parser.ExpectingHex ->
            "Expecting Hex"

        Parser.ExpectingOctal ->
            "Expecting Octal"

        Parser.ExpectingBinary ->
            "Expecting Binary"

        Parser.ExpectingFloat ->
            "Expecting Float"

        Parser.ExpectingNumber ->
            "Expecting Number"

        Parser.ExpectingVariable ->
            "Expecting Variable"

        Parser.ExpectingSymbol _ ->
            "Expecting Symbol"

        Parser.ExpectingKeyword _ ->
            "Expecting Keyword"

        Parser.ExpectingEnd ->
            "Expecting End"

        Parser.UnexpectedChar ->
            "Unexpected char"

        Parser.Problem p ->
            p

        Parser.BadRepeat ->
            "Bad repeat"


type alias Env =
    { functionsEnv : Dict String Type
    , variablesEnv : Dict String Type
    }


functionToDeclarations : Env -> ( Function, String ) -> Result String (List Elm.Declaration)
functionToDeclarations env ( function, body ) =
    let
        argDecls : List ( String, Maybe Type.Annotation )
        argDecls =
            List.map
                (\( _, name ) ->
                    ( name
                    , Just Gen.Glsl.Helper.annotation_.expression
                    )
                )
                function.args

        envWithArgs : Env
        envWithArgs =
            List.foldl (\( type_, name ) -> variableHasType name type_) env function.args

        maybeDeps : Result String (Set String)
        maybeDeps =
            findDepsStatement envWithArgs function.body
                |> Result.mapError (\e -> e ++ " while generating " ++ function.name)
    in
    Result.map
        (\deps ->
            let
                expr : List Elm.Expression -> Elm.Expression
                expr args =
                    Gen.Glsl.Helper.call_.call
                        (Elm.string function.name)
                        (Elm.list args)
                        (deps
                            |> Set.toList
                            |> List.map Elm.string
                            |> Elm.list
                        )
                        |> Elm.withType Gen.Glsl.Helper.annotation_.expression
            in
            [ Elm.string body
                |> Elm.declaration (function.name ++ "Body")
            , Elm.function argDecls expr
                |> Elm.declaration function.name
                |> Elm.expose
            ]
        )
        maybeDeps


variableHasType : String -> Type -> Env -> Env
variableHasType var type_ env =
    { env | variablesEnv = Dict.insert var type_ env.variablesEnv }


union : List (Result String (Set comparable)) -> Result String (Set comparable)
union =
    List.foldl (Result.map2 Set.union) (Ok Set.empty)


findDepsStatement : Env -> Statement -> Result String (Set String)
findDepsStatement env statement =
    case statement of
        If cond true false ->
            union
                [ findDepsExpression env cond
                , findDepsStatement env true
                , findDepsStatement env false
                ]

        Expression e cont ->
            union
                [ findDepsExpression env e
                , findDepsStatement env cont
                ]

        For { var, from, to, step } cont ->
            let
                newEnv : Env
                newEnv =
                    variableHasType var TInt env
            in
            union
                [ findDepsExpression env from
                , findDepsExpression env to
                , findDepsStatement newEnv step
                , findDepsStatement env cont
                ]

        Return e ->
            findDepsExpression env e

        Def { type_, var, val } cont ->
            let
                newEnv : Env
                newEnv =
                    variableHasType var type_ env
            in
            union
                [ findDepsExpression env val
                , findDepsStatement newEnv cont
                ]

        Decl { type_, var } cont ->
            let
                newEnv : Env
                newEnv =
                    variableHasType var type_ env
            in
            findDepsStatement newEnv cont

        Nop ->
            Ok Set.empty


findDepsExpression : Env -> Expression -> Result String (Set String)
findDepsExpression env =
    let
        go : Expression -> Result String ( Type, Set String )
        go expr =
            case expr of
                Float _ ->
                    Ok ( TFloat, Set.empty )

                Int _ ->
                    Ok ( TInt, Set.empty )

                Bool _ ->
                    Ok ( TBool, Set.empty )

                Dot e field ->
                    Result.andThen
                        (\( et, deps ) ->
                            Result.map
                                (\t ->
                                    ( t
                                    , deps
                                    )
                                )
                                (case ( et, String.length field ) of
                                    ( TIVec2, 1 ) ->
                                        Ok TInt

                                    ( _, 1 ) ->
                                        Ok TFloat

                                    ( _, 2 ) ->
                                        Ok TVec2

                                    ( _, 3 ) ->
                                        Ok TVec3

                                    _ ->
                                        Err <| "Unknown size when accessing field " ++ field
                                )
                        )
                        (go e)

                Variable v ->
                    case Dict.get v env.variablesEnv of
                        Nothing ->
                            Err <| "Couldn't find variable " ++ v

                        Just t ->
                            Ok ( t, Set.empty )

                Constant "P32M1" ->
                    Ok ( TInt, Set.empty )

                Constant "P31" ->
                    Ok ( TInt, Set.empty )

                Constant c ->
                    Err <| "TODO: findDepsExpression _ (Constant \"" ++ c ++ "\")"

                Call f args ->
                    args
                        |> Result.Extra.combineMap go
                        |> Result.map List.unzip
                        |> Result.andThen
                            (\( argTypes, argDeps ) ->
                                let
                                    deps =
                                        if Dict.member fname builtinFunctions then
                                            List.foldl Set.union Set.empty argDeps

                                        else
                                            List.foldl Set.union (Set.singleton fname) argDeps

                                    fname =
                                        fullName f argTypes
                                in
                                case Dict.get fname env.functionsEnv of
                                    Nothing ->
                                        Err <| "Function " ++ fname ++ " not found"

                                    Just to ->
                                        Ok ( to, deps )
                            )

                Arr l r ->
                    --union [ go l, go r ]
                    Err "TODO: findDepsExpression _ (Arr _ _)"

                Ternary c t f ->
                    Result.map3
                        (\( _, cd ) ( tt, td ) ( _, fd ) ->
                            ( tt, Set.union cd td |> Set.union fd )
                        )
                        (go c)
                        (go t)
                        (go f)

                UnaryOperation _ e ->
                    go e

                BinaryOperation bop l r ->
                    Result.andThen
                        (\( ( lt, ld ), ( rt, rd ) ) ->
                            Result.map
                                (\t -> ( t, Set.union ld rd ))
                                (if lt == rt then
                                    Ok lt

                                 else
                                    case ( lt, rt ) of
                                        ( TFloat, _ ) ->
                                            Ok rt

                                        ( _, TFloat ) ->
                                            Ok lt

                                        ( TInt, _ ) ->
                                            Ok rt

                                        ( _, TInt ) ->
                                            Ok lt

                                        _ ->
                                            Err <| "Don't know what the result of " ++ typeToString lt ++ " " ++ binaryOperationToString bop ++ " " ++ typeToString rt
                                )
                        )
                        (Result.map2 Tuple.pair
                            (go l)
                            (go r)
                        )

                BooleanOperation _ es ->
                    es
                        |> Result.Extra.combineMap go
                        |> Result.map
                            (\res ->
                                let
                                    ( _, deps ) =
                                        List.unzip res
                                in
                                ( TBool, List.foldl Set.union Set.empty deps )
                            )

                RelationOperation _ l r ->
                    Result.map2
                        (\( _, ld ) ( _, rd ) ->
                            ( TBool, Set.union ld rd )
                        )
                        (go l)
                        (go r)

                PostfixIncrement e ->
                    go e

                PostfixDecrement e ->
                    go e
    in
    go >> Result.map Tuple.second


typeToString : Type -> String
typeToString t =
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


binaryOperationToString : BinaryOperation -> String
binaryOperationToString bop =
    case bop of
        Add ->
            "+"

        Subtract ->
            "-"

        By ->
            "*"

        Div ->
            "/"


fullName : String -> List Type -> String
fullName baseName argTypes =
    let
        typeToShort : Type -> String
        typeToShort t =
            case t of
                TFloat ->
                    "1"

                TInt ->
                    "i1"

                TVec2 ->
                    "2"

                TIVec2 ->
                    "i2"

                TVec3 ->
                    "3"

                TIVec3 ->
                    "i3"

                TVec4 ->
                    "4"

                TIVec4 ->
                    "i4"

                TMat3 ->
                    "m3"

                TVoid ->
                    "v"

                TBool ->
                    "b1"
    in
    String.concat (baseName :: List.map typeToShort argTypes)


builtinUniforms : Dict String Type
builtinUniforms =
    [ ( "u_whiteLines", TFloat )
    , ( "u_completelyReal", TFloat )
    , ( "u_drawAxes", TFloat )
    , ( "u_zoomCenter", TVec2 )
    , ( "u_viewportWidth", TFloat )
    , ( "u_canvasWidth", TFloat )
    , ( "u_canvasHeight", TFloat )
    , ( "u_phi", TFloat )
    , ( "u_theta", TFloat )
    ]
        |> Dict.fromList


builtinFunctions : Dict String Type
builtinFunctions =
    let
        overload : List String -> List ( String, Type ) -> List ( String, Type )
        overload names kinds =
            List.Extra.lift2
                (\name ( suffix, result ) -> ( name ++ suffix, result ))
                names
                kinds

        regular : List ( String, Type )
        regular =
            [ builtin_v_v
            , builtin_v_s
            , builtin_vv_v
            , builtin_vv_s
            , builtin_vs_v
            , builtin_sv_v
            , builtin_vvv_v
            , builtin_vss_v
            , builtin_ssv_v
            , builtin_vvs_v
            ]
                |> List.concatMap (\( names, kinds ) -> overload names kinds)

        vecs : List ( String, Type )
        vecs =
            [ ( 2, TVec2 )
            , ( 3, TVec3 )
            , ( 4, TVec4 )
            ]
                |> List.concatMap
                    (\( size, type_ ) ->
                        List.map
                            (\opt ->
                                ( "vec" ++ String.fromInt size ++ opt, type_ )
                            )
                            ([ "1", "i1" ]
                                |> List.repeat size
                                |> List.Extra.cartesianProduct
                                |> List.map String.concat
                                |> (++) [ "i1", "1", "13" ]
                            )
                    )

        ivecs : List ( String, Type )
        ivecs =
            [ ( 2, TVec2 )
            , ( 3, TVec3 )
            , ( 4, TVec4 )
            ]
                |> List.concatMap
                    (\( size, type_ ) ->
                        List.map
                            (\opt ->
                                ( "ivec" ++ String.fromInt size ++ opt, type_ )
                            )
                            ([ "i1" ]
                                |> List.repeat size
                                |> List.Extra.cartesianProduct
                                |> List.map String.concat
                                |> (++) [ "i1" ]
                            )
                    )

        others : List ( String, Type )
        others =
            [ ( "cross33", TVec3 )
            , ( "floati1", TFloat )
            , ( "int1", TInt )
            ]
    in
    Dict.fromList <| regular ++ vecs ++ ivecs ++ others


builtin_vv_s : ( List String, List ( String, Type ) )
builtin_vv_s =
    ( [ -- Geometric
        "distance"
      , "dot"
      ]
    , [ ( "11", TFloat )
      , ( "22", TFloat )
      , ( "33", TFloat )
      , ( "44", TFloat )
      ]
    )


builtin_vvs_v : ( List String, List ( String, Type ) )
builtin_vvs_v =
    ( [ --Geometric
        "refract"

      -- Other
      , "mix"
      ]
    , [ ( "111", TFloat )
      , ( "221", TVec2 )
      , ( "331", TVec3 )
      , ( "441", TVec4 )
      ]
    )


builtin_ssv_v : ( List String, List ( String, Type ) )
builtin_ssv_v =
    ( [ -- Other
        "smoothstep"
      ]
    , [ ( "111", TFloat )
      , ( "112", TVec2 )
      , ( "113", TVec3 )
      , ( "114", TVec4 )
      ]
    )


builtin_vss_v : ( List String, List ( String, Type ) )
builtin_vss_v =
    ( [ -- Other
        "clamp"
      ]
    , [ ( "111", TFloat )
      , ( "211", TVec2 )
      , ( "311", TVec3 )
      , ( "411", TVec4 )
      ]
    )


builtin_vvv_v : ( List String, List ( String, Type ) )
builtin_vvv_v =
    ( [ -- Other
        "clamp"
      , "mix"
      , "smoothstep"

      -- Geometry
      , "faceforward"
      ]
    , [ ( "111", TFloat )
      , ( "222", TVec2 )
      , ( "333", TVec3 )
      , ( "444", TVec4 )
      ]
    )


builtin_sv_v : ( List String, List ( String, Type ) )
builtin_sv_v =
    ( [ -- Other
        "step"
      ]
    , [ ( "11", TFloat )
      , ( "12", TVec2 )
      , ( "13", TVec3 )
      , ( "14", TVec4 )
      ]
    )


builtin_vs_v : ( List String, List ( String, Type ) )
builtin_vs_v =
    ( [ -- Comparison
        "min"
      , "max"

      -- Other
      , "mod"
      ]
    , [ ( "11", TFloat )
      , ( "21", TVec2 )
      , ( "31", TVec3 )
      , ( "41", TVec4 )
      ]
    )


builtin_vv_v : ( List String, List ( String, Type ) )
builtin_vv_v =
    ( [ -- Comparison
        "min"
      , "max"

      -- Complex and power
      , "pow"

      -- Trig
      , "atan"

      -- Geometry
      , "reflect"

      -- Other
      , "mod"
      , "step"
      ]
    , [ ( "11", TFloat )
      , ( "22", TVec2 )
      , ( "33", TVec3 )
      , ( "44", TVec4 )
      ]
    )


builtin_v_s : ( List String, List ( String, Type ) )
builtin_v_s =
    ( [ -- Geometric
        "length"
      ]
    , [ ( "1", TFloat )
      , ( "2", TFloat )
      , ( "3", TFloat )
      , ( "4", TFloat )
      ]
    )


builtin_v_v : ( List String, List ( String, Type ) )
builtin_v_v =
    ( [ -- Rounding
        "ceil"
      , "floor"
      , "round"
      , "fract"

      -- Complex and power
      , "sign"
      , "exp"
      , "log"
      , "abs"
      , "exp2"
      , "log2"
      , "sqrt"
      , "inversesqrt"

      -- Trig
      , "radians"
      , "degrees"
      , "sin"
      , "cos"
      , "tan"
      , "asin"
      , "acos"
      , "atan"
      , "normalize"
      ]
    , [ ( "1", TFloat )
      , ( "2", TVec2 )
      , ( "3", TVec3 )
      , ( "4", TVec4 )
      ]
    )


parseFile : String -> Result (List Parser.DeadEnd) (List ( Function, String ))
parseFile glsl =
    Parser.run
        (fileParser glsl)
        glsl


fileParser : String -> Parser (List ( Function, String ))
fileParser glsl =
    Parser.succeed (List.filterMap identity)
        |= Parser.sequence
            { start = ""
            , separator = ""
            , item =
                Parser.oneOf
                    [ Parser.succeed Nothing
                        |. Parser.Workaround.lineCommentAfter "//"
                    , Parser.succeed
                        (\begin parsed end -> Just ( parsed, String.slice begin end glsl ))
                        |= Parser.getOffset
                        |= Glsl.Parser.function
                        |= Parser.getOffset
                    ]
            , end = ""
            , trailing = Parser.Optional
            , spaces = Parser.spaces
            }
        |. Parser.end
