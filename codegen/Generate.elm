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
import Parser
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
                        (\{ name, returnType, args } ->
                            functionHasType
                                name
                                (List.map Tuple.first args)
                                returnType
                        )
                        { functionsEnv = builtinFunctions |> Dict.map (\_ -> Tuple.second)
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
                    declarationsDictionary functions :: List.concat decls ++ builtinDecls

                Err e ->
                    "Error generating file"
                        |> Gen.Debug.todo
                        |> Elm.declaration "err"
                        |> Elm.withDocumentation e
                        |> List.singleton


functionHasType : String -> List Type -> Type -> Env -> Env
functionHasType baseName argTypes returnType env =
    { env | functionsEnv = Dict.insert (fullName baseName argTypes) returnType env.functionsEnv }


declarationsDictionary : List Function -> Elm.Declaration
declarationsDictionary functions =
    functions
        |> List.map
            (\{ name } ->
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


functionToDeclarations : Env -> Function -> Result String (List Elm.Declaration)
functionToDeclarations env function =
    let
        envWithArgs : Env
        envWithArgs =
            List.foldl (\( type_, name ) -> variableHasType name type_) env function.args

        maybeDeps : Result String (Set String)
        maybeDeps =
            findDepsStatement envWithArgs function.stat
                |> Result.mapError (\e -> e ++ " while generating " ++ function.name)
    in
    Result.map
        (\deps ->
            [ Elm.string function.body
                |> Elm.declaration (function.name ++ "Body")
            , wrapFunction function.name function.args function.returnType deps
            ]
        )
        maybeDeps


wrapFunction : String -> List ( Type, String ) -> Type -> Set String -> Elm.Declaration
wrapFunction name args returnType deps =
    let
        argDecls : List ( String, Maybe Type.Annotation )
        argDecls =
            List.map
                (\( type_, argName ) ->
                    ( argName
                    , Just <| Gen.Glsl.Helper.annotation_.expression (typeToAnnotation type_)
                    )
                )
                args

        innerCall argValues _ =
            case argValues of
                [] ->
                    Gen.Glsl.Helper.unsafeCall0 name

                [ arg0 ] ->
                    Gen.Glsl.Helper.unsafeCall1 name arg0

                [ arg0, arg1 ] ->
                    Gen.Glsl.Helper.unsafeCall2 name arg0 arg1

                [ arg0, arg1, arg2 ] ->
                    Gen.Glsl.Helper.unsafeCall3 name arg0 arg1 arg2

                [ arg0, arg1, arg2, arg3 ] ->
                    Gen.Glsl.Helper.unsafeCall4 name arg0 arg1 arg2 arg3

                _ :: _ :: _ ->
                    Elm.string "TODO"

        expr : List Elm.Expression -> Elm.Expression
        expr argValues =
            innerCall
                argValues
                (deps
                    |> Set.toList
                    |> List.map Elm.string
                    |> Elm.list
                )
                |> Elm.withType (Gen.Glsl.Helper.annotation_.expression (typeToAnnotation returnType))
    in
    Elm.function argDecls expr
        |> Elm.declaration name
        |> Elm.expose


typeToAnnotation : Type -> Type.Annotation
typeToAnnotation type_ =
    case type_ of
        TBool ->
            Type.bool

        TFloat ->
            Type.float

        TInt ->
            Type.int

        TVec2 ->
            Gen.Glsl.Helper.annotation_.vec2

        TIVec2 ->
            Gen.Glsl.Helper.annotation_.iVec2

        TVec3 ->
            Gen.Glsl.Helper.annotation_.vec3

        TIVec3 ->
            Gen.Glsl.Helper.annotation_.iVec3

        TVec4 ->
            Gen.Glsl.Helper.annotation_.vec4

        TIVec4 ->
            Gen.Glsl.Helper.annotation_.iVec4

        TMat3 ->
            Gen.Glsl.Helper.annotation_.mat3

        TVoid ->
            Gen.Glsl.Helper.annotation_.void


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
    String.concat (baseName :: List.map typeToShort argTypes)


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


builtinFunctions : Dict String ( List Type, Type )
builtinFunctions =
    let
        overload : List String -> List ( List Type, Type ) -> List ( String, List Type, Type )
        overload names kinds =
            List.Extra.lift2
                (\name ( inTypes, result ) -> ( name, inTypes, result ))
                names
                kinds

        regular : List ( String, List Type, Type )
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

        vecs : List ( String, List Type, Type )
        vecs =
            [ ( 2, TVec2 )
            , ( 3, TVec3 )
            , ( 4, TVec4 )
            ]
                |> List.concatMap
                    (\( size, type_ ) ->
                        List.map
                            (\inTypes ->
                                ( "vec" ++ String.fromInt size, inTypes, type_ )
                            )
                            ([ TFloat, TInt ]
                                |> List.repeat size
                                |> List.Extra.cartesianProduct
                                |> (++) [ [ TInt ], [ TFloat ], [ TFloat, TVec3 ] ]
                            )
                    )

        ivecs : List ( String, List Type, Type )
        ivecs =
            [ ( 2, TVec2 )
            , ( 3, TVec3 )
            , ( 4, TVec4 )
            ]
                |> List.map
                    (\( size, type_ ) ->
                        ( "ivec" ++ String.fromInt size, List.repeat size TInt, type_ )
                    )

        others : List ( String, List Type, Type )
        others =
            [ ( "cross", [ TVec3, TVec3 ], TVec3 )
            , ( "float", [ TInt ], TFloat )
            , ( "int", [ TFloat ], TInt )
            ]
    in
    (regular ++ vecs ++ ivecs ++ others)
        |> List.map
            (\( name, inTypes, resultType ) ->
                ( fullName name inTypes, ( inTypes, resultType ) )
            )
        |> Dict.fromList


builtin_v_s : ( List String, List ( List Type, Type ) )
builtin_v_s =
    ( [ -- Geometric
        "length"
      ]
    , [ ( [ TFloat ], TFloat )
      , ( [ TVec2 ], TFloat )
      , ( [ TVec3 ], TFloat )
      , ( [ TVec4 ], TFloat )
      ]
    )


builtin_v_v : ( List String, List ( List Type, Type ) )
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
    , [ ( [ TFloat ], TFloat )
      , ( [ TVec2 ], TVec2 )
      , ( [ TVec3 ], TVec3 )
      , ( [ TVec4 ], TVec4 )
      ]
    )


builtin_vv_v : ( List String, List ( List Type, Type ) )
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
    , [ ( [ TFloat, TFloat ], TFloat )
      , ( [ TVec2, TVec2 ], TVec2 )
      , ( [ TVec3, TVec3 ], TVec3 )
      , ( [ TVec4, TVec4 ], TVec4 )
      ]
    )


builtin_sv_v : ( List String, List ( List Type, Type ) )
builtin_sv_v =
    ( [ -- Other
        "step"
      ]
    , [ ( [ TFloat, TFloat ], TFloat )
      , ( [ TFloat, TVec2 ], TVec2 )
      , ( [ TFloat, TVec3 ], TVec3 )
      , ( [ TFloat, TVec4 ], TVec4 )
      ]
    )


builtin_vs_v : ( List String, List ( List Type, Type ) )
builtin_vs_v =
    ( [ -- Comparison
        "min"
      , "max"

      -- Other
      , "mod"
      ]
    , [ ( [ TFloat, TFloat ], TFloat )
      , ( [ TVec2, TFloat ], TVec2 )
      , ( [ TVec3, TFloat ], TVec3 )
      , ( [ TVec4, TFloat ], TVec4 )
      ]
    )


builtin_vv_s : ( List String, List ( List Type, Type ) )
builtin_vv_s =
    ( [ -- Geometric
        "distance"
      , "dot"
      ]
    , [ ( [ TFloat, TFloat ], TFloat )
      , ( [ TVec2, TVec2 ], TFloat )
      , ( [ TVec3, TVec3 ], TFloat )
      , ( [ TVec4, TVec4 ], TFloat )
      ]
    )


builtin_vvs_v : ( List String, List ( List Type, Type ) )
builtin_vvs_v =
    ( [ --Geometric
        "refract"

      -- Other
      , "mix"
      ]
    , [ ( [ TFloat, TFloat, TFloat ], TFloat )
      , ( [ TVec2, TVec2, TFloat ], TVec2 )
      , ( [ TVec3, TVec3, TFloat ], TVec3 )
      , ( [ TVec4, TVec4, TFloat ], TVec4 )
      ]
    )


builtin_ssv_v : ( List String, List ( List Type, Type ) )
builtin_ssv_v =
    ( [ -- Other
        "smoothstep"
      ]
    , [ ( [ TFloat, TFloat, TFloat ], TFloat )
      , ( [ TFloat, TFloat, TVec2 ], TVec2 )
      , ( [ TFloat, TFloat, TVec3 ], TVec3 )
      , ( [ TFloat, TFloat, TVec4 ], TVec4 )
      ]
    )


builtin_vss_v : ( List String, List ( List Type, Type ) )
builtin_vss_v =
    ( [ -- Other
        "clamp"
      ]
    , [ ( [ TFloat, TFloat, TFloat ], TFloat )
      , ( [ TVec2, TFloat, TFloat ], TVec2 )
      , ( [ TVec3, TFloat, TFloat ], TVec3 )
      , ( [ TVec4, TFloat, TFloat ], TVec4 )
      ]
    )


builtin_vvv_v : ( List String, List ( List Type, Type ) )
builtin_vvv_v =
    ( [ -- Other
        "clamp"
      , "mix"
      , "smoothstep"

      -- Geometry
      , "faceforward"
      ]
    , [ ( [ TFloat, TFloat, TFloat ], TFloat )
      , ( [ TVec2, TVec2, TVec2 ], TVec2 )
      , ( [ TVec3, TVec3, TVec3 ], TVec3 )
      , ( [ TVec4, TVec4, TVec4 ], TVec4 )
      ]
    )


builtinDecls : List Elm.Declaration
builtinDecls =
    builtinFunctions
        |> Dict.toList
        |> List.map
            (\( name, ( inTypes, resultType ) ) ->
                wrapFunction
                    name
                    (List.indexedMap
                        (\i type_ -> ( type_, indexedVar i ))
                        inTypes
                    )
                    resultType
                    Set.empty
                    |> Elm.expose
            )


indexedVar : Int -> String
indexedVar i =
    String.fromChar <| Char.fromCode <| Char.toCode 'a' + i


parseFile : String -> Result (List Parser.DeadEnd) (List Function)
parseFile glsl =
    Parser.run Glsl.Parser.file glsl
