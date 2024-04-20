module Generate exposing (main)

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import Gen.Debug
import Gen.Glsl
import Glsl exposing (BinaryOperation(..), Declaration(..), Expr(..), Expression(..), Function, Stat(..), Statement(..), Type(..), Uniform)
import Glsl.Parser
import Glsl.PrettyPrinter
import List.Extra
import Parser
import Result.Extra
import SortedSet exposing (SortedSet)


main : Program String () ()
main =
    let
        file : String -> Elm.File
        file glsl =
            Elm.fileWith [ "Glsl", "Functions" ]
                { docs =
                    List.map
                        (\{ group, members } ->
                            Elm.docs
                                { group = group
                                , members = List.sort members
                                }
                        )
                , aliases = []
                }
                (generate glsl)
    in
    Generate.fromText (\glsl -> [ file glsl ])


generate : String -> List Elm.Declaration
generate glsl =
    case parseFile glsl of
        Err e ->
            Gen.Debug.todo "Error parsing file"
                |> Elm.declaration "err"
                |> Elm.withDocumentation (errorToString e)
                |> List.singleton

        Ok declarations ->
            let
                functions =
                    List.filterMap
                        (\declaration ->
                            case declaration of
                                FunctionDeclaration function ->
                                    Just function

                                UniformDeclaration _ ->
                                    Nothing
                        )
                        declarations

                uniforms =
                    List.filterMap
                        (\declaration ->
                            case declaration of
                                UniformDeclaration uniform ->
                                    Just uniform

                                FunctionDeclaration _ ->
                                    Nothing
                        )
                        declarations

                env : Env
                env =
                    List.foldl
                        (\{ name, returnType, args } ->
                            functionHasType
                                name
                                (List.map Tuple.first args)
                                returnType
                        )
                        { constantsEnv =
                            Dict.fromList
                                [ ( "P32M1", TInt )
                                , ( "P31", TInt )
                                ]
                        , functionsEnv =
                            builtinFunctions
                                |> Dict.map
                                    (\_ { return } -> return)
                        , variablesEnv =
                            uniforms
                                |> List.map (\{ tipe, name } -> ( name, tipe ))
                                |> Dict.fromList
                        }
                        functions

                maybeDecls : Result String (List (List Elm.Declaration))
                maybeDecls =
                    Result.Extra.combineMap
                        (functionToDeclarations env)
                        functions

                uniformDecls : List Elm.Declaration
                uniformDecls =
                    List.map uniformToDeclaration uniforms
            in
            case maybeDecls of
                Ok decls ->
                    List.concat decls ++ builtinDecls ++ uniformDecls

                Err e ->
                    "Error generating file"
                        |> Gen.Debug.todo
                        |> Elm.declaration "err"
                        |> Elm.withDocumentation e
                        |> List.singleton


uniformToDeclaration : Uniform -> Elm.Declaration
uniformToDeclaration { name, tipe } =
    Gen.Glsl.var name
        |> Elm.withType (Gen.Glsl.annotation_.expression (typeToAnnotation tipe))
        |> Elm.declaration name
        |> Elm.exposeWith { exposeConstructor = False, group = Just "uniforms" }


functionHasType : String -> List Type -> Type -> Env -> Env
functionHasType baseName argTypes returnType env =
    { env | functionsEnv = Dict.insert (fullName baseName argTypes) returnType env.functionsEnv }


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
    { constantsEnv : Dict String Type
    , functionsEnv : Dict String Type
    , variablesEnv : Dict String Type
    }


functionToDeclarations : Env -> Function -> Result String (List Elm.Declaration)
functionToDeclarations env function =
    let
        fname =
            fullName function.name (List.map Tuple.first function.args)

        envWithArgs : Env
        envWithArgs =
            List.foldl (\( type_, name ) -> variableHasType name type_) env function.args

        maybeDeps : Result String (SortedSet String)
        maybeDeps =
            findDepsStatement envWithArgs function.stat
                |> Result.mapError (\e -> e ++ " while generating " ++ fname)
    in
    Result.map
        (\deps ->
            [ Elm.string function.body
                |> Elm.declaration (fname ++ "Body")
            , wrapFunction function.name (SortedSet.insert fname deps) function.args function.returnType
            ]
        )
        maybeDeps


wrapFunction : String -> SortedSet String -> List ( Type, String ) -> Type -> Elm.Declaration
wrapFunction name deps args returnType =
    let
        fname =
            fullName name (List.map Tuple.first args)

        argDecls : List ( String, Maybe Type.Annotation )
        argDecls =
            List.map
                (\( type_, argName ) ->
                    ( argName
                    , Just <| Gen.Glsl.annotation_.expression (typeToAnnotation type_)
                    )
                )
                args

        depsExpr : Elm.Expression
        depsExpr =
            deps
                |> SortedSet.toList
                |> List.map (\dep -> Elm.val <| dep ++ "Body")
                |> Elm.list

        innerCall argValues =
            case argValues of
                [] ->
                    Gen.Glsl.call_.unsafeCall0 (Elm.string name) depsExpr

                [ arg0 ] ->
                    Gen.Glsl.call_.unsafeCall1 (Elm.string name) depsExpr arg0

                [ arg0, arg1 ] ->
                    Gen.Glsl.call_.unsafeCall2 (Elm.string name) depsExpr arg0 arg1

                [ arg0, arg1, arg2 ] ->
                    Gen.Glsl.call_.unsafeCall3 (Elm.string name) depsExpr arg0 arg1 arg2

                [ arg0, arg1, arg2, arg3 ] ->
                    Gen.Glsl.call_.unsafeCall4 (Elm.string name) depsExpr arg0 arg1 arg2 arg3

                _ :: _ :: _ ->
                    Elm.string "TODO"

        expr : List Elm.Expression -> Elm.Expression
        expr argValues =
            innerCall
                argValues
                |> Elm.withType (Gen.Glsl.annotation_.expression (typeToAnnotation returnType))
    in
    Elm.function argDecls expr
        |> Elm.declaration fname
        |> Elm.exposeWith
            { exposeConstructor = False
            , group =
                Just <|
                    if
                        List.member (String.left 1 name) [ "g", "i", "c" ]
                            && not (List.member name [ "cbrt", "increment", "cosh", "is_even", "is_odd" ])
                    then
                        String.dropLeft 1 name

                    else
                        name
            }


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
            Gen.Glsl.annotation_.vec2

        TIVec2 ->
            Gen.Glsl.annotation_.iVec2

        TVec3 ->
            Gen.Glsl.annotation_.vec3

        TIVec3 ->
            Gen.Glsl.annotation_.iVec3

        TVec4 ->
            Gen.Glsl.annotation_.vec4

        TIVec4 ->
            Gen.Glsl.annotation_.iVec4

        TMat3 ->
            Gen.Glsl.annotation_.mat3

        TVoid ->
            Gen.Glsl.annotation_.void

        TIn tt ->
            Gen.Glsl.annotation_.in_ (typeToAnnotation tt)

        TOut tt ->
            Gen.Glsl.annotation_.out (typeToAnnotation tt)


variableHasType : String -> Type -> Env -> Env
variableHasType var type_ env =
    { env | variablesEnv = Dict.insert var type_ env.variablesEnv }


union : List (Result String (SortedSet comparable)) -> Result String (SortedSet comparable)
union =
    List.foldl (Result.map2 SortedSet.insertAll) (Ok SortedSet.empty)


findDepsStatement : Env -> Stat -> Result String (SortedSet String)
findDepsStatement env statement =
    case statement of
        If cond true cont ->
            union
                [ findDepsExpression env cond
                , findDepsStatement env true
                , findDepsStatement env cont
                ]

        IfElse cond true false cont ->
            union
                [ findDepsExpression env cond
                , findDepsStatement env true
                , findDepsStatement env false
                , findDepsStatement env cont
                ]

        ExpressionStatement e cont ->
            union
                [ findDepsExpression env e
                , findDepsStatement env cont
                ]

        For var from _ to _ step cont ->
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

        Decl type_ var maybeVal cont ->
            let
                newEnv : Env
                newEnv =
                    variableHasType var type_ env
            in
            union
                [ maybeVal
                    |> Maybe.map (findDepsExpression env)
                    |> Maybe.withDefault (Ok SortedSet.empty)
                , findDepsStatement newEnv cont
                ]

        Nop ->
            Ok SortedSet.empty

        Break ->
            Ok SortedSet.empty

        Continue ->
            Ok SortedSet.empty


findDepsExpression : Env -> Expr -> Result String (SortedSet String)
findDepsExpression env =
    let
        go : Expr -> Result String ( Type, SortedSet String )
        go expr =
            case expr of
                Float _ ->
                    Ok ( TFloat, SortedSet.empty )

                Int _ ->
                    Ok ( TInt, SortedSet.empty )

                Bool _ ->
                    Ok ( TBool, SortedSet.empty )

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

                Variable "P32M1" ->
                    Ok ( TInt, SortedSet.empty )

                Variable "P31" ->
                    Ok ( TInt, SortedSet.empty )

                Variable v ->
                    case Dict.get v env.variablesEnv of
                        Nothing ->
                            Err <| "Couldn't find variable " ++ v

                        Just t ->
                            Ok ( t, SortedSet.empty )

                Call f args ->
                    args
                        |> Result.Extra.combineMap go
                        |> Result.map List.unzip
                        |> Result.andThen
                            (\( argTypes, argDeps ) ->
                                let
                                    deps =
                                        if Dict.member fname builtinFunctions then
                                            List.foldl SortedSet.insertAll SortedSet.empty argDeps

                                        else
                                            List.foldl SortedSet.insertAll (SortedSet.singleton fname) argDeps

                                    fname =
                                        fullName f argTypes
                                in
                                case Dict.get fname env.functionsEnv of
                                    Nothing ->
                                        Err <| "Function " ++ fname ++ " not found"

                                    Just to ->
                                        Ok ( to, deps )
                            )

                Array l r ->
                    Result.andThen
                        (\( ( lt, ld ), ( rt, rd ) ) ->
                            Result.map
                                (\t -> ( t, SortedSet.insertAll ld rd ))
                                (case ( lt, rt ) of
                                    ( TVec3, TInt ) ->
                                        Ok TFloat

                                    _ ->
                                        Err <| "Don't know what the result of " ++ Glsl.PrettyPrinter.type_ lt ++ "[" ++ Glsl.PrettyPrinter.type_ rt ++ "] is"
                                )
                        )
                        (Result.map2 Tuple.pair
                            (go l)
                            (go r)
                        )

                Ternary c t f ->
                    Result.map3
                        (\( _, cd ) ( tt, td ) ( _, fd ) ->
                            ( tt
                            , SortedSet.insertAll cd td
                                |> SortedSet.insertAll fd
                            )
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
                                (\t -> ( t, SortedSet.insertAll ld rd ))
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
                                            Err <| "Don't know what the result of " ++ Glsl.PrettyPrinter.type_ lt ++ " " ++ binaryOperationToString bop ++ " " ++ Glsl.PrettyPrinter.type_ rt
                                )
                        )
                        (Result.map2 Tuple.pair
                            (go l)
                            (go r)
                        )

                Comparison _ l r ->
                    Result.map2
                        (\( _, ld ) ( _, rd ) ->
                            ( TBool, SortedSet.insertAll ld rd )
                        )
                        (go l)
                        (go r)

                PostfixIncrement e ->
                    go e

                PostfixDecrement e ->
                    go e

                AssignCombo _ l r ->
                    Result.map2
                        (\( lt, ld ) ( _, rd ) ->
                            ( lt, SortedSet.insertAll ld rd )
                        )
                        (go l)
                        (go r)
    in
    go >> Result.map Tuple.second


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

        And ->
            "&&"

        Or ->
            "||"


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

        TIn tt ->
            "n" ++ typeToShort tt

        TOut tt ->
            "o" ++ typeToShort tt


builtinFunctions : Dict String { baseName : String, args : List Type, return : Type }
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
                                |> (++)
                                    (if size == 4 then
                                        [ [ TInt ]
                                        , [ TFloat ]
                                        , [ TFloat, TVec3 ]
                                        , [ TVec3, TFloat ]
                                        ]

                                     else
                                        [ [ TInt ], [ TFloat ] ]
                                    )
                            )
                    )

        mats : List ( String, List Type, Type )
        mats =
            [ ( [ TVec3, TVec3, TVec3 ], 3, TMat3 ) ]
                |> List.map
                    (\( inTypes, size, type_ ) ->
                        ( "mat" ++ String.fromInt size
                        , inTypes
                        , type_
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

        builtTuple :
            ( String, List Type, Type )
            -> ( String, { baseName : String, args : List Type, return : Type } )
        builtTuple ( name, inTypes, resultType ) =
            ( fullName name inTypes, { baseName = name, args = inTypes, return = resultType } )
    in
    (regular ++ vecs ++ ivecs ++ mats ++ others)
        |> List.map builtTuple
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
    ( [ "fwidth"

      -- Rounding
      , "ceil"
      , "floor"
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
            (\( _, { baseName, args, return } ) ->
                wrapFunction
                    baseName
                    SortedSet.empty
                    (List.indexedMap
                        (\i type_ -> ( type_, indexedVar i ))
                        args
                    )
                    return
                    |> Elm.exposeWith { exposeConstructor = False, group = Just baseName }
            )


indexedVar : Int -> String
indexedVar i =
    String.fromChar <| Char.fromCode <| Char.toCode 'a' + i


parseFile : String -> Result (List Parser.DeadEnd) (List Declaration)
parseFile glsl =
    Parser.run Glsl.Parser.file glsl
