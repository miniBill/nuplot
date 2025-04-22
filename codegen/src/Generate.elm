module Generate exposing (run)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Dict exposing (Dict)
import Elm
import Elm.Annotation as Type
import FatalError exposing (FatalError)
import Gen.Glsl
import Glsl exposing (BinaryOperation(..), Declaration(..), Expr(..), Expression(..), Function, RelationOperation(..), Stat(..), Statement(..), Type(..), Uniform)
import Glsl.Parser
import Glsl.PrettyPrinter
import List.Extra
import Pages.Script as Script exposing (Script)
import Parser
import Result.Extra
import Set
import SortedSet exposing (SortedSet)


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    File.rawFile "codegen/functions.frag"
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\glsl ->
                case generate glsl of
                    Err msg ->
                        let
                            reflowedMsg : String
                            reflowedMsg =
                                msg
                                    |> String.split "\n"
                                    |> (::) ""
                                    |> String.join "\n  "

                            coloredMsg : String
                            coloredMsg =
                                Ansi.Color.fontColor Ansi.Color.red "Error:"
                                    ++ reflowedMsg
                        in
                        BackendTask.fail (FatalError.fromString coloredMsg)

                    Ok declarations ->
                        let
                            file =
                                declarations
                                    |> List.Extra.gatherEqualsBy Tuple.first
                                    |> List.map
                                        (\( head, tail ) ->
                                            let
                                                group : List Elm.Declaration
                                                group =
                                                    List.map Tuple.second (head :: tail)
                                            in
                                            case Tuple.first head of
                                                Just groupName ->
                                                    Elm.group (Elm.docs ("#" ++ groupName) :: group)

                                                Nothing ->
                                                    Elm.group group
                                        )
                                    |> Elm.file
                                        [ "Glsl", "Functions", "NuPlot" ]
                        in
                        Script.writeFile
                            { path = "generated/" ++ file.path
                            , body = file.contents
                            }
                            |> BackendTask.allowFatal
            )


generate : String -> Result String (List ( Maybe String, Elm.Declaration ))
generate glsl =
    case Parser.run Glsl.Parser.file glsl of
        Err e ->
            Err (parserErrorToString glsl e)

        Ok ( _, declarations ) ->
            let
                functions =
                    List.filterMap
                        (\declaration ->
                            case declaration of
                                FunctionDeclaration function ->
                                    Just function

                                UniformDeclaration _ ->
                                    Nothing

                                ConstDeclaration _ ->
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

                                ConstDeclaration _ ->
                                    Nothing
                        )
                        declarations

                constants =
                    List.filterMap
                        (\declaration ->
                            case declaration of
                                ConstDeclaration const ->
                                    Just const

                                FunctionDeclaration _ ->
                                    Nothing

                                UniformDeclaration _ ->
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
                            constants
                                |> List.map (\{ tipe, name } -> ( name, tipe ))
                                |> Dict.fromList
                                |> Dict.insert "P32M1" TInt
                                |> Dict.insert "P31" TInt
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

                maybeDecls : Result String (List ( Maybe String, Elm.Declaration ))
                maybeDecls =
                    Result.Extra.combineMap
                        (functionToDeclarations env)
                        functions
                        |> Result.map List.concat

                uniformDecls : List ( Maybe String, Elm.Declaration )
                uniformDecls =
                    List.map uniformToDeclaration uniforms
            in
            case maybeDecls of
                Ok decls ->
                    Ok (decls ++ builtinDecls ++ uniformDecls)

                Err e ->
                    Err ("Error generating file: " ++ e)


uniformToDeclaration : Uniform -> ( Maybe String, Elm.Declaration )
uniformToDeclaration { name, tipe } =
    ( Just "uniforms"
    , Gen.Glsl.unsafeVar name
        |> Elm.withType (Gen.Glsl.annotation_.expression (typeToAnnotation tipe))
        |> Elm.declaration name
        |> Elm.expose
    )


functionHasType : String -> List Type -> Type -> Env -> Env
functionHasType baseName argTypes returnType env =
    { env | functionsEnv = Dict.insert (fullName baseName argTypes) returnType env.functionsEnv }


parserErrorToString : String -> List Parser.DeadEnd -> String
parserErrorToString input err =
    err
        |> List.Extra.gatherEqualsBy (\{ row, col } -> ( row, col ))
        |> List.map (errorToString input)
        |> String.join "\n\n"


errorToString : String -> ( Parser.DeadEnd, List Parser.DeadEnd ) -> String
errorToString source ( error, errors ) =
    let
        -- How many lines of context to show
        context : Int
        context =
            4

        lines : List String
        lines =
            String.split "\n" source
                |> List.drop (error.row - context)
                |> List.take (context * 2)

        errorString : String
        errorString =
            [ String.repeat (error.col - 1) " "
            , Ansi.Color.fontColor Ansi.Color.red "^ "
            , " at row "
            , String.fromInt error.row
            , ", col "
            , String.fromInt error.col
            , " "
            , (error :: errors)
                |> List.map (\{ problem } -> problemToString problem)
                |> Set.fromList
                |> Set.toList
                |> String.join ", "
                |> Ansi.Color.fontColor Ansi.Color.yellow
            ]
                |> String.concat

        before : Int
        before =
            min error.row context
    in
    List.take before lines
        ++ errorString
        :: List.drop before lines
        |> String.join "\n"


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


functionToDeclarations : Env -> Function -> Result String (List ( Maybe String, Elm.Declaration ))
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
            [ ( Nothing
              , function.stat
                    |> Glsl.PrettyPrinter.stat 0
                    |> Elm.string
                    |> Elm.declaration (fname ++ "Body")
              )
            , wrapFunction function.name (SortedSet.insert fname deps) function.args function.returnType
            ]
        )
        maybeDeps


wrapFunction : String -> SortedSet String -> List ( Type, String ) -> Type -> ( Maybe String, Elm.Declaration )
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
    ( if
        List.member (String.left 1 name) [ "g", "i", "c" ]
            && not (List.member name [ "cbrt", "increment", "cosh", "is_even", "is_odd" ])
      then
        Just (String.dropLeft 1 name)

      else
        Just name
    , Elm.function argDecls expr
        |> Elm.declaration fname
        |> Elm.expose
    )


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

        TBVec2 ->
            Gen.Glsl.annotation_.bVec2

        TVec3 ->
            Gen.Glsl.annotation_.vec3

        TIVec3 ->
            Gen.Glsl.annotation_.iVec3

        TBVec3 ->
            Gen.Glsl.annotation_.bVec3

        TVec4 ->
            Gen.Glsl.annotation_.vec4

        TIVec4 ->
            Gen.Glsl.annotation_.iVec4

        TBVec4 ->
            Gen.Glsl.annotation_.bVec4

        TMat3 ->
            Gen.Glsl.annotation_.mat3

        TVoid ->
            Gen.Glsl.annotation_.void

        TIn tt ->
            Gen.Glsl.annotation_.in_ (typeToAnnotation tt)

        TOut tt ->
            Gen.Glsl.annotation_.out (typeToAnnotation tt)

        TUint ->
            Type.int

        TUVec2 ->
            Gen.Glsl.annotation_.uVec2

        TUVec3 ->
            Gen.Glsl.annotation_.uVec3

        TUVec4 ->
            Gen.Glsl.annotation_.uVec4

        TDouble ->
            Type.float

        TDVec2 ->
            Gen.Glsl.annotation_.dVec2

        TDVec3 ->
            Gen.Glsl.annotation_.dVec3

        TDVec4 ->
            Gen.Glsl.annotation_.dVec4

        TMat2 ->
            Gen.Glsl.annotation_.mat2

        TMat4 ->
            Gen.Glsl.annotation_.mat4

        TMat23 ->
            Gen.Glsl.annotation_.mat23

        TMat24 ->
            Gen.Glsl.annotation_.mat24

        TMat32 ->
            Gen.Glsl.annotation_.mat32

        TMat34 ->
            Gen.Glsl.annotation_.mat34

        TMat42 ->
            Gen.Glsl.annotation_.mat42

        TMat43 ->
            Gen.Glsl.annotation_.mat43

        TDMat2 ->
            Gen.Glsl.annotation_.dMat2

        TDMat3 ->
            Gen.Glsl.annotation_.dMat3

        TDMat4 ->
            Gen.Glsl.annotation_.dMat4

        TDMat23 ->
            Gen.Glsl.annotation_.dMat23

        TDMat24 ->
            Gen.Glsl.annotation_.dMat24

        TDMat32 ->
            Gen.Glsl.annotation_.dMat32

        TDMat34 ->
            Gen.Glsl.annotation_.dMat34

        TDMat42 ->
            Gen.Glsl.annotation_.dMat42

        TDMat43 ->
            Gen.Glsl.annotation_.dMat43


variableHasType : String -> Type -> Env -> Env
variableHasType var type_ env =
    { env | variablesEnv = Dict.insert var type_ env.variablesEnv }


union : List (Result String (SortedSet comparable)) -> Result String (SortedSet comparable)
union =
    List.foldl (Result.map2 SortedSet.insertAll) (Ok SortedSet.empty)


findDepsStatement : Env -> Stat -> Result String (SortedSet String)
findDepsStatement env statement =
    case statement of
        If cond true ->
            union
                [ findDepsExpression env cond
                , findDepsStatement env true
                ]

        IfElse cond true false ->
            union
                [ findDepsExpression env cond
                , findDepsStatement env true
                , findDepsStatement env false
                ]

        ExpressionStatement e ->
            findDepsExpression env e

        For from cond step loop ->
            union
                [ case from of
                    Just f ->
                        findDepsStatement env f

                    Nothing ->
                        Ok SortedSet.empty
                , findDepsExpression env cond
                , findDepsExpression env step
                , findDepsStatement env loop
                ]

        Return e ->
            findDepsExpression env e

        Decl type_ var maybeVal ->
            let
                newEnv : Env
                newEnv =
                    variableHasType var type_ env
            in
            maybeVal
                |> Maybe.map (findDepsExpression newEnv)
                |> Maybe.withDefault (Ok SortedSet.empty)

        Nop ->
            Ok SortedSet.empty

        Break ->
            Ok SortedSet.empty

        Continue ->
            Ok SortedSet.empty

        Block l m r ->
            union (List.map (findDepsStatement env) (l :: m :: r))


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

                Uint _ ->
                    Ok ( TInt, SortedSet.empty )

                Double _ ->
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

                Call (Variable f) args ->
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

                Call _ _ ->
                    Err "Complex calls are not supported yet"

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

                BinaryOperation l bop r ->
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

        ArraySubscript ->
            "[]"

        Mod ->
            "%"

        ShiftLeft ->
            "<<"

        ShiftRight ->
            ">>"

        BitwiseAnd ->
            "&"

        BitwiseOr ->
            "|"

        BitwiseXor ->
            "^"

        Xor ->
            "^^"

        Assign ->
            "="

        ComboAdd ->
            "+="

        ComboSubtract ->
            "-="

        ComboBy ->
            "*="

        ComboDiv ->
            "/="

        ComboMod ->
            "%="

        ComboLeftShift ->
            "<<="

        ComboRightShift ->
            ">>="

        ComboBitwiseAnd ->
            "&="

        ComboBitwiseXor ->
            "^="

        ComboBitwiseOr ->
            "|="

        Comma ->
            ","

        RelationOperation LessThan ->
            "<"

        RelationOperation GreaterThan ->
            ">"

        RelationOperation LessThanOrEquals ->
            "<="

        RelationOperation GreaterThanOrEquals ->
            ">="

        RelationOperation Equals ->
            "=="

        RelationOperation NotEquals ->
            "!="


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

        TBool ->
            "b1"

        TUint ->
            "u1"

        TDouble ->
            "d1"

        TVec2 ->
            "2"

        TIVec2 ->
            "i2"

        TBVec2 ->
            "b2"

        TUVec2 ->
            "u2"

        TDVec2 ->
            "d2"

        TVec3 ->
            "3"

        TIVec3 ->
            "i3"

        TBVec3 ->
            "b3"

        TUVec3 ->
            "u3"

        TDVec3 ->
            "d3"

        TVec4 ->
            "4"

        TIVec4 ->
            "i4"

        TBVec4 ->
            "b4"

        TUVec4 ->
            "u4"

        TDVec4 ->
            "d4"

        TMat2 ->
            "m2"

        TDMat2 ->
            "dm2"

        TMat23 ->
            "m23"

        TDMat23 ->
            "dm23"

        TMat24 ->
            "m24"

        TDMat24 ->
            "dm24"

        TMat32 ->
            "m32"

        TDMat32 ->
            "dm32"

        TMat42 ->
            "m42"

        TDMat42 ->
            "dm42"

        TMat3 ->
            "m3"

        TDMat3 ->
            "dm3"

        TMat34 ->
            "m34"

        TDMat34 ->
            "dm34"

        TMat43 ->
            "m43"

        TDMat43 ->
            "dm43"

        TMat4 ->
            "m4"

        TDMat4 ->
            "dm4"

        TVoid ->
            "v"

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


builtinDecls : List ( Maybe String, Elm.Declaration )
builtinDecls =
    builtinFunctions
        |> Dict.toList
        |> List.map
            (\( _, { baseName, args, return } ) ->
                ( Just baseName
                , wrapFunction
                    baseName
                    SortedSet.empty
                    (List.indexedMap
                        (\i type_ -> ( type_, indexedVar i ))
                        args
                    )
                    return
                    |> Tuple.second
                    |> Elm.expose
                )
            )


indexedVar : Int -> String
indexedVar i =
    String.fromChar <| Char.fromCode <| Char.toCode 'a' + i
