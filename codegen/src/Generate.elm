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
import Parser.Advanced
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
    case Parser.Advanced.run Glsl.Parser.file glsl of
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
                        , functionsEnv = Dict.empty
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
            in
            case maybeDecls of
                Ok decls ->
                    let
                        uniformDecls : List ( Maybe String, Elm.Declaration )
                        uniformDecls =
                            List.map uniformToDeclaration uniforms
                    in
                    Ok (decls ++ uniformDecls)

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


parserErrorToString : String -> List Glsl.Parser.DeadEnd -> String
parserErrorToString input err =
    err
        |> List.Extra.gatherEqualsBy (\{ row, col, contextStack } -> ( row, col, contextStack ))
        |> List.map (errorToString input)
        |> String.join "\n\n"


errorToString : String -> ( Glsl.Parser.DeadEnd, List Glsl.Parser.DeadEnd ) -> String
errorToString source ( error, errors ) =
    let
        -- How many lines of context to show
        contextSize : Int
        contextSize =
            4

        lines : List String
        lines =
            String.split "\n" source
                |> List.drop (error.row - contextSize)
                |> List.take (contextSize * 2)

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
            , " while "
            , error.contextStack
                |> List.map (\frame -> contextToString frame.context)
                |> String.join ", while "
            ]
                |> String.concat

        before : Int
        before =
            min error.row contextSize
    in
    List.take before lines
        ++ errorString
        :: List.drop before lines
        |> String.join "\n"


contextToString : Glsl.Parser.Context -> String
contextToString context =
    case context of
        Glsl.Parser.ParsingFile ->
            "parsing a file"

        Glsl.Parser.ParsingFunction ->
            "parsing a function"

        Glsl.Parser.ParsingStatement ->
            "parsing a statement"

        Glsl.Parser.ParsingExpression ->
            "parsing an expression"

        Glsl.Parser.ParsingForInitialization ->
            "parsing a for's initialization"

        Glsl.Parser.ParsingForCondition ->
            "parsing a for's condition"

        Glsl.Parser.ParsingForStep ->
            "parsing a for's step"

        Glsl.Parser.ParsingForBody ->
            "parsing a for's body"


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
        functionName : String
        functionName =
            fullName function.name (List.map Tuple.first function.args)

        envWithArgs : Env
        envWithArgs =
            List.foldl (\( type_, name ) -> variableHasType name type_) env function.args

        maybeDeps : Result String ( Env, SortedSet String )
        maybeDeps =
            findDepsStatement function.stat envWithArgs
                |> Result.mapError (\e -> e ++ " while generating " ++ functionName)
    in
    Result.map
        (\( _, deps ) ->
            [ ( Nothing
              , function.stat
                    |> Glsl.PrettyPrinter.stat 0
                    |> Elm.string
                    |> Elm.declaration (functionName ++ "Body")
              )
            , wrapFunction function.name (SortedSet.insert functionName deps) function.args function.returnType
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


union : List (Env -> Result String ( Env, SortedSet comparable )) -> Env -> Result String ( Env, SortedSet comparable )
union list initialEnv =
    List.foldl
        (\e a ->
            a
                |> Result.andThen
                    (\( env, deps ) ->
                        e env
                            |> Result.map
                                (Tuple.mapSecond
                                    (\newDeps -> SortedSet.insertAll newDeps deps)
                                )
                    )
        )
        (Ok ( initialEnv, SortedSet.empty ))
        list


findDepsStatement : Stat -> Env -> Result String ( Env, SortedSet String )
findDepsStatement statement =
    case statement of
        If cond true ->
            union
                [ findDepsExpression cond
                , findDepsStatement true
                ]

        IfElse cond true false ->
            union
                [ findDepsExpression cond
                , findDepsStatement true
                , findDepsStatement false
                ]

        ExpressionStatement e ->
            findDepsExpression e

        For from cond step loop ->
            union
                [ case from of
                    Just f ->
                        findDepsStatement f

                    Nothing ->
                        nop
                , findDepsExpression cond
                , findDepsExpression step
                , findDepsStatement loop
                ]

        Return e ->
            findDepsExpression e

        Decl type_ var maybeVal ->
            \env ->
                let
                    newEnv : Env
                    newEnv =
                        variableHasType var type_ env
                in
                case maybeVal of
                    Nothing ->
                        nop newEnv

                    Just val ->
                        findDepsExpression val newEnv

        Nop ->
            nop

        Break ->
            nop

        Continue ->
            nop

        Block l m r ->
            union (List.map findDepsStatement (l :: m :: r))


nop : Env -> Result String ( Env, SortedSet String )
nop env =
    Ok ( env, SortedSet.empty )


findDepsExpression : Expr -> Env -> Result String ( Env, SortedSet String )
findDepsExpression root env =
    let
        go : Expr -> Result String (SortedSet String)
        go expr =
            case expr of
                Float _ ->
                    Ok SortedSet.empty

                Int _ ->
                    Ok SortedSet.empty

                Uint _ ->
                    Ok SortedSet.empty

                Double _ ->
                    Ok SortedSet.empty

                Bool _ ->
                    Ok SortedSet.empty

                Dot e _ ->
                    go e

                Variable _ ->
                    Ok SortedSet.empty

                Call (Variable f) args ->
                    args
                        |> Result.Extra.combineMap go
                        |> Result.andThen
                            (\argDeps ->
                                let
                                    initial =
                                        SortedSet.singleton (fullName f [])

                                    deps =
                                        List.foldl SortedSet.insertAll initial argDeps
                                in
                                Ok deps
                            )

                Call _ _ ->
                    Err "Complex calls are not supported yet"

                Ternary c t f ->
                    Result.map3
                        (\cd td fd ->
                            SortedSet.insertAll cd td
                                |> SortedSet.insertAll fd
                        )
                        (go c)
                        (go t)
                        (go f)

                UnaryOperation _ e ->
                    go e

                BinaryOperation l _ r ->
                    Result.map2
                        (\ld rd ->
                            SortedSet.insertAll ld rd
                        )
                        (go l)
                        (go r)
    in
    go root
        |> Result.map (\deps -> ( env, deps ))


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
