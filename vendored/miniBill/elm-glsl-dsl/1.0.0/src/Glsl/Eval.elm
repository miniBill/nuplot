module Glsl.Eval exposing (Context, Error(..), Value(..), interpret, value)

import Dict exposing (Dict)
import Glsl exposing (BinaryOperation(..), Expr(..), Expression(..), RelationOperation(..), Stat(..), Statement(..), UnaryOperation(..))


type Value
    = VFloat Float
    | VBool Bool
    | VInt Int
    | VVec2 Float Float
    | VVec3 Float Float Float
    | VMat3 ( Float, Float, Float ) ( Float, Float, Float ) ( Float, Float, Float )
    | VVoid -- Used for void return


type alias Context =
    Dict String Value


type Error
    = MissingVariable String
    | InvalidTypes String


value : Context -> Expression t -> Result Error ( Context, Value )
value initialContext (Expression input) =
    innerValue initialContext input.expr


innerValue : Context -> Expr -> Result Error ( Context, Value )
innerValue ctx e =
    case e of
        Float f ->
            Ok ( ctx, VFloat f )

        Bool b ->
            Ok ( ctx, VBool b )

        Int i ->
            Ok ( ctx, VInt i )

        Variable v ->
            case Dict.get v ctx of
                Just f ->
                    Ok ( ctx, f )

                Nothing ->
                    Err <| MissingVariable v

        UnaryOperation Negate c ->
            innerValue ctx c
                |> Result.andThen
                    (\( ctx2, vc ) ->
                        case vc of
                            VFloat fc ->
                                Ok ( ctx2, VFloat <| negate fc )

                            _ ->
                                Err <|
                                    InvalidTypes
                                        ("Cannot calculate `-` for " ++ valueToString vc)
                    )

        BinaryOperation And l r ->
            innerValue2 ctx l r <|
                \ctx2 vl vr ->
                    case ( vl, vr ) of
                        ( VBool bl, VBool br ) ->
                            Ok ( ctx2, VBool <| bl && br )

                        _ ->
                            Err <|
                                InvalidTypes
                                    ("Cannot calculate `and` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        BinaryOperation Or l r ->
            innerValue2 ctx l r <|
                \ctx2 vl vr ->
                    case ( vl, vr ) of
                        ( VBool bl, VBool br ) ->
                            Ok ( ctx2, VBool <| bl || br )

                        _ ->
                            Err <|
                                InvalidTypes
                                    ("Cannot calculate `or` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        Comparison k l r ->
            innerValue2 ctx l r <|
                \ctx2 vl vr ->
                    case ( vl, vr, k ) of
                        ( VFloat fl, VFloat fr, LessThan ) ->
                            Ok ( ctx2, VBool (fl < fr) )

                        ( VFloat fl, VFloat fr, LessThanOrEquals ) ->
                            Ok ( ctx2, VBool (fl <= fr) )

                        ( VFloat fl, VFloat fr, Equals ) ->
                            Ok ( ctx2, VBool (fl == fr) )

                        ( VFloat fl, VFloat fr, GreaterThanOrEquals ) ->
                            Ok ( ctx2, VBool (fl >= fr) )

                        ( VFloat fl, VFloat fr, GreaterThan ) ->
                            Ok ( ctx2, VBool (fl > fr) )

                        _ ->
                            Err <|
                                InvalidTypes
                                    ("Cannot compare " ++ valueToString vl ++ " and " ++ valueToString vr)

        Ternary _ _ _ ->
            Debug.todo "branch 'Ternary _ _ _' not implemented"

        BinaryOperation Add _ _ ->
            Debug.todo "branch 'Add _ _' not implemented"

        BinaryOperation Subtract _ _ ->
            Debug.todo "branch 'Subtract _ _' not implemented"

        BinaryOperation By l r ->
            innerValue2 ctx l r <|
                \ctx2 vl vr ->
                    case ( vl, vr ) of
                        ( VFloat fl, VFloat fr ) ->
                            Ok ( ctx2, VFloat <| fl * fr )

                        _ ->
                            Err <|
                                InvalidTypes
                                    ("Cannot calculate `*` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        BinaryOperation Div _ _ ->
            Debug.todo "branch 'Div _ _' not implemented"

        Call "vec2" [ l, r ] ->
            innerValue2 ctx l r <|
                \ctx2 vl vr ->
                    case ( vl, vr ) of
                        ( VFloat fl, VFloat fr ) ->
                            Ok ( ctx2, VVec2 fl fr )

                        _ ->
                            Err <|
                                InvalidTypes
                                    ("Cannot calculate `vec2` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        Call "exp" [ l ] ->
            autovectorizingFloatOp ctx "exp" (\fv -> Basics.e ^ fv) l

        Call "cos" [ l ] ->
            autovectorizingFloatOp ctx "cos" Basics.cos l

        Call "sin" [ l ] ->
            autovectorizingFloatOp ctx "sin" Basics.sin l

        Call name args ->
            Debug.todo <| "branch 'Call \"" ++ name ++ "\" [" ++ String.join ", " (List.map (Debug.toString >> (++) " ") args) ++ " ]' not implemented"

        Dot _ _ ->
            Debug.todo "branch 'Dot _ _' not implemented"

        Array _ _ ->
            Debug.todo "branch 'Array _ _' not implemented"

        AssignCombo _ _ _ ->
            Debug.todo "branch 'AssignCombo _ _' not implemented"

        PostfixIncrement _ ->
            Debug.todo "branch 'PostfixIncrement _' not implemented"

        PostfixDecrement _ ->
            Debug.todo "branch 'PostfixDecrement _' not implemented"


innerValue2 :
    Context
    -> Expr
    -> Expr
    -> (Context -> Value -> Value -> Result Error ( Context, Value ))
    -> Result Error ( Context, Value )
innerValue2 ctx l r k =
    innerValue ctx l
        |> Result.andThen
            (\( ctx2, vl ) ->
                innerValue ctx2 r
                    |> Result.andThen
                        (\( ctx3, vr ) ->
                            k ctx3 vl vr
                        )
            )


autovectorizingFloatOp : Context -> String -> (Float -> Float) -> Expr -> Result Error ( Dict String Value, Value )
autovectorizingFloatOp ctx name inner e =
    innerValue ctx e
        |> Result.andThen
            (\( ctx2, v ) ->
                case v of
                    VFloat fv ->
                        Ok ( ctx2, VFloat <| inner fv )

                    VVec2 x y ->
                        Ok ( ctx2, VVec2 (inner x) (inner y) )

                    VVec3 x y z ->
                        Ok ( ctx2, VVec3 (inner x) (inner y) (inner z) )

                    _ ->
                        Err <|
                            InvalidTypes
                                ("Cannot calculate `" ++ name ++ "` for " ++ valueToString v)
            )


interpret : Context -> Statement a -> Result Error ( Context, Value )
interpret ctx (Statement s) =
    innerInterpret ctx s.stat


innerInterpret : Context -> Stat -> Result Error ( Context, Value )
innerInterpret ctx stat =
    case stat of
        Return e ->
            e
                |> innerValue ctx
                |> Result.map (\( ctx2, val ) -> ( ctx2, val ))

        If c t n ->
            c
                |> innerValue ctx
                |> Result.andThen
                    (\( ctx2, cval ) ->
                        case cval of
                            VBool True ->
                                innerInterpret ctx2 t
                                    |> Result.andThen
                                        (\( ctx3, res ) ->
                                            case res of
                                                -- TODO: Fix this
                                                VVoid ->
                                                    innerInterpret ctx3 n

                                                _ ->
                                                    Ok ( ctx3, res )
                                        )

                            VBool False ->
                                innerInterpret ctx2 n

                            _ ->
                                Err <| InvalidTypes <| "Condition of if evaluated to " ++ Debug.toString cval
                    )

        IfElse _ _ _ _ ->
            Debug.todo "branch 'IfElse _' not implemented"

        Nop ->
            Ok ( ctx, VVoid )

        ExpressionStatement _ _ ->
            Debug.todo "branch 'ExpressionStatement _' not implemented"

        Decl _ _ _ _ ->
            Debug.todo "branch 'Decl _ _ _' not implemented"

        For _ _ _ _ _ _ _ ->
            Debug.todo "branch 'For _ _ _ _ _ _ _' not implemented"

        Break ->
            Debug.todo "branch 'Break' not implemented"

        Continue ->
            Debug.todo "branch 'Continue' not implemented"


valueToString : Value -> String
valueToString v =
    case v of
        VFloat f ->
            String.fromFloat f

        VBool b ->
            if b then
                "true"

            else
                "false"

        VInt i ->
            String.fromInt i

        VVoid ->
            "void"

        VVec2 x y ->
            "vec2(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"

        VVec3 x y z ->
            "vec3(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ "," ++ String.fromFloat z ++ ")"

        VMat3 _ _ _ ->
            Debug.todo "branch 'VMat3 _ _ _' not implemented"
