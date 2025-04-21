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

        BinaryOperation l And r ->
            innerValue2 ctx l r <| \ctx2 vl vr ->
            case ( vl, vr ) of
                ( VBool bl, VBool br ) ->
                    Ok ( ctx2, VBool <| bl && br )

                _ ->
                    Err <|
                        InvalidTypes
                            ("Cannot calculate `and` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        BinaryOperation l Or r ->
            innerValue2 ctx l r <| \ctx2 vl vr ->
            case ( vl, vr ) of
                ( VBool bl, VBool br ) ->
                    Ok ( ctx2, VBool <| bl || br )

                _ ->
                    Err <|
                        InvalidTypes
                            ("Cannot calculate `or` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        BinaryOperation l (RelationOperation k) r ->
            innerValue2 ctx l r <| \ctx2 vl vr ->
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

        BinaryOperation l By r ->
            innerValue2 ctx l r <| \ctx2 vl vr ->
            case ( vl, vr ) of
                ( VFloat fl, VFloat fr ) ->
                    Ok ( ctx2, VFloat <| fl * fr )

                _ ->
                    Err <|
                        InvalidTypes
                            ("Cannot calculate `*` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        Call (Variable "vec2") [ l, r ] ->
            innerValue2 ctx l r <| \ctx2 vl vr ->
            case ( vl, vr ) of
                ( VFloat fl, VFloat fr ) ->
                    Ok ( ctx2, VVec2 fl fr )

                _ ->
                    Err <|
                        InvalidTypes
                            ("Cannot calculate `vec2` for " ++ valueToString vl ++ " and " ++ valueToString vr)

        Call (Variable "exp") [ l ] ->
            autovectorizingFloatOp ctx "exp" (\fv -> Basics.e ^ fv) l

        Call (Variable "cos") [ l ] ->
            autovectorizingFloatOp ctx "cos" Basics.cos l

        Call (Variable "sin") [ l ] ->
            autovectorizingFloatOp ctx "sin" Basics.sin l

        Call name args ->
            Debug.todo <|
                "branch 'Call \""
                    ++ Debug.toString name
                    ++ "\" ["
                    ++ String.join ", " (List.map (Debug.toString >> (++) " ") args)
                    ++ " ]' not implemented"

        Dot _ _ ->
            Debug.todo "branch 'Dot _ _' not implemented"

        Uint _ ->
            Debug.todo "branch 'Uint _' not implemented"

        Double _ ->
            Debug.todo "branch 'Double _' not implemented"

        UnaryOperation PostfixIncrement _ ->
            Debug.todo "branch 'UnaryOperation PostfixIncrement _' not implemented"

        UnaryOperation PostfixDecrement _ ->
            Debug.todo "branch 'UnaryOperation PostfixDecrement _' not implemented"

        UnaryOperation PrefixIncrement _ ->
            Debug.todo "branch 'UnaryOperation PrefixIncrement _' not implemented"

        UnaryOperation PrefixDecrement _ ->
            Debug.todo "branch 'UnaryOperation PrefixDecrement _' not implemented"

        UnaryOperation Plus _ ->
            Debug.todo "branch 'UnaryOperation Plus _' not implemented"

        UnaryOperation Invert _ ->
            Debug.todo "branch 'UnaryOperation Invert _' not implemented"

        UnaryOperation Not _ ->
            Debug.todo "branch 'UnaryOperation Not _' not implemented"

        BinaryOperation _ ArraySubscript _ ->
            Debug.todo "branch 'BinaryOperation _ ArraySubscript _' not implemented"

        BinaryOperation _ Mod _ ->
            Debug.todo "branch 'BinaryOperation _ Mod _' not implemented"

        BinaryOperation _ ShiftLeft _ ->
            Debug.todo "branch 'BinaryOperation _ ShiftLeft _' not implemented"

        BinaryOperation _ ShiftRight _ ->
            Debug.todo "branch 'BinaryOperation _ ShiftRight _' not implemented"

        BinaryOperation _ BitwiseAnd _ ->
            Debug.todo "branch 'BinaryOperation _ BitwiseAnd _' not implemented"

        BinaryOperation _ BitwiseOr _ ->
            Debug.todo "branch 'BinaryOperation _ BitwiseOr _' not implemented"

        BinaryOperation _ BitwiseXor _ ->
            Debug.todo "branch 'BinaryOperation _ BitwiseXor _' not implemented"

        BinaryOperation _ Xor _ ->
            Debug.todo "branch 'BinaryOperation _ Xor _' not implemented"

        BinaryOperation _ Assign _ ->
            Debug.todo "branch 'BinaryOperation _ Assign _' not implemented"

        BinaryOperation _ ComboAdd _ ->
            Debug.todo "branch 'BinaryOperation _ ComboAdd _' not implemented"

        BinaryOperation _ ComboSubtract _ ->
            Debug.todo "branch 'BinaryOperation _ ComboSubtract _' not implemented"

        BinaryOperation _ ComboBy _ ->
            Debug.todo "branch 'BinaryOperation _ ComboBy _' not implemented"

        BinaryOperation _ ComboDiv _ ->
            Debug.todo "branch 'BinaryOperation _ ComboDiv _' not implemented"

        BinaryOperation _ ComboMod _ ->
            Debug.todo "branch 'BinaryOperation _ ComboMod _' not implemented"

        BinaryOperation _ ComboLeftShift _ ->
            Debug.todo "branch 'BinaryOperation _ ComboLeftShift _' not implemented"

        BinaryOperation _ ComboRightShift _ ->
            Debug.todo "branch 'BinaryOperation _ ComboRightShift _' not implemented"

        BinaryOperation _ ComboBitwiseAnd _ ->
            Debug.todo "branch 'BinaryOperation _ ComboBitwiseAnd _' not implemented"

        BinaryOperation _ ComboBitwiseXor _ ->
            Debug.todo "branch 'BinaryOperation _ ComboBitwiseXor _' not implemented"

        BinaryOperation _ ComboBitwiseOr _ ->
            Debug.todo "branch 'BinaryOperation _ ComboBitwiseOr _' not implemented"

        BinaryOperation _ Comma _ ->
            Debug.todo "branch 'BinaryOperation _ Comma _' not implemented"

        Ternary _ _ _ ->
            Debug.todo "branch 'Ternary _ _ _' not implemented"

        BinaryOperation _ Div _ ->
            Debug.todo "branch 'BinaryOperation _ Div _' not implemented"

        BinaryOperation _ Add _ ->
            Debug.todo "branch 'BinaryOperation _ Add _' not implemented"

        BinaryOperation _ Subtract _ ->
            Debug.todo "branch 'BinaryOperation _ Subtract _' not implemented"


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
            innerValue ctx e

        If c t ->
            c
                |> innerValue ctx
                |> Result.andThen
                    (\( ctx2, cval ) ->
                        case cval of
                            VBool True ->
                                innerInterpret ctx2 t

                            VBool False ->
                                Ok ( ctx2, VVoid )

                            _ ->
                                Err <| InvalidTypes <| "Condition of if evaluated to " ++ Debug.toString cval
                    )

        IfElse _ _ _ ->
            Debug.todo "branch 'IfElse _' not implemented"

        Nop ->
            Ok ( ctx, VVoid )

        ExpressionStatement _ ->
            Debug.todo "branch 'ExpressionStatement _' not implemented"

        Decl _ _ _ ->
            Debug.todo "branch 'Decl _ _ _' not implemented"

        For _ _ _ _ ->
            Debug.todo "branch 'For _ _ _ _' not implemented"

        Break ->
            Debug.todo "branch 'Break' not implemented"

        Continue ->
            Debug.todo "branch 'Continue' not implemented"

        Block _ _ _ ->
            Debug.todo "branch 'Block _ _ _' not implemented"


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
