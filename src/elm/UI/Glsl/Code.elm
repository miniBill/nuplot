module UI.Glsl.Code exposing (expressionToGlsl, threshold)

import Dict
import Expression exposing (FunctionName(..), KnownFunction(..), PrintExpression(..), RelationOperation(..), toPrintExpression)
import Glsl exposing (cabs2, cacos2, carg2, casin2, catan2, catan222, cby22, cceil2, ccos2, ccosh2, cdiv22, cexp2, cfloor2, cim2, cln2, clog102, cmax22, cmbrot22, cmin22, cmod22, cpow22, cpw222, cre2, cround2, csign2, csin2, csinh2, csquare2, ctan2, ctanh2, vec21, vec211)
import Glsl.Helper exposing (Expression)
import UI.Glsl.Generator exposing (Vec2, abs2, add2, by, exp, expr, float, negate2, one, subtract, subtract2, vec2Zero, zero)


expressionToGlsl : List ( String, Expression Float ) -> Expression.Expression -> Expression Vec2
expressionToGlsl context =
    let
        ctx : Dict.Dict String (Expression Float)
        ctx =
            Dict.fromList context

        variadic : (Expression Vec2 -> Expression Vec2 -> Expression Vec2) -> List PrintExpression -> Expression Vec2
        variadic f es =
            case List.map go es of
                [] ->
                    vec2Zero

                head :: tail ->
                    List.foldl (\e a -> f a e) head tail

        go : PrintExpression -> Expression Vec2
        go expr =
            case expr of
                PVariable "i" ->
                    vec211 zero one

                PVariable "pi" ->
                    vec211 constants.pi zero

                PVariable "e" ->
                    vec211 (exp one) zero

                PVariable v ->
                    case Dict.get v ctx of
                        Just w ->
                            vec211 w zero

                        Nothing ->
                            vec21 zero

                PInteger v ->
                    vec211 (float <| toFloat v) zero

                PFloat f ->
                    vec211 (float f) zero

                PNegate expression ->
                    negate2 <| go expression

                PAdd l (PNegate r) ->
                    subtract2 (go l) (go r)

                PAdd l r ->
                    add2 (go l) (go r)

                PRel op l r ->
                    case op of
                        LessThan ->
                            subtract2 (go r) (go l)

                        LessThanOrEquals ->
                            subtract2 (go r) (go l)

                        NotEquals ->
                            abs2 <| subtract (go r) (go l)

                        Equals ->
                            abs2 <| subtract (go r) (go l)

                        GreaterThanOrEquals ->
                            subtract2 (go l) (go r)

                        GreaterThan ->
                            subtract2 (go l) (go r)

                PBy l r ->
                    cby22 (go l) (go r)

                PDiv l r ->
                    cdiv22 (go l) (go r)

                PPower (PVariable v) (PInteger 2) ->
                    case Dict.get v ctx of
                        Just w ->
                            vec211 (by w w) zero

                        Nothing ->
                            vec2Zero

                PPower l (PInteger 2) ->
                    csquare2 (go l)

                PPower l r ->
                    cpow22 (go l) (go r)

                PApply (KnownFunction Simplify) [ e ] ->
                    go e

                PApply (KnownFunction Sin) [ e ] ->
                    csin2 (go e)

                PApply (KnownFunction Cos) [ e ] ->
                    ccos2 (go e)

                PApply (KnownFunction Tan) [ e ] ->
                    ctan2 (go e)

                PApply (KnownFunction Asin) [ e ] ->
                    casin2 (go e)

                PApply (KnownFunction Acos) [ e ] ->
                    cacos2 (go e)

                PApply (KnownFunction Atan) [ e ] ->
                    catan2 (go e)

                PApply (KnownFunction Atan2) [ l, r ] ->
                    catan222 (go l) (go r)

                PApply (KnownFunction Sinh) [ e ] ->
                    csinh2 (go e)

                PApply (KnownFunction Cosh) [ e ] ->
                    ccosh2 (go e)

                PApply (KnownFunction Tanh) [ e ] ->
                    ctanh2 (go e)

                PApply (KnownFunction Abs) [ e ] ->
                    cabs2 (go e)

                PApply (KnownFunction Ln) [ e ] ->
                    cln2 (go e)

                PApply (KnownFunction Log10) [ e ] ->
                    clog102 (go e)

                PApply (KnownFunction Exp) [ e ] ->
                    cexp2 (go e)

                PApply (KnownFunction Sign) [ e ] ->
                    csign2 (go e)

                PApply (KnownFunction Re) [ e ] ->
                    cre2 (go e)

                PApply (KnownFunction Im) [ e ] ->
                    cim2 (go e)

                PApply (KnownFunction Arg) [ e ] ->
                    carg2 (go e)

                PApply (KnownFunction Round) [ e ] ->
                    cround2 (go e)

                PApply (KnownFunction Floor) [ e ] ->
                    cfloor2 (go e)

                PApply (KnownFunction Ceil) [ e ] ->
                    cceil2 (go e)

                PApply (KnownFunction Pw) [ c, t, f ] ->
                    cpw222 (go c) (go t) (go f)

                PApply (KnownFunction Mod) [ l, r ] ->
                    cmod22 (go l) (go r)

                PApply (KnownFunction Mbrot) [ x, y ] ->
                    cmbrot22 (go x) (go y)

                PApply (KnownFunction Min) es ->
                    variadic cmin22 es

                PApply (KnownFunction Max) es ->
                    variadic cmax22 es

                PReplace var e ->
                    go (Expression.pfullSubstitute var e)

                -- If this happens, it's too late
                PApply _ _ ->
                    vec2Zero

                PList _ ->
                    vec2Zero

                PLambda _ _ ->
                    vec2Zero
    in
    toPrintExpression >> go


threshold : Expression Float -> Expression Float
threshold max_distance =
    by (float 0.000001) max_distance
