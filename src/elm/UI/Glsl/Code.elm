module UI.Glsl.Code exposing (expressionToGlsl, threshold)

import Dict
import Expression exposing (FunctionName(..), KnownFunction(..), PrintExpression(..), RelationOperation(..), toPrintExpression)
import Glsl exposing (vec211)
import Glsl.Helper exposing (Expression)
import UI.Glsl.Generator exposing (Vec2, abs2, add2, by, exp, expr, float, negate2, one, subtract, subtract2, vec2Zero, zero)


expressionToGlsl : List ( String, Expression Float ) -> Expression.Expression -> Expression Vec2
expressionToGlsl context =
    let
        ctx =
            Dict.fromList context

        variadic f es =
            case List.map go es of
                [] ->
                    vec2Zero

                head :: tail ->
                    List.foldl (\e a -> f a e) head tail

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
                            vec2Zero

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
                    cby (go l) (go r)

                PDiv l r ->
                    cdiv (go l) (go r)

                PPower (PVariable v) (PInteger 2) ->
                    case Dict.get v ctx of
                        Just w ->
                            vec2 (by w w) zero

                        Nothing ->
                            vec2Zero

                PPower l (PInteger 2) ->
                    csquare (go l)

                PPower l r ->
                    cpow (go l) (go r)

                PApply (KnownFunction Simplify) [ e ] ->
                    go e

                PApply (KnownFunction Sin) [ e ] ->
                    csin (go e)

                PApply (KnownFunction Cos) [ e ] ->
                    ccos (go e)

                PApply (KnownFunction Tan) [ e ] ->
                    ctan (go e)

                PApply (KnownFunction Asin) [ e ] ->
                    casin (go e)

                PApply (KnownFunction Acos) [ e ] ->
                    cacos (go e)

                PApply (KnownFunction Atan) [ e ] ->
                    catan (go e)

                PApply (KnownFunction Atan2) [ l, r ] ->
                    catan2 (go l) (go r)

                PApply (KnownFunction Sinh) [ e ] ->
                    csinh (go e)

                PApply (KnownFunction Cosh) [ e ] ->
                    ccosh (go e)

                PApply (KnownFunction Tanh) [ e ] ->
                    ctanh (go e)

                PApply (KnownFunction Abs) [ e ] ->
                    cabs (go e)

                PApply (KnownFunction Ln) [ e ] ->
                    cln (go e)

                PApply (KnownFunction Log10) [ e ] ->
                    clog10 (go e)

                PApply (KnownFunction Exp) [ e ] ->
                    cexp (go e)

                PApply (KnownFunction Sign) [ e ] ->
                    csign (go e)

                PApply (KnownFunction Re) [ e ] ->
                    cre (go e)

                PApply (KnownFunction Im) [ e ] ->
                    cim (go e)

                PApply (KnownFunction Arg) [ e ] ->
                    carg (go e)

                PApply (KnownFunction Round) [ e ] ->
                    cround (go e)

                PApply (KnownFunction Floor) [ e ] ->
                    cfloor (go e)

                PApply (KnownFunction Ceiling) [ e ] ->
                    cceiling (go e)

                PApply (KnownFunction Pw) [ c, t, f ] ->
                    cpw (go c) (go t) (go f)

                PApply (KnownFunction Mod) [ l, r ] ->
                    cmod (go l) (go r)

                PApply (KnownFunction Mbrot) [ x, y ] ->
                    cmbrot (go x) (go y)

                PApply (KnownFunction Min) es ->
                    variadic cmin es

                PApply (KnownFunction Max) es ->
                    variadic cmax es

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
