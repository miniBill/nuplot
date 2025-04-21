module Expression.NumericRange exposing (NumericRange(..), get, isCompletelyReal)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), UnaryOperation(..), filterContext)


type NumericRange
    = Positive
    | Nonnegative
    | Zero
    | Nonpositive
    | Negative
    | Real
    | Complex


isCompletelyReal : Expression -> Bool
isCompletelyReal =
    get >> (/=) Complex


get : Expression -> NumericRange
get =
    getRange (Dict.fromList [ ( "i", Complex ), ( "e", Positive ), ( "pi", Positive ) ])


getRange : Dict String NumericRange -> Expression -> NumericRange
getRange ctx e =
    case e of
        Variable v ->
            Dict.get v ctx |> Maybe.withDefault Real

        Integer j ->
            sign j

        Float j ->
            sign j

        UnaryOperation Negate c ->
            negateRange <| getRange ctx c

        BinaryOperation Power b ex ->
            powerRange ctx (getRange ctx b) ex

        BinaryOperation Division n d ->
            multiplyRange (getRange ctx n) (getRange ctx d)

        RelationOperation _ _ _ ->
            Nonnegative

        AssociativeOperation Addition l m o ->
            List.foldl addRange (getRange ctx l) <| List.map (getRange ctx) (m :: o)

        AssociativeOperation Multiplication l m o ->
            List.foldl multiplyRange (getRange ctx l) <| List.map (getRange ctx) (m :: o)

        List es ->
            case es of
                [] ->
                    Zero

                h :: t ->
                    List.foldl union (getRange ctx h) <| List.map (getRange ctx) t

        Apply (KnownFunction Abs) [ c ] ->
            case getRange ctx c of
                Positive ->
                    Positive

                Negative ->
                    Positive

                Zero ->
                    Zero

                _ ->
                    Nonnegative

        Apply (KnownFunction Arg) [ c ] ->
            case getRange ctx c of
                Complex ->
                    Real

                -- monotone with arg(0) = 0
                o ->
                    o

        Apply (KnownFunction Sqrt) [ c ] ->
            sqrtRange <| getRange ctx c

        Apply (KnownFunction Cbrt) [ c ] ->
            getRange ctx c

        Apply (KnownFunction Re) [ _ ] ->
            Real

        Apply (KnownFunction Im) [ _ ] ->
            Real

        Apply (KnownFunction Atan2) [ y, x ] ->
            if getRange ctx y /= Complex && getRange ctx x /= Complex then
                Real

            else
                Complex

        Apply (KnownFunction Piecewise) [ _, t, f ] ->
            union (getRange ctx t) (getRange ctx f)

        Apply (KnownFunction Mbrot) _ ->
            Complex

        Apply (KnownFunction _) args ->
            if List.any (\c -> getRange ctx c == Complex) args then
                Complex

            else
                Real

        Replace variables c ->
            getRange
                (List.foldl (\( k, v ) -> Dict.insert k (getRange ctx v))
                    ctx
                    (Dict.toList <| filterContext variables)
                )
                c

        Lambda x f ->
            getRange (Dict.remove x ctx) f


powerRange : Dict String NumericRange -> NumericRange -> Expression -> NumericRange
powerRange ctx brange ex =
    let
        default () =
            case ( brange, getRange ctx ex ) of
                ( _, Zero ) ->
                    Positive

                ( Zero, Positive ) ->
                    Zero

                ( Zero, Negative ) ->
                    Zero

                ( Zero, _ ) ->
                    Nonnegative

                ( Positive, ek ) ->
                    case ek of
                        Complex ->
                            Complex

                        Zero ->
                            Positive

                        -- Big negative or small positive exponent could round to zero
                        _ ->
                            Nonnegative

                ( Nonnegative, ek ) ->
                    if ek == Complex then
                        Complex

                    else
                        Nonnegative

                _ ->
                    Complex

        asInteger n =
            case n of
                Integer i ->
                    Just i

                Float f ->
                    let
                        rounded =
                            round f
                    in
                    if toFloat rounded == f then
                        Just rounded

                    else
                        Nothing

                _ ->
                    Nothing
    in
    case asInteger ex of
        Just j ->
            if abs j > 10 then
                default ()

            else if modBy 2 j == 1 then
                brange

            else if j == 0 then
                default ()

            else
                squareRange brange

        Nothing ->
            default ()


sqrtRange : NumericRange -> NumericRange
sqrtRange v =
    case v of
        Nonnegative ->
            Nonnegative

        Positive ->
            Positive

        Zero ->
            Zero

        _ ->
            Complex


squareRange : NumericRange -> NumericRange
squareRange v =
    case v of
        Positive ->
            Positive

        Nonnegative ->
            Nonnegative

        Zero ->
            Zero

        Nonpositive ->
            Nonnegative

        Negative ->
            Positive

        Real ->
            Nonnegative

        Complex ->
            Complex


negateRange : NumericRange -> NumericRange
negateRange range =
    case range of
        Positive ->
            Negative

        Nonnegative ->
            Nonpositive

        Zero ->
            Zero

        Nonpositive ->
            Nonnegative

        Negative ->
            Positive

        Real ->
            Real

        Complex ->
            Complex


addRange : NumericRange -> NumericRange -> NumericRange
addRange arg1 arg2 =
    if arg1 == arg2 then
        arg1

    else
        case ( arg1, arg2 ) of
            ( Zero, _ ) ->
                arg2

            ( _, Zero ) ->
                arg1

            ( Complex, _ ) ->
                Complex

            ( _, Complex ) ->
                Complex

            ( Positive, Nonnegative ) ->
                Positive

            ( Nonnegative, Positive ) ->
                Positive

            ( Nonpositive, Negative ) ->
                Negative

            ( Negative, Nonpositive ) ->
                Negative

            _ ->
                Real


multiplyRange : NumericRange -> NumericRange -> NumericRange
multiplyRange arg1 arg2 =
    case ( arg1, arg2 ) of
        ( Zero, _ ) ->
            Zero

        ( _, Zero ) ->
            Zero

        ( Complex, _ ) ->
            Complex

        ( _, Complex ) ->
            Complex

        ( Real, _ ) ->
            Real

        ( _, Real ) ->
            Real

        ( Positive, _ ) ->
            arg2

        ( _, Positive ) ->
            arg1

        ( Negative, _ ) ->
            negateRange arg2

        ( _, Negative ) ->
            negateRange arg1

        ( Nonnegative, _ ) ->
            union arg2 Zero

        ( _, Nonnegative ) ->
            union arg1 Zero

        ( Nonpositive, _ ) ->
            union (negateRange arg2) Zero


union : NumericRange -> NumericRange -> NumericRange
union arg1 arg2 =
    if arg1 == arg2 then
        arg1

    else
        case ( arg1, arg2 ) of
            ( Complex, _ ) ->
                Complex

            ( _, Complex ) ->
                Complex

            ( Positive, Zero ) ->
                Nonnegative

            ( Zero, Positive ) ->
                Nonnegative

            ( Negative, Zero ) ->
                Nonpositive

            ( Zero, Negative ) ->
                Nonpositive

            ( Nonnegative, Positive ) ->
                Nonnegative

            ( Positive, Nonnegative ) ->
                Nonnegative

            ( Nonpositive, Negative ) ->
                Nonpositive

            ( Negative, Nonpositive ) ->
                Nonpositive

            _ ->
                Real


sign : number -> NumericRange
sign j =
    if j == 0 then
        Zero

    else if j > 0 then
        Positive

    else
        Negative
