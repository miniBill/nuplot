module Expression.Solver exposing (solve)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), RelationOperation(..), SolutionTree(..), UnaryOperation(..))
import Expression.Simplify exposing (simplify, stepSimplify)
import Expression.Utils exposing (asPoly, by, byShort, div, divShort, factor, i, ipow, isZero, minus, negateShort, negate_, one, plus, plusShort, pow, sqrt_, square, zero)
import Fraction exposing (gcd)
import List.Extra as List
import Result
import Result.Extra as Result


solve : Expression -> Expression -> SolutionTree
solve e x =
    case x of
        Variable v ->
            e
                |> stepSimplify (innerSolve v)
                |> cut

        _ ->
            SolutionError "The second argument of `solve` must be the variable you want to solve for"


type Shortcut
    = Shortcut SolutionTree (Dict String Shortcut)


cut : SolutionTree -> SolutionTree
cut =
    let
        go tree =
            case tree of
                SolutionError _ ->
                    ( Dict.empty, tree )

                SolutionNone _ ->
                    ( Dict.empty, tree )

                SolutionForall _ ->
                    ( Dict.empty, tree )

                SolutionDone e ->
                    ( Dict.singleton (Expression.toString e) (Shortcut tree Dict.empty), tree )

                SolutionStep e n ->
                    let
                        ( shortcuts, ncut ) =
                            go n

                        key =
                            Expression.toString e
                    in
                    case Dict.get key shortcuts of
                        Nothing ->
                            let
                                res =
                                    SolutionStep e ncut
                            in
                            ( Dict.insert key (Shortcut res shortcuts) shortcuts, res )

                        Just (Shortcut shortTree lessShortcuts) ->
                            ( Dict.insert key (Shortcut shortTree shortcuts) lessShortcuts, shortTree )

                SolutionBranch ls ->
                    ( Dict.empty, SolutionBranch <| List.map cut ls )
    in
    go >> Tuple.second


innerSolve : String -> Expression -> SolutionTree
innerSolve v expr =
    case expr of
        RelationOperation Equals l r ->
            SolutionStep expr <| solveEquation v l r

        RelationOperation _ _ _ ->
            SolutionError "Cannot solve disequations [yet!]"

        _ ->
            innerSolve v <| RelationOperation Equals expr zero


solveEquation : String -> Expression -> Expression -> SolutionTree
solveEquation v l r =
    let
        go expr =
            case asPoly v expr |> Result.map (Dict.map (always simplify)) of
                Err err ->
                    SolutionError <| "Cannot solve " ++ Expression.toString expr ++ ", " ++ err

                Ok coeffs ->
                    SolutionStep (RelationOperation Equals expr zero) <|
                        SolutionStep (coeffsToEq v coeffs) <|
                            case Dict.toList coeffs |> List.reverse |> List.filterNot (Tuple.second >> isZero) of
                                [] ->
                                    solve0 v zero

                                [ ( 0, k ) ] ->
                                    solve0 v k

                                [ ( 1, a ) ] ->
                                    solve1 v a zero

                                [ ( 1, a ), ( 0, b ) ] ->
                                    solve1 v a b

                                [ ( 2, a ) ] ->
                                    solve2 v a zero zero

                                [ ( 2, a ), ( 1, b ) ] ->
                                    solve2 v a b zero

                                [ ( 2, a ), ( 0, c ) ] ->
                                    solve2 v a zero c

                                [ ( 2, a ), ( 1, b ), ( 0, c ) ] ->
                                    solve2 v a b c

                                [ ( 4, a ) ] ->
                                    solve4Biquad v a zero zero

                                [ ( 4, a ), ( 0, e ) ] ->
                                    solve4Biquad v a zero e

                                [ ( 4, a ), ( 2, c ), ( 0, e ) ] ->
                                    solve4Biquad v a c e

                                [ ( 4, a ), ( 2, c ) ] ->
                                    solve4Biquad v a c zero

                                cleanCoeffs ->
                                    case findRationalRoot v cleanCoeffs of
                                        Ok { root, rest } ->
                                            SolutionBranch
                                                [ innerSolve v <| RelationOperation Equals (Variable v) root
                                                , innerSolve v <| RelationOperation Equals rest zero
                                                ]

                                        Err e ->
                                            SolutionError <| "Cannot solve " ++ Expression.toString (coeffsToEq v coeffs) ++ ", " ++ e
    in
    case ( l, r ) of
        ( Integer 0, _ ) ->
            go r

        ( _, Integer 0 ) ->
            go l

        _ ->
            go (minus l r)


findRationalRoot : String -> List ( Int, Expression ) -> Result String { root : Expression, rest : Expression }
findRationalRoot v coeffs =
    let
        asFraction e =
            case e of
                Integer i ->
                    Ok <| Fraction.fromInt i

                BinaryOperation Division n d ->
                    Result.map2 Fraction.div
                        (asFraction n)
                        (asFraction d)

                AssociativeOperation Addition l m r ->
                    List.foldl (Result.map2 Fraction.plus) (asFraction l) <| List.map asFraction (m :: r)

                AssociativeOperation Multiplication l m r ->
                    List.foldl (Result.map2 Fraction.by) (asFraction l) <| List.map asFraction (m :: r)

                UnaryOperation Negate inner ->
                    asFraction inner
                        |> Result.map Fraction.negate

                _ ->
                    Err <| Expression.toString e ++ " is not a fraction"

        maybeRats =
            coeffs
                |> List.map (\( d, e ) -> Result.map (Tuple.pair d) (asFraction e))
                |> Result.combine

        findRoot rats =
            let
                commonMultiple =
                    rats
                        |> List.map Tuple.second
                        |> List.foldl (\f a -> abs <| Fraction.divisor f * a // gcd (Fraction.divisor f) a) 1

                reduced =
                    rats
                        |> List.map
                            (Tuple.mapSecond
                                (Fraction.toPair
                                    >> (\( n, d ) ->
                                            n * commonMultiple // d
                                       )
                                )
                            )

                known =
                    reduced
                        |> List.find (\( d, _ ) -> d == 0)
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault 0

                potentialsN =
                    allDivisors known

                head =
                    reduced
                        |> List.sortBy (\( d, _ ) -> -d)
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault 1

                potentialsD =
                    allDivisors head

                allDivisors n =
                    n
                        |> abs
                        |> factor
                        |> (::) ( 1, 0 )
                        |> List.map (\( f, m ) -> List.initialize (m + 1) (\e -> f ^ e))
                        |> List.cartesianProduct
                        |> List.map List.product

                potentials =
                    potentialsN
                        |> List.concatMap
                            (\n ->
                                List.map (Fraction.build n) potentialsD
                            )
                        |> List.concatMap (\f -> [ f, Fraction.negate f ])
                        |> List.uniqueBy Fraction.toPair
                        |> List.sortBy fractionToFloat

                fractionToFloat f =
                    let
                        ( n, d ) =
                            Fraction.toPair f
                    in
                    toFloat n / toFloat d

                res =
                    potentials
                        |> List.filterMap tryDivision
                        |> List.head

                reducedDict =
                    Dict.fromList reduced

                degree =
                    Dict.keys reducedDict |> List.maximum |> Maybe.withDefault 0

                getCoeff d =
                    Fraction.fromInt <| Maybe.withDefault 0 <| Dict.get d reducedDict

                tryDivision potential =
                    List.range 0 (degree - 1)
                        |> List.foldr
                            (\d ( r, a ) ->
                                let
                                    k =
                                        getCoeff d

                                    r_ =
                                        Fraction.plus k (Fraction.by r potential)
                                in
                                ( r_, r :: a )
                            )
                            ( getCoeff degree, [] )
                        |> (\( r, c ) ->
                                if Fraction.isZero r then
                                    let
                                        toExpr f =
                                            let
                                                ( n, d ) =
                                                    Fraction.toPair f
                                            in
                                            if d == 1 then
                                                Integer n

                                            else if n == 0 then
                                                Integer 0

                                            else
                                                div (Integer n) (Integer d)
                                    in
                                    Just
                                        { root = toExpr potential
                                        , rest =
                                            c
                                                |> List.indexedMap
                                                    (\e k ->
                                                        let
                                                            ke =
                                                                toExpr k
                                                        in
                                                        if Fraction.isZero k then
                                                            Nothing

                                                        else
                                                            Just <|
                                                                case e of
                                                                    0 ->
                                                                        toExpr k

                                                                    1 ->
                                                                        if ke == Integer 1 then
                                                                            Variable v

                                                                        else
                                                                            by [ toExpr k, Variable v ]

                                                                    _ ->
                                                                        if ke == Integer 1 then
                                                                            ipow (Variable v) e

                                                                        else
                                                                            by [ toExpr k, ipow (Variable v) e ]
                                                    )
                                                |> List.filterMap identity
                                                |> List.reverse
                                                |> plus
                                        }

                                else
                                    Nothing
                           )
            in
            res
                |> Result.fromMaybe "no rational root found, and no simple formula is usable"
    in
    maybeRats |> Result.andThen findRoot


coeffsToEq : String -> Dict Int Expression -> Expression
coeffsToEq v coeffs =
    coeffs
        |> Dict.toList
        |> List.reverse
        |> List.filterMap
            (\( e, c ) ->
                if e == 0 then
                    if isZero c then
                        Nothing

                    else
                        Just c

                else
                    let
                        p =
                            if e == 1 then
                                Variable v

                            else
                                ipow (Variable v) e
                    in
                    if isZero c then
                        Nothing

                    else
                        case c of
                            Integer i ->
                                if i == 1 then
                                    Just p

                                else if i == -1 then
                                    Just <| negate_ p

                                else
                                    Just <| by [ c, p ]

                            Float f ->
                                if f == 1 then
                                    Just p

                                else if f == -1 then
                                    Just <| negate_ p

                                else
                                    Just <| by [ c, p ]

                            _ ->
                                Just <| by [ c, p ]
            )
        |> plus
        |> (\l -> RelationOperation Equals l zero)


solIs : String -> Expression -> Expression
solIs =
    RelationOperation Equals << Variable


solve0 : String -> Expression -> SolutionTree
solve0 v k =
    if isZero k then
        SolutionForall v

    else
        SolutionNone v


solve1 : String -> Expression -> Expression -> SolutionTree
solve1 v a b =
    if isZero a then
        solve0 v b

    else
        let
            sol =
                solIs v <| divShort (negateShort b) a
        in
        stepSimplify SolutionDone sol


stepSimplify : (Expression -> SolutionTree) -> Expression -> SolutionTree
stepSimplify final =
    let
        go i e =
            if i <= 0 then
                final e

            else
                let
                    next =
                        Expression.Simplify.stepSimplify Dict.empty e
                in
                if Expression.equals next e then
                    final e

                else
                    SolutionStep e <| go (i - 1) next
    in
    go 100


solve2 : String -> Expression -> Expression -> Expression -> SolutionTree
solve2 v a b c =
    if isZero a then
        solve1 v b c

    else if isZero b then
        let
            solPlus =
                sqrt_ (divShort (negateShort c) a)

            branch sol =
                stepSimplify SolutionDone (solIs v sol)
        in
        SolutionBranch
            [ branch <| negateShort solPlus
            , branch solPlus
            ]

    else
        let
            delta =
                minus (square b) (byShort [ Integer 4, a, c ])

            solPlus =
                divShort (plusShort [ negateShort b, sqrt_ delta ]) (byShort [ Integer 2, a ])

            solMinus =
                divShort (minus (negateShort b) (sqrt_ delta)) (byShort [ Integer 2, a ])

            branch sol =
                stepSimplify SolutionDone (solIs v sol)
        in
        SolutionBranch
            [ branch solMinus
            , branch solPlus
            ]


solve4Biquad : String -> Expression -> Expression -> Expression -> SolutionTree
solve4Biquad v a c e =
    if isZero a then
        solve2 v c zero e

    else if isZero c then
        let
            solPlus =
                pow (divShort (negateShort e) a) (div one (Integer 4))

            branch sol =
                stepSimplify SolutionDone (solIs v sol)
        in
        SolutionBranch
            [ branch solPlus
            , branch <| negateShort solPlus
            , branch <| byShort [ i, solPlus ]
            , branch <| byShort [ negateShort i, solPlus ]
            ]

    else
        let
            delta =
                minus (square c) (byShort [ Integer 4, a, e ])

            solPlus =
                sqrt_ <| div (plus [ negateShort c, sqrt_ delta ]) (byShort [ Integer 2, a ])

            solMinus =
                sqrt_ <| div (minus (negateShort c) (sqrt_ delta)) (byShort [ Integer 2, a ])

            branch sol =
                stepSimplify SolutionDone (solIs v sol)
        in
        SolutionBranch
            [ branch solPlus
            , branch solMinus
            , branch <| negateShort solPlus
            , branch <| negateShort solMinus
            ]
