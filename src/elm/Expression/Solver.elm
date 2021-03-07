module Expression.Solver exposing (solve, stepSimplify)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), RelationOperation(..), SolutionTree(..), UnaryOperation(..))
import Expression.Polynomial exposing (Exponents, Polynomial, asPolynomial)
import Expression.Simplify exposing (simplify, stepSimplify)
import Expression.Utils exposing (by, byShort, cbrt, div, divShort, double, factor, i, ipow, ipowShort, isZero, minus, negateShort, negate_, one, plus, plusShort, pow, sqrt_, square, zero)
import Fraction exposing (Fraction, gcd)
import List.Extra as List
import Result
import Result.Extra as Result


solve : Expression -> Expression -> SolutionTree
solve e x =
    case x of
        Variable v ->
            e
                |> stepSimplify
                    (\s ->
                        case s of
                            RelationOperation Equals l (BinaryOperation Division n d) ->
                                stepSimplify (innerSolve v) <| RelationOperation Equals (by [ l, d ]) n

                            RelationOperation Equals (BinaryOperation Division n d) r ->
                                stepSimplify (innerSolve v) <| RelationOperation Equals n (by [ r, d ])

                            _ ->
                                innerSolve v s
                    )
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
            case asPolynomial [ v ] expr |> Maybe.map (Dict.map (always simplify)) of
                Nothing ->
                    SolutionError <| "Cannot solve " ++ Expression.toString expr

                Just coeffs ->
                    let
                        rebuilt =
                            coeffsToEq coeffs

                        cleanCoeffs =
                            Dict.toList coeffs
                                |> List.reverse
                                |> List.filterNot (Tuple.second >> isZero)

                        get k =
                            cleanCoeffs
                                |> List.find (\( d, _ ) -> d == [ ( v, k ) ])
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault zero

                        default () =
                            case findRationalRoot v (Dict.fromList cleanCoeffs) of
                                Ok { root, rest } ->
                                    SolutionBranch
                                        [ innerSolve v <| RelationOperation Equals (Variable v) root
                                        , innerSolve v <| RelationOperation Equals rest zero
                                        ]

                                Err e ->
                                    SolutionError <| "Cannot solve " ++ Expression.toString rebuilt ++ ", " ++ e

                        last =
                            case List.head cleanCoeffs of
                                Nothing ->
                                    solve0 v zero

                                Just ( [], k ) ->
                                    solve0 v k

                                Just ( [ ( _, 1 ) ], a ) ->
                                    solve1 v a (get 0)

                                Just ( [ ( _, 2 ) ], a ) ->
                                    solve2 v a (get 1) (get 0)

                                Just ( [ ( _, 3 ) ], a ) ->
                                    solve3 v a (get 2) (get 1) (get 0)

                                Just ( [ ( _, 4 ) ], a ) ->
                                    if isZero (get 1) && isZero (get 3) then
                                        solve4Biquad v a (get 2) (get 0)

                                    else
                                        default ()

                                _ ->
                                    default ()
                    in
                    SolutionStep (RelationOperation Equals expr zero) <|
                        SolutionStep rebuilt <|
                            last
    in
    case ( l, r ) of
        ( Integer 0, _ ) ->
            go r

        ( _, Integer 0 ) ->
            go l

        _ ->
            go (minus l r)


findRationalRoot : String -> Polynomial -> Result String { root : Expression, rest : Expression }
findRationalRoot v coeffs =
    let
        findRoot : List ( Exponents, Fraction ) -> Result String { root : Expression, rest : Expression }
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
                        |> List.find (\( d, _ ) -> List.isEmpty d)
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault 0

                potentialsN =
                    allDivisors known

                head =
                    reduced
                        |> List.sortBy
                            (\( d, _ ) ->
                                d
                                    |> List.map Tuple.second
                                    |> List.sum
                                    |> negate
                            )
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
                        |> List.map (\( f, m ) -> List.initialize (m + 1) (\exp -> f ^ exp))
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
                        |> List.sortBy Fraction.toFloat

                res =
                    potentials
                        |> List.filterMap tryDivision
                        |> List.head

                reducedDict =
                    Dict.fromList reduced

                maximumDegree =
                    Dict.keys reducedDict
                        |> List.map (List.map Tuple.second >> List.sum)
                        |> List.maximum
                        |> Maybe.withDefault 0

                getCoeff d =
                    Fraction.fromInt <| Maybe.withDefault 0 <| Dict.get [ ( v, d ) ] reducedDict

                tryDivision potential =
                    List.range 0 (maximumDegree - 1)
                        |> List.foldr
                            (\degree ( r, a ) ->
                                let
                                    k =
                                        getCoeff degree

                                    r_ =
                                        Fraction.plus k (Fraction.by r potential)
                                in
                                ( r_, r :: a )
                            )
                            ( getCoeff maximumDegree, [] )
                        |> (\( r, c ) ->
                                if Fraction.isZero r then
                                    Just <| rootAndRest potential c

                                else
                                    Nothing
                           )
            in
            res
                |> Result.fromMaybe "no rational root found, and no simple formula is usable"

        rootAndRest potential c =
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

                rest =
                    c
                        |> List.indexedMap
                            (\exponent k ->
                                let
                                    ke =
                                        toExpr k
                                in
                                if Fraction.isZero k then
                                    Nothing

                                else
                                    Just <|
                                        case exponent of
                                            0 ->
                                                toExpr k

                                            1 ->
                                                if ke == Integer 1 then
                                                    Variable v

                                                else
                                                    by [ toExpr k, Variable v ]

                                            _ ->
                                                if ke == Integer 1 then
                                                    ipow (Variable v) exponent

                                                else
                                                    by [ toExpr k, ipow (Variable v) exponent ]
                            )
                        |> List.filterMap identity
                        |> List.reverse
                        |> plus
            in
            { root = toExpr potential
            , rest = rest
            }
    in
    coeffs
        |> Dict.toList
        |> List.map (\( d, e ) -> Result.map (Tuple.pair d) (asFraction e))
        |> Result.combine
        |> Result.andThen findRoot


asFraction : Expression -> Result String Fraction
asFraction e =
    case e of
        Integer i ->
            Ok <| Fraction.fromInt i

        BinaryOperation Division n d ->
            Result.map2 Fraction.div
                (asFraction n)
                (asFraction d)

        AssociativeOperation Addition l m r ->
            List.foldr (Result.map2 Fraction.plus) (asFraction l) <| List.map asFraction (m :: r)

        AssociativeOperation Multiplication l m r ->
            List.foldr (Result.map2 Fraction.by) (asFraction l) <| List.map asFraction (m :: r)

        UnaryOperation Negate inner ->
            asFraction inner
                |> Result.map Fraction.negate

        _ ->
            Err <| Expression.toString e ++ " is not a fraction"


coeffsToEq : Polynomial -> Expression
coeffsToEq coeffs =
    coeffs
        |> Dict.toList
        |> List.reverse
        |> List.filterMap
            (\( exps, c ) ->
                if List.isEmpty exps then
                    if isZero c then
                        Nothing

                    else
                        Just c

                else
                    let
                        pows =
                            byShort <| List.map (\( v, e ) -> ipowShort (Variable v) e) exps
                    in
                    if isZero c then
                        Nothing

                    else
                        case c of
                            Integer i ->
                                if i == 1 then
                                    Just pows

                                else if i == -1 then
                                    Just <| negate_ pows

                                else
                                    Just <| by [ c, pows ]

                            Float f ->
                                if f == 1 then
                                    Just pows

                                else if f == -1 then
                                    Just <| negate_ pows

                                else
                                    Just <| by [ c, pows ]

                            _ ->
                                Just <| by [ c, pows ]
            )
        |> plus
        |> (\l -> RelationOperation Equals l zero)


solutionBranch : String -> Expression -> SolutionTree
solutionBranch v sol =
    stepSimplify SolutionDone (solIs v sol)


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
        solutionBranch v <| divShort (negateShort b) a


solve2 : String -> Expression -> Expression -> Expression -> SolutionTree
solve2 v a b c =
    if isZero a then
        solve1 v b c

    else if isZero b then
        let
            solPlus =
                sqrt_ (divShort (negateShort c) a)
        in
        SolutionBranch
            [ solutionBranch v <| negateShort solPlus
            , solutionBranch v solPlus
            ]

    else
        let
            delta =
                minus (square b) (byShort [ Integer 4, a, c ])

            solPlus =
                divShort (plusShort [ negateShort b, sqrt_ delta ]) (byShort [ Integer 2, a ])

            solMinus =
                divShort (minus (negateShort b) (sqrt_ delta)) (byShort [ Integer 2, a ])
        in
        SolutionBranch
            [ solutionBranch v solMinus
            , solutionBranch v solPlus
            ]


solve3 : String -> Expression -> Expression -> Expression -> Expression -> SolutionTree
solve3 v a b c d =
    if isZero a then
        solve2 v b c d

    else if isZero b && isZero c then
        let
            solPlus =
                cbrt (negate_ d)

            xi =
                div (plus [ Integer -1, sqrt_ (Integer -3) ]) (Integer 2)

            xiTwo =
                div (minus (Integer -1) (sqrt_ (Integer -3))) (Integer 2)
        in
        SolutionBranch
            [ solutionBranch v solPlus
            , solutionBranch v <| byShort [ xi, solPlus ]
            , solutionBranch v <| byShort [ xiTwo, solPlus ]
            ]

    else
        let
            deltaZero =
                minus (square b) (by [ Integer 3, a, c ])

            deltaOne =
                plus [ double <| ipow b 3, negate_ <| by [ Integer 9, a, b, c ], by [ Integer 27, square a, d ] ]

            bigC =
                cbrt <|
                    div
                        (plus [ deltaOne, sqrt_ <| minus (square deltaOne) (by [ Integer 4, ipow deltaZero 3 ]) ])
                        (Integer 2)

            xi =
                div (plus [ Integer -1, sqrt_ (Integer -3) ]) (Integer 2)

            xiTwo =
                div (minus (Integer -1) (sqrt_ (Integer -3))) (Integer 2)

            sol k =
                let
                    xiC =
                        byShort [ k, bigC ]
                in
                by
                    [ div
                        (Integer -1)
                        (by [ Integer 3, a ])
                    , plus
                        [ b
                        , xiC
                        , div deltaZero xiC
                        ]
                    ]
        in
        SolutionBranch
            [ solutionBranch v <| sol one
            , solutionBranch v <| sol xi
            , solutionBranch v <| sol xiTwo
            ]


solve4Biquad : String -> Expression -> Expression -> Expression -> SolutionTree
solve4Biquad v a c e =
    if isZero a then
        solve2 v c zero e

    else if isZero c then
        let
            solPlus =
                pow (divShort (negateShort e) a) (div one (Integer 4))

            branch =
                solutionBranch v
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
