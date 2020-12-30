module Expression.Solver exposing (solve)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), RelationOperation(..), SolutionTree(..), UnaryOperation(..))
import Expression.Simplify exposing (simplify)
import Expression.Utils exposing (by, div, i, ipow, minus, negate_, one, plus, pow, sqrt_, square, zero)
import List.Extra as List


solve : Expression -> Expression -> SolutionTree
solve e x =
    case x of
        Variable v ->
            simplify e
                |> innerSolve v
                |> SolutionStep e
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
            SolutionError "Cannot solve this"


solveEquation : String -> Expression -> Expression -> SolutionTree
solveEquation v l r =
    let
        go expr =
            case asPoly v expr |> Result.map (Dict.map (always simplify)) of
                Err err ->
                    SolutionError <| "Cannot solve " ++ Expression.toString expr ++ ", " ++ err

                Ok coeffs ->
                    SolutionStep (RelationOperation Equals expr zero)
                        (case Dict.toList coeffs |> List.reverse |> List.filterNot (Tuple.second >> isZero) of
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

                            _ ->
                                SolutionError <| "Cannot solve " ++ Expression.toString (RelationOperation Equals expr zero)
                        )
    in
    case ( l, r ) of
        ( Integer 0, _ ) ->
            go r

        ( _, Integer 0 ) ->
            go l

        _ ->
            go (minus l r)


isZero : Expression -> Bool
isZero k =
    case k of
        Integer i ->
            i == 0

        Float f ->
            f == 0

        _ ->
            False


neg : Expression -> Expression
neg b =
    case b of
        Integer q ->
            Integer -q

        UnaryOperation Negate q ->
            q

        _ ->
            negate_ b


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
                solIs v <| div (neg b) a
        in
        SolutionStep sol <|
            SolutionDone (simplify sol)


solve2 : String -> Expression -> Expression -> Expression -> SolutionTree
solve2 v a b c =
    if isZero a then
        solve1 v b c

    else if isZero b then
        let
            solPlus =
                sqrt_ (div (neg c) a)

            branch sol =
                SolutionStep (solIs v sol) <|
                    SolutionDone (solIs v <| simplify sol)
        in
        SolutionBranch
            [ branch solPlus
            , branch <| neg solPlus
            ]

    else
        let
            delta =
                minus (square b) (by [ Integer 4, a, c ])

            solPlus =
                div (plus [ neg b, sqrt_ delta ]) (by [ Integer 2, a ])

            solMinus =
                div (minus (neg b) (sqrt_ delta)) (by [ Integer 2, a ])

            branch sol =
                SolutionStep (solIs v sol) <|
                    SolutionDone (solIs v <| simplify sol)
        in
        SolutionBranch
            [ branch solPlus
            , branch solMinus
            ]


solve4Biquad : String -> Expression -> Expression -> Expression -> SolutionTree
solve4Biquad v a c e =
    if isZero a then
        solve2 v c zero e

    else if isZero c then
        let
            solPlus =
                if a == Integer 1 then
                    pow (neg e) (div one (Integer 4))

                else
                    pow (div (neg e) a) (div one (Integer 4))

            branch sol =
                SolutionStep (solIs v sol) <|
                    SolutionDone (solIs v <| simplify sol)
        in
        SolutionBranch
            [ branch solPlus
            , branch <| neg solPlus
            , branch <| by [ i, solPlus ]
            , branch <| by [ neg i, solPlus ]
            ]

    else
        let
            delta =
                minus (square c) (by [ Integer 4, a, e ])

            solPlus =
                sqrt_ <| div (plus [ neg c, sqrt_ delta ]) (by [ Integer 2, a ])

            solMinus =
                sqrt_ <| div (minus (neg c) (sqrt_ delta)) (by [ Integer 2, a ])

            branch sol =
                SolutionStep (solIs v sol) <|
                    SolutionDone (solIs v <| simplify sol)
        in
        SolutionBranch
            [ branch solPlus
            , branch solMinus
            , branch <| neg solPlus
            , branch <| neg solMinus
            ]


asPoly : String -> Expression -> Result String (Dict Int Expression)
asPoly v e =
    case e of
        AssociativeOperation Addition l m r ->
            List.foldl
                (Result.map2 <|
                    \el acc ->
                        Dict.merge
                            Dict.insert
                            (\k a b -> Dict.insert k <| plus [ a, b ])
                            Dict.insert
                            el
                            acc
                            Dict.empty
                )
                (Ok <| Dict.empty)
                (List.map (asPoly v) (l :: m :: r))

        AssociativeOperation Multiplication l m r ->
            (l :: m :: r)
                |> List.map (asPoly v >> Result.map Dict.toList)
                |> List.foldl
                    (Result.map2 <|
                        \el acc ->
                            acc
                                |> List.concatMap (\acc_e -> el |> List.map (Tuple.pair acc_e))
                                |> List.map (\( ( dl, cl ), ( dr, cr ) ) -> ( dl + dr, by [ cl, cr ] ))
                    )
                    (Ok [ ( 0, one ) ])
                |> Result.map
                    (\ls ->
                        ls
                            |> List.gatherEqualsBy Tuple.first
                            |> List.map (\( ( d, f ), ts ) -> ( d, plus <| f :: List.map Tuple.second ts ))
                            |> Dict.fromList
                    )

        Integer _ ->
            Ok <| Dict.singleton 0 e

        Float _ ->
            Ok <| Dict.singleton 0 e

        Variable w ->
            Ok <|
                if v == w then
                    Dict.singleton 1 one

                else
                    Dict.singleton 0 e

        UnaryOperation Negate inner ->
            inner
                |> asPoly v
                |> Result.map (Dict.map (always negate_))

        BinaryOperation Power b (Integer ex) ->
            if ex < 0 then
                Err ("Cannot extract coefficients from " ++ Expression.toString e)

            else if ex == 0 then
                Ok <| Dict.singleton 0 one

            else
                asPoly v <| by [ b, ipow b (ex - 1) ]

        _ ->
            Err ("Cannot extract coefficients from " ++ Expression.toString e)
