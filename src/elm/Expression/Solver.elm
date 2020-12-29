module Expression.Solver exposing (solve)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), RelationOperation(..), SolutionTree(..), UnaryOperation(..))
import Expression.Simplify exposing (simplify)
import Expression.Utils exposing (by, div, ipow, minus, negate_, one, plus, sqrt_, square, zero)
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


cut : SolutionTree -> SolutionTree
cut =
    let
        go shortcuts tree =
            case tree of
                SolutionError _ ->
                    tree

                _ ->
                    let
                        _ =
                            Debug.log "TODO: cut" tree
                    in
                    tree
    in
    go Dict.empty


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
        eq =
            RelationOperation Equals

        eqv =
            RelationOperation Equals (Variable v)

        neg b =
            case b of
                Integer q ->
                    Integer -q

                UnaryOperation Negate q ->
                    q

                _ ->
                    negate_ b

        go e =
            case asPoly v e |> Result.map (Dict.map (always simplify)) of
                Err err ->
                    SolutionError <| "Cannot solve " ++ Expression.toString e ++ ", " ++ err

                Ok coeffs ->
                    let
                        isZero k =
                            case k of
                                Integer i ->
                                    i == 0

                                Float f ->
                                    f == 0

                                _ ->
                                    False

                        solve0 k =
                            if isZero k then
                                SolutionForall v

                            else
                                SolutionNone v

                        solve1 a b =
                            if isZero a then
                                solve0 b

                            else
                                let
                                    sol =
                                        eqv <| div (neg b) a
                                in
                                SolutionStep sol <|
                                    SolutionDone (simplify sol)

                        solve2 a b c =
                            if isZero a then
                                solve1 b c

                            else if isZero b then
                                let
                                    solPlus =
                                        sqrt_ (div (neg c) a)

                                    branch sol =
                                        SolutionStep (eqv sol) <|
                                            SolutionDone (eqv <| simplify sol)
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
                                        SolutionStep (eqv sol) <|
                                            SolutionDone (eqv <| simplify sol)
                                in
                                SolutionBranch
                                    [ branch solPlus
                                    , branch solMinus
                                    ]
                    in
                    SolutionStep (eq e zero)
                        (case Dict.toList coeffs |> List.reverse |> List.filterNot (Tuple.second >> isZero) of
                            [] ->
                                SolutionError <| "Cannot solve " ++ Expression.toString e

                            [ ( 0, k ) ] ->
                                solve0 k

                            [ ( 1, a ) ] ->
                                solve1 a zero

                            [ ( 1, a ), ( 0, b ) ] ->
                                solve1 a b

                            [ ( 2, a ) ] ->
                                solve2 a zero zero

                            [ ( 2, a ), ( 1, b ) ] ->
                                solve2 a b zero

                            [ ( 2, a ), ( 0, c ) ] ->
                                solve2 a zero c

                            [ ( 2, a ), ( 1, b ), ( 0, c ) ] ->
                                solve2 a b c

                            _ ->
                                SolutionError <| "Cannot solve " ++ Expression.toString e
                        )
    in
    case ( l, r ) of
        ( Integer 0, _ ) ->
            go r

        ( _, Integer 0 ) ->
            go l

        _ ->
            go (minus l r)


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
