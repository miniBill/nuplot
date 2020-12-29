module Expression.Solver exposing (solve)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), RelationOperation(..), UnaryOperation(..))
import Expression.Simplify exposing (simplify)
import Expression.Utils exposing (by, div, ipow, minus, negate_, one, plus, sqrt_, square, vector, zero)
import List.Extra as List


solve : Expression -> Expression -> List (Result String Expression)
solve e x =
    case x of
        Variable v ->
            simplify e
                |> innerSolve v
                |> (::) (Ok e)
                |> cut

        _ ->
            [ Err "The second argument of `solve` must be the variable you want to solve for" ]


cut : List (Result String Expression) -> List (Result String Expression)
cut ls =
    let
        ts x =
            case x of
                Ok e ->
                    Expression.toString e

                Err e ->
                    "!ERR!" ++ e
    in
    ls
        |> List.foldr
            (\e ( acc, seen ) ->
                case Dict.get (ts e) seen of
                    Nothing ->
                        ( e :: acc, Dict.insert (ts e) acc seen )

                    Just shortcut ->
                        ( e :: shortcut, seen )
            )
            ( [], Dict.empty )
        |> Tuple.first


innerSolve : String -> Expression -> List (Result String Expression)
innerSolve v expr =
    case expr of
        RelationOperation Equals l r ->
            Ok expr :: solveEquation v l r

        RelationOperation _ _ _ ->
            [ Err "Cannot solve disequations [yet!]" ]

        _ ->
            [ Err "Cannot solve this" ]


type SolutionTree
    = Forall String
    | Error String
    | None
    | Step Expression SolutionTree
    | Branch Expression (List SolutionTree)


solveEquation : String -> Expression -> Expression -> List (Result String Expression)
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
                    [ Err <| "Cannot solve " ++ Expression.toString e
                    , Err err
                    ]

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
                                [ Err <| "∀" ++ v ]

                            else
                                [ Err "No solution" ]

                        solve1 a b =
                            if isZero a then
                                solve0 b

                            else
                                let
                                    sol =
                                        eqv <| div (neg b) a
                                in
                                [ Ok sol
                                , Ok <| simplify sol
                                ]

                        solve2 a b c =
                            if isZero a then
                                solve1 b c

                            else if isZero b then
                                let
                                    solp =
                                        sqrt_ (div (neg c) a)

                                    solm =
                                        neg solp
                                in
                                [ Ok <|
                                    List
                                        [ vector
                                            [ eqv solp
                                            , eqv <| simplify solp
                                            ]
                                        , vector
                                            [ eqv solm
                                            , eqv <| simplify solm
                                            ]
                                        ]
                                ]

                            else
                                let
                                    delta =
                                        minus (square b) (by [ Integer 4, a, c ])

                                    solp =
                                        div (plus [ neg b, sqrt_ delta ]) (by [ Integer 2, a ])

                                    solm =
                                        div (minus (neg b) (sqrt_ delta)) (by [ Integer 2, a ])
                                in
                                [ Ok <|
                                    List
                                        [ vector
                                            [ eqv solp
                                            , eqv <| simplify solp
                                            ]
                                        , vector
                                            [ eqv solm
                                            , eqv <| simplify solm
                                            ]
                                        ]
                                ]
                    in
                    Ok (eq e zero)
                        :: (case Dict.toList coeffs |> List.reverse |> List.filterNot (Tuple.second >> isZero) of
                                [] ->
                                    [ Err <| "Cannot solve " ++ Expression.toString e ]

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
                                    [ Err <| "Cannot solve " ++ Expression.toString e ]
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
