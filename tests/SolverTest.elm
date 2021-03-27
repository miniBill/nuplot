module SolverTest exposing (suite)

import Dict
import Expect
import Expression exposing (AssociativeOperation(..), Expression(..), RelationOperation(..), SolutionTree(..))
import Expression.Solver
import Expression.Utils exposing (by, c, ipow, minus, plus, square, t, x, y, z)
import List
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        toTestSolve ( from, x, to ) =
            let
                solved =
                    Expression.Solver.solve from x

                solutions =
                    go solved

                go node =
                    case node of
                        SolutionDone s ->
                            [ s ]

                        SolutionForall s ->
                            [ Variable <| "forall: " ++ s ]

                        SolutionError e ->
                            [ Variable <| "error:" ++ e.en ]

                        SolutionNone e ->
                            [ Variable <| "none: " ++ e ]

                        SolutionStep _ c ->
                            go c

                        SolutionBranch cs ->
                            List.concatMap go cs

                toString e =
                    Expression.toString e ++ " = " ++ Debug.toString e
            in
            test ("Has the correct solutions for " ++ Expression.toString from) <|
                \_ ->
                    Expect.equalLists
                        (List.map toString solutions)
                        (List.map toString to)
    in
    describe "The Expression.Solver module"
        [ describe "Expression.Solver.solve" <| List.map toTestSolve solveTests ]


solveTests : List ( Expression, Expression, List Expression )
solveTests =
    [ let
        repls =
            [ "x", "y", "z" ]
                |> List.map
                    (\v ->
                        ( v
                        , Just <|
                            plus
                                [ Variable <| "o" ++ v
                                , by [ t, Variable <| "d" ++ v ]
                                ]
                        )
                    )
                |> Dict.fromList
      in
      ( Replace repls <|
            minus
                (plus [ square x, square y ])
                (ipow z 2)
      , t
      , []
      )
    ]
