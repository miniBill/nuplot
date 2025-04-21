module SolverTest exposing (suite)

import Expect
import Expression exposing (Expression(..), RelationOperation(..), SolutionTree(..))
import Expression.Simplify exposing (simplify)
import Expression.Solver
import Expression.Utils exposing (by, div, i, minus, minusOne, negate_, one, plus, square, two, x, zero)
import List
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Expression.Solver module"
        [ describe "Expression.Solver.solve" <| List.map toTestSolve solveTests ]


toTestSolve : ( Expression, Expression, List Expression ) -> Test
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


solveTests : List ( Expression, Expression, List Expression )
solveTests =
    let
        xeq =
            RelationOperation Equals x

        none =
            Variable "none: x"

        -- pi =
        --     Variable "pi"
    in
    [ {- let
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
         ),
      -}
      ( zero, x, [ Variable "forall: x" ] )
    , ( one, x, [ none ] )
    , ( x, x, [ xeq zero ] )
    , ( plus [ x, one ], x, [ xeq minusOne ] )
    , ( plus [ by [ two, x ], one ], x, [ xeq <| negate_ <| div one two ] )
    , ( square x, x, [ xeq zero ] )
    , ( plus [ square x, minusOne ], x, [ xeq one, xeq minusOne ] )
    , ( plus [ square x, one ], x, [ xeq i, xeq <| negate_ i ] )
    , ( plus [ square x, x ], x, [ xeq zero, xeq minusOne ] )
    , ( plus [ square x, by [ two, x ], one ], x, [ xeq minusOne ] )
    , ( plus [ square x, by [ two, x ], Integer -3 ], x, [ xeq one, xeq (Integer -3) ] )
    , ( simplify <|
            by
                [ minus x one
                , minus x two
                , minus x minusOne
                , minus x two
                ]
      , x
      , [ xeq minusOne
        , xeq one
        , xeq two
        ]
      )

    {- , ( simplify <|
             by
                 [ minus x pi
                 , minus x pi
                 , minus x pi
                 ]
       , x
       , [ xeq pi
         , xeq pi
         , xeq pi
         ]
       )
    -}
    ]
