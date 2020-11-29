module PlotterTest exposing (suite)

import Expect
import Expression.Plotter as Plotter exposing (Bounds)
import Test exposing (Test, describe, test)


bounds : Bounds
bounds =
    { minx = -1
    , maxx = 3
    , miny = -2
    , maxy = 4
    , steps = 100
    }


suite : Test
suite =
    describe "screen transformations"
        [ test "screenToX 0 is minx" <|
            \_ ->
                0
                    |> Plotter.screenToX bounds
                    |> Expect.equal bounds.minx
        , test "screenToY 0 is maxy" <|
            \_ ->
                0
                    |> Plotter.screenToY bounds
                    |> Expect.equal bounds.maxy
        , test "screenToX (steps-1) is maxx" <|
            \_ ->
                bounds.steps
                    - 1
                    |> Plotter.screenToX bounds
                    |> Expect.equal bounds.maxx
        , test "screenToY (steps-1) is miny" <|
            \_ ->
                (bounds.steps - 1)
                    |> Plotter.screenToY bounds
                    |> Expect.equal bounds.miny
        , test "xToScreen minx is 0" <|
            \_ ->
                bounds.minx
                    |> Plotter.xToScreen bounds
                    |> Expect.equal 0
        , test "yToScreen maxy is 0" <|
            \_ ->
                bounds.maxy
                    |> Plotter.yToScreen bounds
                    |> Expect.equal 0
        , test "xToScreen maxx is (steps-1)" <|
            \_ ->
                bounds.maxx
                    |> Plotter.xToScreen bounds
                    |> Expect.equal (bounds.steps - 1)
        , test "yToScreen miny is (steps-1)" <|
            \_ ->
                bounds.miny
                    |> Plotter.yToScreen bounds
                    |> Expect.equal (bounds.steps - 1)
        ]



-- [zx+iy](z²-1)(z-2-i)²/(z²+2+2i)
