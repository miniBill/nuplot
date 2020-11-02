module Expression.Plotter exposing (Bounds, defaultBounds, getShapes)

import Complex
import Dict
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import List.MyExtra as List
import Model exposing (ColoredShapes, Shape(..))


type alias Bounds =
    { minx : Float
    , miny : Float
    , maxx : Float
    , maxy : Float
    , steps : Int
    }


defaultBounds : Bounds
defaultBounds =
    { minx = -5
    , maxx = 5
    , miny = -5
    , maxy = 5
    , steps = 250
    }


getShapes : Bounds -> Graph -> List ColoredShapes
getShapes { minx, miny, maxx, maxy, steps } graph =
    let
        xrange =
            List.range 0 (steps - 1)
                |> List.map toFloat
                |> List.map (\x -> (x / (toFloat steps - 1)) * (maxx - minx) + minx)

        yrange =
            List.range 0 (steps - 1)
                |> List.map toFloat
                |> List.map (\y -> (y / (toFloat steps - 1)) * (maxy - miny) + miny)

        axes =
            case graph of
                Explicit2D _ ->
                    axes2

                Implicit2D _ _ ->
                    axes2

                Relation2D _ _ _ ->
                    axes2

        axes2 =
            [ { color = "red"
              , shapes = [ Line { x1 = minx, y1 = 0, x2 = maxx, y2 = 0 } ]
              }
            , { color = "green"
              , shapes = [ Line { x1 = 0, y1 = miny, x2 = 0, y2 = maxy } ]
              }
            ]

        plot =
            case graph of
                Explicit2D e ->
                    explicit2d e

                Relation2D rop l r ->
                    relation2d rop l r

                Implicit2D l r ->
                    implicit2d xrange yrange l r

        explicit2d e =
            xrange
                |> List.map
                    (\x ->
                        ( x
                        , Complex.real <| Expression.value (Dict.singleton "x" <| Complex.fromReal x) e
                        )
                    )
                |> List.mapAccuml1 (\( ox, oy ) ( x, y ) -> Line { x1 = ox, y1 = -oy, x2 = x, y2 = -y })

        relation2d rop l r =
            let
                w =
                    (maxx - minx) / toFloat steps

                h =
                    (maxy - miny) / toFloat steps
            in
            xrange
                |> List.concatMap
                    (\x ->
                        yrange
                            |> List.concatMap
                                (\y ->
                                    if 0 < (dvalue x y <| RelationOperation rop l r) then
                                        [ Rectangle { x = x, y = y, width = w, height = h } ]

                                    else
                                        []
                                )
                    )
    in
    { color = "black", shapes = plot } :: axes


marchingSquare : Float -> Float -> { x : Float, val : Bool, lastrowVal : Bool } -> { lastrowVal : Bool, val : Bool, x : Float } -> List ( ( Float, Float ), ( Float, Float ) )
marchingSquare y lastrowY { x, val, lastrowVal } left =
    case ( ( left.lastrowVal, lastrowVal ), ( val, left.val ) ) of
        ( ( True, True ), ( True, True ) ) ->
            []

        ( ( False, False ), ( False, False ) ) ->
            []

        ( ( True, True ), ( True, False ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, y ) ) ]

        ( ( True, True ), ( False, True ) ) ->
            [ ( ( (left.x + x) / 2, y ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( True, False ), ( True, True ) ) ->
            [ ( ( (left.x + x) / 2, lastrowY ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( False, True ), ( True, True ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, lastrowY ) ) ]

        ( ( False, False ), ( False, True ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, y ) ) ]

        ( ( False, False ), ( True, False ) ) ->
            [ ( ( (left.x + x) / 2, y ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( False, True ), ( False, False ) ) ->
            [ ( ( (left.x + x) / 2, lastrowY ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( True, False ), ( False, False ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, lastrowY ) ) ]

        ( ( True, True ), ( False, False ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( False, False ), ( True, True ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( True, False ), ( True, False ) ) ->
            [ ( ( (left.x + x) / 2, lastrowY ), ( (left.x + x) / 2, y ) ) ]

        ( ( False, True ), ( False, True ) ) ->
            [ ( ( (left.x + x) / 2, lastrowY ), ( (left.x + x) / 2, y ) ) ]

        ( ( True, False ), ( False, True ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, lastrowY ) )
            , ( ( (left.x + x) / 2, y ), ( x, (lastrowY + y) / 2 ) )
            ]

        ( ( False, True ), ( True, False ) ) ->
            [ ( ( (left.x + x) / 2, lastrowY ), ( x, (lastrowY + y) / 2 ) )
            , ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, y ) )
            ]


implicit2d : List Float -> List Float -> Expression -> Expression -> List Shape
implicit2d xrange yrange l r =
    case yrange of
        [] ->
            []

        miny :: resty ->
            let
                e : Expression
                e =
                    RelationOperation Equals l r

                calculateRow y =
                    List.map (\x -> dvalue x y e > 0) xrange
            in
            resty
                |> List.foldl
                    (\y ( lastrow, lastrowY, acc ) ->
                        let
                            row =
                                calculateRow y

                            lines =
                                List.map3 (\x val lastrowVal -> { x = x, val = val, lastrowVal = lastrowVal }) xrange row lastrow
                                    |> List.mapAccuml1 (\left curr -> marchingSquare y lastrowY curr left)
                                    |> List.concat
                        in
                        ( row, y, lines :: acc )
                    )
                    ( calculateRow miny, miny, [] )
                |> (\( _, _, a ) -> a)
                |> List.concat
                |> List.map (\( ( x1, y1 ), ( x2, y2 ) ) -> Line { x1 = x1, y1 = y1, x2 = x2, y2 = y2 })


dvalue : Float -> Float -> Expression -> Float
dvalue x y e =
    Complex.real <|
        Expression.value
            (Dict.fromList
                [ ( "x", Complex.fromReal x )
                , ( "y", Complex.fromReal y )
                ]
            )
            e
