module UI.View exposing (view)

import Element exposing (Element, centerX, column, el, fill, padding, rgb, spacing, text, width)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import Expression.Plotter
import List.MyExtra as List
import Model exposing (ColoredShapes, Model, Msg(..), Row, RowResult(..), Shape(..))
import Svg
import Svg.Attributes as Svg
import UI.Theme as Theme


draw : List ColoredShapes -> Element msg
draw coloredShapes =
    let
        toPixel n =
            String.fromFloat <| 50 * n

        shapesToSvg { color, shapes } =
            Svg.g [ Svg.stroke color ] <|
                List.map
                    (\shape ->
                        case shape of
                            Line { x1, y1, x2, y2 } ->
                                Svg.line
                                    [ Svg.x1 <| toPixel x1
                                    , Svg.y1 <| toPixel y1
                                    , Svg.x2 <| toPixel x2
                                    , Svg.y2 <| toPixel y2
                                    ]
                                    []

                            Rectangle r ->
                                Svg.rect
                                    [ Svg.x <| toPixel r.x
                                    , Svg.y <| toPixel r.y
                                    , Svg.width <| toPixel r.width
                                    , Svg.height <| toPixel r.height
                                    ]
                                    []
                    )
                    shapes

        { minx, maxx, miny, maxy } =
            Expression.Plotter.defaultBounds

        width =
            maxx - minx

        height =
            maxy - miny
    in
    coloredShapes
        |> List.map shapesToSvg
        |> Svg.svg
            [ Svg.viewBox <| String.join " " <| List.map toPixel [ minx, miny, width, height ]
            , Svg.width <| toPixel width
            , Svg.height <| toPixel height
            , Svg.stroke "black"
            , Svg.strokeWidth <| String.fromFloat <| Basics.min height width / 10
            ]
        |> Element.html


viewRow : Int -> Row -> Element Msg
viewRow index row =
    let
        inputLine =
            Input.text [ width fill ]
                { label = Input.labelHidden "Input"
                , onChange =
                    \newValue ->
                        Input
                            { row = index
                            , input = newValue
                            }
                , placeholder = Just <| Input.placeholder [] <| text "y = f(x)"
                , text = row.input
                }

        statusLine =
            case row.result of
                Empty ->
                    Element.none

                Waiting ->
                    text "Typing..."

                Calculating ->
                    text "Calculating..."

                ParseError e ->
                    el
                        [ Font.family [ Font.monospace ]
                        , Font.color <| rgb 1 0 0
                        ]
                    <|
                        text e

                Plotted { interpreted } ->
                    text <| "Interpreted as: " ++ interpreted

        outputBlock =
            Element.Keyed.el [ centerX ]
                ( row.input ++ resultTag
                , case row.result of
                    Empty ->
                        Element.none

                    Waiting ->
                        Element.none

                    Calculating ->
                        Element.none

                    ParseError _ ->
                        Element.none

                    Plotted { shapes } ->
                        draw shapes
                )

        resultTag =
            case row.result of
                Empty ->
                    " "

                Waiting ->
                    "W"

                Calculating ->
                    "C"

                ParseError _ ->
                    "E"

                Plotted _ ->
                    "P"
    in
    column
        [ padding Theme.spacing
        , spacing Theme.spacing
        , width fill
        ]
        [ inputLine, statusLine, outputBlock ]


view : Model -> Element Msg
view model =
    column
        [ spacing <| 2 * Theme.spacing
        , width fill
        ]
    <|
        List.indexedMap (\index row -> Element.Lazy.lazy2 viewRow index row) model
