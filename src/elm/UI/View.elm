module UI.View exposing (view)

import Element exposing (Element, alignTop, centerX, column, el, fill, rgb, spacing, text, width, wrappedRow)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import List.MyExtra as List
import Model exposing (Model, Msg(..), Row, RowResult(..))
import UI.Theme as Theme


draw : String -> Element msg
draw png =
    Element.image []
        { src = png
        , description = "Plot"
        }


viewRow : Int -> Row -> Element Msg
viewRow index row =
    column
        [ spacing Theme.spacing
        , width fill
        , alignTop
        ]
        [ inputLine index row
        , statusLine row
        , outputBlock row
        ]


view : Model -> Element Msg
view model =
    wrappedRow
        [ spacing <| 2 * Theme.spacing
        , width fill
        ]
    <|
        List.indexedMap (\index row -> Element.Lazy.lazy2 viewRow index row) model


inputLine : Int -> Row -> Element Msg
inputLine index row =
    Input.text [ width <| Element.minimum 600 fill ]
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


statusLine : Row -> Element msg
statusLine row =
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


outputBlock : Row -> Element msg
outputBlock row =
    let
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

            Plotted { png } ->
                draw png
        )
