module UI.Theme exposing (bracketBorderWidth, bracketWidth, colors, column, darkIconAttrs, darken, fontSize, grid, lightIconAttrs, onCtrlEnter, onEnter, roundness, row, spacing, whiteLines, wrappedRow)

import Ant.Icon
import Color
import Element exposing (Attribute, Color, Element, none, rgb, shrink)
import Html.Events
import Json.Decode as Decode


darkIconAttrs : List (Ant.Icon.Attribute msg)
darkIconAttrs =
    [ Ant.Icon.width 24
    , Ant.Icon.height 24
    , Ant.Icon.fill colors.iconDarkColor
    ]


lightIconAttrs : List (Ant.Icon.Attribute msg)
lightIconAttrs =
    [ Ant.Icon.width 24
    , Ant.Icon.height 24
    , Ant.Icon.fill colors.iconLightColor
    ]


roundness : number
roundness =
    -- Sync with nuplot.ts
    3


fontSize : number
fontSize =
    20


spacing : number
spacing =
    10


whiteLines : number
whiteLines =
    6


bracketWidth : number
bracketWidth =
    8


bracketBorderWidth : number
bracketBorderWidth =
    2


colors : { background : Color, iconDarkColor : Color, iconLightColor : Color, selectedDocument : Color, unselectedDocument : Color }
colors =
    { background = rgb 0.9 0.9 0.9
    , iconDarkColor = rgb 0.1 0.1 0.1
    , iconLightColor = rgb 0.9 0.9 0.9
    , selectedDocument = rgb 0.8 0.8 0.9
    , unselectedDocument = rgb 0.8 0.8 0.8
    }


darken : Color -> Color
darken =
    mapHsl (\c -> { c | lightness = 0.8 * c.lightness })


type alias Hsla =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }


mapHsl : (Hsla -> Hsla) -> Color -> Color
mapHsl f =
    Element.toRgb
        >> Color.fromRgba
        >> Color.toHsla
        >> f
        >> Color.fromHsla
        >> Color.toRgba
        >> Element.fromRgb


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row (Element.spacing spacing :: attrs)


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs =
    Element.wrappedRow (Element.spacing spacing :: attrs)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column (Element.spacing spacing :: attrs)


grid : List (Attribute msg) -> List (List (Element msg)) -> Element msg
grid attrs rows =
    if List.isEmpty rows then
        none

    else
        let
            toColumn i =
                { width = shrink
                , header = none
                , view =
                    \x ->
                        x
                            |> List.drop i
                            |> List.head
                            |> Maybe.withDefault none
                            |> Element.el [ Element.alignBottom ]
                }

            w =
                List.map List.length rows
                    |> List.maximum
                    |> Maybe.withDefault 0
        in
        Element.table (Element.spacing spacing :: attrs)
            { columns = List.map toColumn <| List.range 0 (w - 1)
            , data = rows
            }


onEnter : msg -> Attribute msg
onEnter msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\s ->
                if s == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail "ignored"
            )
        |> Html.Events.on "keyup"
        |> Element.htmlAttribute


onCtrlEnter : msg -> Attribute msg
onCtrlEnter msg =
    Decode.map2 Tuple.pair
        (Decode.field "key" Decode.string)
        (Decode.field "ctrlKey" Decode.bool)
        |> Decode.andThen
            (\( s, ctrl ) ->
                if s == "Enter" && ctrl then
                    Decode.succeed msg

                else
                    Decode.fail "ignored"
            )
        |> Html.Events.on "keyup"
        |> Element.htmlAttribute
