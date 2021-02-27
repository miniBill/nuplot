module UI.Theme exposing (bracketBorderWidth, bracketWidth, colors, column, darkIconAttrs, darken, fontSize, grid, hr, iconSize, lightIconAttrs, onCtrlEnter, onEnter, onKey, roundness, row, smallDarkIconAttrs, spacing, whiteLines, wrappedRow)

import Ant.Icon
import Color
import Element.WithContext as Element exposing (Attribute, Color, Element, el, fill, none, rgb, rgb255, rgba, shrink, width)
import Element.WithContext.Border as Border
import Html.Events
import Json.Decode as Decode
import Model exposing (Context)


type alias Element msg =
    Element.Element Context msg


type alias Attribute msg =
    Element.Attribute Context msg


iconSize :
    { documentPicker : Int
    , standard : number
    }
iconSize =
    { documentPicker = 20 * 3 // 4
    , standard = 20
    }


smallDarkIconAttrs : List (Ant.Icon.Attribute msg)
smallDarkIconAttrs =
    [ Ant.Icon.width iconSize.documentPicker
    , Ant.Icon.fill colors.iconDarkColor
    ]


darkIconAttrs : List (Ant.Icon.Attribute msg)
darkIconAttrs =
    [ Ant.Icon.width iconSize.standard
    , Ant.Icon.fill colors.iconDarkColor
    ]


lightIconAttrs : List (Ant.Icon.Attribute msg)
lightIconAttrs =
    [ Ant.Icon.width iconSize.standard
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


colors :
    { background : Color
    , errorMessage : Color
    , iconDarkColor : Color
    , iconLightColor : Color
    , modalTransparentBackground : Color
    , selectedDocument : Color
    , unselectedDocument : Color
    }
colors =
    { background = rgb 0.9 0.9 0.9
    , errorMessage = rgb 0.9 0 0
    , iconDarkColor = rgb 0.1 0.1 0.1
    , iconLightColor = rgb 0.9 0.9 0.9
    , modalTransparentBackground = rgba 0.5 0.5 0.5 0.5
    , selectedDocument = rgb255 0xFF 0xD7 0
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


onKey : (String -> Maybe msg) -> Attribute msg
onKey msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\s ->
                case msg s of
                    Just m ->
                        Decode.succeed m

                    Nothing ->
                        Decode.fail "ignored"
            )
        |> Html.Events.on "keyup"
        |> Element.htmlAttribute


hr : Element msg
hr =
    el
        [ width fill
        , Border.widthEach
            { top = 1
            , left = 0
            , right = 0
            , bottom = 0
            }
        ]
        Element.none
