module Glsl.GlslConverter exposing (main)

import Browser
import Element as Element exposing (Element, column, el, fill, height, padding, paragraph, row, scrollbars, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Glsl.Parser as Parser
import Glsl.PrettyPrinter as PrettyPrinter
import Glsl.Simplify as Simplify
import Glsl.Types exposing (Function, Statement)
import Html
import Parser exposing ((|.), (|=), DeadEnd, Step(..), Trailing(..), oneOf, spaces, succeed)


type alias Model =
    { input : String
    , output : String
    }


type Msg
    = Input String


type Either l r
    = Left l
    | Right r


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = Element.layout [ width fill, height fill ] << view
        , update = update
        }


init : Model
init =
    let
        i =
            """{



}"""
    in
    { input = i
    , output = toOutput i
    }


view : Model -> Element Msg
view model =
    row
        [ padding 10
        , spacing 10
        , width fill
        , height fill
        ]
        [ input model.input
        , output model.output
        ]


input : String -> Element Msg
input value =
    Input.multiline
        [ width fill
        , height fill
        , spacing 10
        ]
        { onChange = Input
        , text = value
        , placeholder = Nothing
        , label = Input.labelAbove [] <| text "Input (GLSL function or expression)"
        , spellcheck = False
        }


output : String -> Element Msg
output value =
    let
        bordering =
            el
                [ width fill
                , height fill
                , Border.width 1
                , Font.family [ Font.typeface "Fira Code", Font.monospace ]
                , padding 10
                , scrollbars
                ]
    in
    if String.contains "problem" value then
        paragraph [] [ text value ]
            |> bordering

    else
        let
            tc =
                Html.pre [] [ Html.text value ]
                    |> Element.html
                    |> bordering
        in
        column
            [ spacing 10
            , width fill
            , height fill
            ]
            [ text "Output (Elm code)"
            , tc
            ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input i ->
            { model | input = i, output = toOutput i }


toOutput : String -> String
toOutput i =
    case parse i of
        Err e ->
            Debug.toString e

        Ok (Left o) ->
            PrettyPrinter.function <| Simplify.function o

        Ok (Right o) ->
            PrettyPrinter.statement <| Simplify.statement o


parse : String -> Result (List DeadEnd) (Either Function Statement)
parse =
    Parser.run <|
        succeed identity
            |. spaces
            |= oneOf
                [ Parser.map Left Parser.function
                , Parser.map Right Parser.statement
                ]
            |. spaces
            |. Parser.end
