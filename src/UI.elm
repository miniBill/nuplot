module UI exposing (main)

import Browser
import Element exposing (Element, column, el, fill, height, padding, spacing, text, width)
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Expression exposing (Graph(..))
import Expression.Parser
import List.Extra as List
import UI.Theme as Theme


type alias Model =
    List String


type alias Msg =
    { row : Int
    , value : String
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view =
            Element.layout
                [ width fill
                , height fill
                ]
                << view
        , update = update
        }


init : Model
init =
    [ "y = x² + x - 6"
    , "x² + y² = 25"
    , "!"
    , ""
    ]


view : Model -> Element Msg
view model =
    column
        [ spacing <| 2 * Theme.spacing
        , width fill
        ]
    <|
        List.indexedMap (\row value -> Element.Lazy.lazy2 viewRow row value) model


viewRow : Int -> String -> Element Msg
viewRow row value =
    column
        [ padding Theme.spacing
        , spacing Theme.spacing
        , width fill
        ]
    <|
        Input.text [ width fill ]
            { label = Input.labelHidden "Input"
            , onChange =
                \newValue ->
                    { row = row
                    , value = newValue
                    }
            , placeholder = Just <| Input.placeholder [] <| text "y = x² + 5x + 6"
            , text = value
            }
            :: (if String.isEmpty value then
                    []

                else
                    let
                        parsed =
                            Expression.Parser.parse value
                    in
                    case parsed of
                        Err e ->
                            [ el [ Font.family [ Font.monospace ] ] <| text <| Expression.Parser.errorsToString value e ]

                        Ok expr ->
                            let
                                graph =
                                    Expression.Parser.parseGraph expr
                            in
                            [ text <| "Interpreted as: " ++ Expression.toString expr, draw graph ]
               )


draw : Graph -> Element msg
draw graph =
    case graph of
        Explicit2D e ->
            text <| "y = " ++ Expression.toString e

        Implicit2D l rop r ->
            text <| Expression.toString l ++ " " ++ Expression.relationToString rop ++ " " ++ Expression.toString r


update : Msg -> Model -> Model
update { row, value } model =
    model
        |> List.setAt row value
        |> List.filter (not << String.isEmpty)
        |> (\l -> l ++ [ "" ])
