module UI exposing (main)

import Bounce
import Browser
import Browser.Dom
import Browser.Events
import Element exposing (Element, fill, height, width)
import Element.Lazy
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import Expression.Parser
import Expression.Utils exposing (zero)
import List.Extra as List
import Model exposing (Flags, Model, Msg(..), Output(..))
import Task
import UI.RowView
import UI.Theme as Theme


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view =
            Element.layout
                [ width fill
                , height fill
                , Element.padding Theme.spacing
                ]
                << view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        ex x =
            { input = x
            , output = Parsed <| Result.withDefault zero <| Expression.Parser.parse x
            , bounce = Bounce.init
            }

        raw =
            [ ex "plotsqrtx"
            , ex "plot{2x,3x}"
            , Model.emptyRow
            ]

        measure =
            Browser.Dom.getViewport
                |> Task.map (.viewport >> .width >> floor)
    in
    ( { rows = raw
      , pageWidth = 1024
      }
    , Task.perform Width measure
    )


view : Model -> Element Msg
view model =
    Element.column
        [ Element.spacing <| 2 * Theme.spacing
        , width fill
        ]
    <|
        List.indexedMap (\index row -> Element.Lazy.lazy3 UI.RowView.view model.pageWidth index row) model.rows


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input { row, input } ->
            model.rows
                |> List.updateAt row
                    (\{ bounce, output } ->
                        { input = input
                        , output = toTyping output
                        , bounce = Bounce.push bounce
                        }
                    )
                |> List.filter (not << String.isEmpty << .input)
                |> (\l ->
                        ( { model | rows = l ++ [ Model.emptyRow ] }
                        , if String.isEmpty input then
                            Cmd.none

                          else
                            Bounce.delay 1000 (BounceMsg row)
                        )
                   )

        BounceMsg row ->
            case List.getAt row model.rows of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just { input, bounce, output } ->
                    let
                        newBounce =
                            Bounce.pop bounce
                    in
                    ( { model
                        | rows =
                            model.rows
                                |> List.setAt row
                                    { input = input
                                    , bounce = newBounce
                                    , output =
                                        if Bounce.steady newBounce then
                                            case Expression.Parser.parse input of
                                                Ok e ->
                                                    Parsed e

                                                Err e ->
                                                    ParseError <| Expression.Parser.errorsToString input e

                                        else
                                            toTyping output
                                    }
                      }
                    , Cmd.none
                    )

        Width width ->
            ( { model | pageWidth = width }, Cmd.none )


toTyping : Output -> Output
toTyping output =
    Typing <|
        case output of
            Empty ->
                Nothing

            ParseError _ ->
                Nothing

            Typing e ->
                e

            Parsed e ->
                Just e


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w _ -> Width w)
