module UI exposing (main)

import Bounce
import Browser
import Element exposing (Element, fill, height, width)
import Element.Lazy
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import Expression.Parser
import Expression.Utils exposing (zero)
import List.Extra as List
import Model exposing (Flags, Model, Msg(..), Output(..))
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


init : Flags -> ( Model, Cmd msg )
init _ =
    let
        ex x =
            { input = x
            , output = Parsed <| Result.withDefault zero <| Expression.Parser.parse x
            , bounce = Bounce.init
            }

        raw =
            [ ex "plot([zx+iy]z)"
            , Model.emptyRow
            ]
    in
    ( raw
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    Element.column
        [ Element.spacing <| 2 * Theme.spacing
        , width fill
        ]
    <|
        List.indexedMap (\index row -> Element.Lazy.lazy2 UI.RowView.view index row) model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input { row, input } ->
            model
                |> List.updateAt row
                    (\{ bounce, output } ->
                        { input = input
                        , output = toTyping output
                        , bounce = Bounce.push bounce
                        }
                    )
                |> List.filter (not << String.isEmpty << .input)
                |> (\l ->
                        ( l ++ [ Model.emptyRow ]
                        , if String.isEmpty input then
                            Cmd.none

                          else
                            Bounce.delay 1000 (BounceMsg row)
                        )
                   )

        BounceMsg row ->
            case List.getAt row model of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just { input, bounce, output } ->
                    let
                        newBounce =
                            Bounce.pop bounce
                    in
                    ( model
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
                    , Cmd.none
                    )


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
    Sub.none
