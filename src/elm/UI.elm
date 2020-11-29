module UI exposing (main)

import Bounce
import Browser
import Element exposing (fill, height, width)
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import Expression.Parser
import List.Extra as List
import Model exposing (Flags, Model, Msg(..), PlotStatus(..))
import UI.Theme as Theme
import UI.View exposing (view)


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
            , plotStatus = Plotting <| Result.withDefault (Integer 0) <| Expression.Parser.parse x
            , bounce = Bounce.init
            }

        raw =
            [ ex "y = sin x"

            --, ex "x² + y² = 25"
            --, ex "x² + y² < 25"
            --, Model.emptyRow
            ]
    in
    ( raw
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input { row, input } ->
            model
                |> List.updateAt row
                    (\{ bounce, plotStatus } ->
                        { input = input
                        , plotStatus = toTyping plotStatus
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

                Just { input, bounce, plotStatus } ->
                    let
                        newBounce =
                            Bounce.pop bounce
                    in
                    ( model
                        |> List.setAt row
                            { input = input
                            , bounce = newBounce
                            , plotStatus =
                                if Bounce.steady newBounce then
                                    case Expression.Parser.parse input of
                                        Ok r ->
                                            Plotting r

                                        Err e ->
                                            ParseError <| Expression.Parser.errorsToString input e

                                else
                                    toTyping plotStatus
                            }
                    , Cmd.none
                    )


toTyping : PlotStatus -> PlotStatus
toTyping plotStatus =
    Typing <|
        case plotStatus of
            Plotting e ->
                Just e

            Typing e ->
                e

            Empty ->
                Nothing

            ParseError _ ->
                Nothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
