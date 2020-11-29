module UI exposing (main)

import Bounce
import Browser
import Element exposing (fill, height, width)
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import List.Extra as List
import Model exposing (Flags, Model, Msg(..), RowResult(..))
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
            , result = Plotted
            , plotting = x
            , bounce = Bounce.init
            }

        raw =
            [ ex "y = sin x"
            , ex "x² + y² = 25"
            , ex "x² + y² < 25"
            , Model.emptyRow
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
                    (\{ bounce, plotting } ->
                        { input = input
                        , result = Typing
                        , bounce = Bounce.push bounce
                        , plotting = plotting
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

                Just { input, bounce, plotting } ->
                    let
                        newBounce =
                            Bounce.pop bounce
                    in
                    ( model
                        |> List.setAt row
                            { input = input
                            , bounce = newBounce
                            , result =
                                if Bounce.steady newBounce then
                                    Plotted

                                else
                                    Typing
                            , plotting =
                                if Bounce.steady newBounce then
                                    input

                                else
                                    plotting
                            }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
