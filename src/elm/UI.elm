port module UI exposing (main)

import Browser
import Element exposing (fill, height, width)
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import List.Extra as List
import Model exposing (Flags, GraphStatus(..), Model, Msg(..), PlotResult)
import UI.View exposing (view)


port plot : String -> Cmd msg


port plotted : (PlotResult -> msg) -> Sub msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view =
            Element.layout
                [ width fill
                , height fill
                ]
                << view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd msg )
init _ =
    let
        raw =
            [ "y = sin x"
            , "x² + y² = 25"
            , "!"
            , ""
            ]
    in
    ( List.map (\i -> { input = i, graph = Just Drawing }) raw
    , Cmd.batch <| List.map (\i -> plot i) raw
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Input { row, value } ->
            model
                |> List.updateAt row (\r -> { r | input = value })
                |> List.filter (not << String.isEmpty << .input)
                |> (\l ->
                        ( l
                            ++ [ { input = ""
                                 , graph = Nothing
                                 }
                               ]
                        , plot value
                        )
                   )

        Plotted _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    plotted Plotted
