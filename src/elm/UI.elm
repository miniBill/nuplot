port module UI exposing (main)

import Browser
import Codec
import Element exposing (fill, height, width)
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import List.Extra as List
import Model exposing (Flags, Model, Msg(..), RowResult(..), WorkerRequest(..), WorkerResponse(..), workerRequestCodec, workerResponseCodec)
import Process
import Task
import UI.Theme as Theme
import UI.View exposing (view)
import Worker


port toWorker : String -> Cmd msg


send : WorkerRequest -> Cmd Msg
send request =
    case ( request, Theme.paintInForegroud ) of
        ( PlotRequest input, True ) ->
            Process.sleep 10
                |> Task.map (\_ -> Worker.doPlot input)
                |> Task.perform WorkerResponse

        _ ->
            toWorker <| Codec.encodeToString 0 workerRequestCodec request


port fromWorker : (String -> msg) -> Sub msg


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
        raw =
            [ { input = "y = sin x", result = Calculating }
            , { input = "x² + y² = 25", result = Calculating }
            , { input = "x² + y² < 25", result = Calculating }
            , { input = "", result = Empty }
            ]
    in
    ( raw
    , raw
        |> List.map .input
        |> List.filterNot String.isEmpty
        |> List.map (PlotRequest >> send)
        |> Cmd.batch
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input { row, input } ->
            model
                |> List.setAt row
                    { input = input
                    , result = Calculating
                    }
                |> List.filter (not << String.isEmpty << .input)
                |> (\l ->
                        ( l ++ [ Model.emptyRow ]
                        , if String.isEmpty input then
                            Cmd.none

                          else
                            send <| PlotRequest input
                        )
                   )

        WorkerResponse (PlotResponse { input, result }) ->
            ( model
                |> List.updateIf (.input >> (==) input)
                    (\r ->
                        { r
                            | result = toRowResult result
                        }
                    )
            , Cmd.none
            )

        WorkerResponseDecodingFailed ->
            ( model, Cmd.none )


toRowResult :
    Result String
        { interpreted : String
        , png : String
        }
    -> RowResult
toRowResult result =
    case result of
        Err e ->
            ParseError e

        Ok r ->
            Plotted r


subscriptions : Model -> Sub Msg
subscriptions _ =
    fromWorker
        (Codec.decodeString workerResponseCodec
            >> Result.map WorkerResponse
            >> Result.withDefault WorkerResponseDecodingFailed
        )
