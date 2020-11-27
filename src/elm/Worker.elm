port module Worker exposing (doPlot, main)

import Codec
import Expression.Parser
import Expression.Plotter
import Model exposing (Msg(..), RowResult(..), WorkerRequest(..), WorkerResponse(..), workerRequestCodec, workerResponseCodec)


port toWorker : (String -> msg) -> Sub msg


send : WorkerResponse -> Cmd msg
send =
    Codec.encodeToString 0 workerResponseCodec >> fromWorker


port fromWorker : String -> Cmd msg


type alias Flags =
    {}


type alias Model =
    ()


main : Program Flags Model WorkerRequest
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd msg )
init _ =
    ( (), Cmd.none )


update : WorkerRequest -> Model -> ( Model, Cmd msg )
update msg () =
    case msg of
        WorkerRequestDecodingFailed ->
            ( (), Cmd.none )

        PlotRequest input ->
            ( ()
            , send <| doPlot input
            )


doPlot : String -> WorkerResponse
doPlot input =
    PlotResponse <|
        let
            parsed =
                Expression.Parser.parse input
        in
        case parsed of
            Err e ->
                { input = input
                , result = Err <| Expression.Parser.errorsToString input e
                }

            Ok o ->
                let
                    g =
                        Expression.Parser.parseGraph o
                in
                { input = input
                , result = Ok <| Expression.Plotter.getPng Expression.Plotter.defaultBounds g
                }


subscriptions : Model -> Sub WorkerRequest
subscriptions _ =
    toWorker (Codec.decodeString workerRequestCodec >> Result.withDefault WorkerRequestDecodingFailed)
