module Model exposing (Flags, Model, Msg(..), Row, RowResult(..), WorkerRequest(..), WorkerResponse(..), WorkerResult, emptyRow, workerRequestCodec, workerResponseCodec, workerResultCodec)

import Codec exposing (Codec)


type alias Flags =
    {}


type alias Model =
    List Row


type alias Row =
    { input : String
    , result : RowResult
    }


emptyRow : Row
emptyRow =
    { input = ""
    , result = Empty
    }


type RowResult
    = Empty
    | Waiting
    | Calculating
    | ParseError String
    | Plotted { interpreted : String, png : String }


type alias WorkerResult =
    { input : String
    , result :
        Result String
            { interpreted : String
            , png : String
            }
    }


workerResultCodec : Codec WorkerResult
workerResultCodec =
    Codec.object WorkerResult
        |> Codec.field "input" .input Codec.string
        |> Codec.field "result"
            .result
            (Codec.result Codec.string
                (Codec.object (\i p -> { interpreted = i, png = p })
                    |> Codec.field "interpreted" .interpreted Codec.string
                    |> Codec.field "png" .png Codec.string
                    |> Codec.buildObject
                )
            )
        |> Codec.buildObject


type Msg
    = Input { row : Int, input : String }
    | WorkerResponse WorkerResponse
    | WorkerResponseDecodingFailed


type WorkerRequest
    = PlotRequest String
    | WorkerRequestDecodingFailed


workerRequestCodec : Codec WorkerRequest
workerRequestCodec =
    Codec.custom
        (\ffail fplot value ->
            case value of
                WorkerRequestDecodingFailed ->
                    ffail

                PlotRequest p ->
                    fplot p
        )
        |> Codec.variant0 "WorkerRequestDecodingFailed" WorkerRequestDecodingFailed
        |> Codec.variant1 "PlotRequest" PlotRequest Codec.string
        |> Codec.buildCustom


type WorkerResponse
    = PlotResponse WorkerResult


workerResponseCodec : Codec WorkerResponse
workerResponseCodec =
    Codec.custom
        (\fplot value ->
            case value of
                PlotResponse r ->
                    fplot r
        )
        |> Codec.variant1 "PlotResponse" PlotResponse workerResultCodec
        |> Codec.buildCustom
