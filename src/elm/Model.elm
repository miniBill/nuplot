module Model exposing (Flags, Model, Msg(..), Row, RowResult(..), WorkerRequest(..), WorkerResponse(..), WorkerResult, emptyRow, workerRequestCodec, workerResponseCodec, workerResultCodec)

import Bounce exposing (Bounce)
import Codec exposing (Codec)


type alias Flags =
    {}


type alias Model =
    List Row


type alias Row =
    { input : String
    , result : RowResult
    , bounce : Bounce
    }


emptyRow : Row
emptyRow =
    { input = ""
    , result = Empty
    , bounce = Bounce.init
    }


type RowResult
    = Empty
    | Waiting
    | Calculating
    | ParseError String
    | Plotted String


type alias WorkerResult =
    { input : String
    , result : Result String String
    }


workerResultCodec : Codec WorkerResult
workerResultCodec =
    Codec.object WorkerResult
        |> Codec.field "input" .input Codec.string
        |> Codec.field "result" .result (Codec.result Codec.string Codec.string)
        |> Codec.buildObject


type Msg
    = Input { row : Int, input : String }
    | WorkerResponse WorkerResponse
    | WorkerResponseDecodingFailed
    | BounceMsg Int


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
