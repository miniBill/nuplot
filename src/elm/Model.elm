module Model exposing (ColoredShapes, Flags, Model, Msg(..), PlotResult, Row, RowResult(..), Shape(..), WorkerRequest(..), WorkerResponse(..), plotResultCodec, workerRequestCodec, workerResponseCodec)

import Codec exposing (Codec)


type alias Flags =
    {}


type alias Model =
    List Row


type alias Row =
    { input : String
    , result : RowResult
    }


type RowResult
    = Empty
    | Waiting
    | Calculating
    | ParseError String
    | Plotted { interpreted : String, shapes : List ColoredShapes }


type alias PlotResult =
    { input : String
    , result :
        Result String
            { interpreted : String
            , shapes : List ColoredShapes
            }
    }


plotResultCodec : Codec PlotResult
plotResultCodec =
    Codec.object PlotResult
        |> Codec.field "input" .input Codec.string
        |> Codec.field "result"
            .result
            (Codec.result Codec.string
                (Codec.object (\i s -> { interpreted = i, shapes = s })
                    |> Codec.field "interpreted" .interpreted Codec.string
                    |> Codec.field "shapes" .shapes (Codec.list coloredShapesCodec)
                    |> Codec.buildObject
                )
            )
        |> Codec.buildObject


type alias ColoredShapes =
    { color : String
    , shapes : List Shape
    }


coloredShapesCodec : Codec ColoredShapes
coloredShapesCodec =
    Codec.object ColoredShapes
        |> Codec.field "color" .color Codec.string
        |> Codec.field "shapes" .shapes (Codec.list shapeCodec)
        |> Codec.buildObject


type Shape
    = Rectangle { x : Float, y : Float, width : Float, height : Float }
    | Line { x1 : Float, y1 : Float, x2 : Float, y2 : Float }


rect : Float -> Float -> Float -> Float -> Shape
rect x y width height =
    Rectangle
        { x = x
        , y = y
        , width = width
        , height = height
        }


line : Float -> Float -> Float -> Float -> Shape
line x1 y1 x2 y2 =
    Line
        { x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        }


shapeCodec : Codec Shape
shapeCodec =
    Codec.custom
        (\frect fline value ->
            case value of
                Rectangle { x, y, width, height } ->
                    frect x y width height

                Line { x1, y1, x2, y2 } ->
                    fline x1 y1 x2 y2
        )
        |> Codec.variant4 "Rectangle" rect Codec.float Codec.float Codec.float Codec.float
        |> Codec.variant4 "Line" line Codec.float Codec.float Codec.float Codec.float
        |> Codec.buildCustom


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
    = PlotResponse PlotResult


workerResponseCodec : Codec WorkerResponse
workerResponseCodec =
    Codec.custom
        (\fplot value ->
            case value of
                PlotResponse r ->
                    fplot r
        )
        |> Codec.variant1 "PlotResponse" PlotResponse plotResultCodec
        |> Codec.buildCustom
