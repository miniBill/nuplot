port module UI exposing (main)

import Browser
import Codec
import Element exposing (fill, height, width)
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import List.Extra as List
import Model exposing (ColoredShapes, Flags, Model, Msg(..), RowResult(..), WorkerRequest(..), WorkerResponse(..), workerRequestCodec, workerResponseCodec)
import UI.View exposing (view)


port toWorker : String -> Cmd msg


send : WorkerRequest -> Cmd msg
send =
    Codec.encodeToString 0 workerRequestCodec >> toWorker


port fromWorker : (String -> msg) -> Sub msg


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
    ( List.map
        (\i ->
            { input = i
            , result = Calculating
            }
        )
        raw
    , Cmd.batch <| List.map (\i -> send <| PlotRequest i) raw
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Input { row, input } ->
            model
                |> List.updateAt row (\r -> { r | input = input })
                |> List.filter (not << String.isEmpty << .input)
                |> (\l ->
                        ( l
                            ++ [ { input = ""
                                 , result = Empty
                                 }
                               ]
                        , send <| PlotRequest input
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
        , shapes : List ColoredShapes
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
