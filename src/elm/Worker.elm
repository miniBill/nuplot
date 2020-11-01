port module Worker exposing (main)

import Model exposing (PlotResult)


port plot : (String -> msg) -> Sub msg


port plotted : PlotResult -> Cmd msg


type Msg
    = Plot String


type alias Flags =
    {}


type alias Model =
    ()


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg () =
    case msg of
        Plot expression ->
            ( ()
            , plotted
                { expression = expression
                , lines = []
                }
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    plot Plot
