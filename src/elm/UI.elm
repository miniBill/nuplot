port module UI exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict
import Element exposing (Element, fill, height, width)
import Element.Font as Font
import Element.Lazy
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import Expression.Parser
import Json.Decode as JD
import List.Extra as List
import Model exposing (Flags, Model, Msg(..), Output(..))
import Task
import UI.RowView
import UI.Theme as Theme


port save : { key : String, value : String } -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view =
            Element.layout
                [ width fill
                , height fill
                , Font.size Theme.fontSize
                , Element.padding Theme.spacing
                ]
                << view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        parsed =
            flags
                |> JD.decodeValue (JD.dict JD.string)
                |> Result.map
                    (Dict.toList
                        >> List.filterMap (\( k, v ) -> Maybe.map (\ik -> ( ik, v )) (String.toInt k))
                        >> List.sortBy Tuple.first
                        >> List.map Tuple.second
                    )

        ex x =
            { input = x
            , output = Empty
            }

        default =
            [ "{{plotsinx, plot(x<y), plot(x²+y²=3)}, {[zx+iy]plotexp(1/z), plot(x²+y²+z²=3), plot{sinx,x,-sinhx,-x,x²+y²=3,cosx,sinhx,-cosx,-sinx,x²+y²=4}}}"
            , "[zx+iy]{plot(z³-1),plotabs(z³-1),plotarg(z³-1)}"
            , "plot({z=sin(x²+y²),x²+y²+(z-2)² = 3}"
            ]

        measure =
            Browser.Dom.getViewport
                |> Task.map
                    (\{ viewport } ->
                        { width = floor viewport.width
                        , height = floor viewport.height
                        }
                    )
    in
    ( { rows =
            parsed
                |> Result.withDefault []
                |> List.filterNot String.isEmpty
                |> (\l ->
                        if List.isEmpty l then
                            default

                        else
                            l
                   )
                |> (\l -> l ++ [ "" ])
                |> List.map ex
      , size = { width = 1024, height = 768 }
      }
    , Task.perform Resized measure
    )


view : Model -> Element Msg
view model =
    Element.column
        [ Element.spacing <| 2 * Theme.spacing
        , width fill
        ]
    <|
        List.indexedMap (\index row -> Element.Lazy.lazy3 UI.RowView.view model.size index row) model.rows


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input row input ->
            let
                rows_ =
                    List.updateAt row
                        (\r -> { r | input = input })
                        model.rows
            in
            ( { model | rows = rows_ }
            , rows_
                |> List.indexedMap (\i r -> save { key = String.fromInt i, value = r.input })
                |> Cmd.batch
            )

        Calculate row ->
            let
                rows_ =
                    List.updateAt row
                        (\r -> { r | output = parseOrError r.input })
                        model.rows
            in
            ( { model
                | rows =
                    List.filterNot (String.isEmpty << .input) rows_
                        ++ [ Model.emptyRow ]
              }
            , Cmd.none
            )

        Resized size ->
            ( { model | size = size }, Cmd.none )


parseOrError : String -> Output
parseOrError input =
    if String.isEmpty input then
        Empty

    else
        case Expression.Parser.parse input of
            Ok e ->
                Parsed e

            Err e ->
                ParseError <| Expression.Parser.errorsToString input e


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> Resized { width = w, height = h })
