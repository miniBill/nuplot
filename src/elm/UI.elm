port module UI exposing (main)

import Bounce
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
                |> Result.map (Dict.filter (\k _ -> String.toInt k /= Nothing))
                |> Result.map Dict.values

        ex x =
            { input = x
            , output = parseOrError x
            , bounce = Bounce.init
            }

        default =
            [ "{{plotsinx, plot(x<y)}, {plot(x²+y²=3), [zx+iy]plotexp(1/z)},{plot(x²+y²+z²=3),plot{sinx,x,x²,absx,x²+y²=3,cosx,sinhx,-cosx,-sinx,x²+y²=4}}}"
            , "[zx+iy]{plot(z³-1),plotabs(z³-1),plotarg(z³-1)}"
            ]

        measure =
            Browser.Dom.getViewport
                |> Task.map (.viewport >> .width >> floor)
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
      , pageWidth = 1024
      }
    , Task.perform Width measure
    )


view : Model -> Element Msg
view model =
    Element.column
        [ Element.spacing <| 2 * Theme.spacing
        , width fill
        ]
    <|
        List.indexedMap (\index row -> Element.Lazy.lazy3 UI.RowView.view model.pageWidth index row) model.rows


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input { row, input } ->
            let
                rows_ =
                    model.rows
                        |> List.updateAt row
                            (\{ bounce, output } ->
                                { input = input
                                , output = toTyping output
                                , bounce = Bounce.push bounce
                                }
                            )
            in
            ( { model | rows = rows_ }
            , Bounce.delay 1000 (BounceMsg row)
            )

        BounceMsg row ->
            case List.getAt row model.rows of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just { input, bounce, output } ->
                    let
                        newBounce =
                            Bounce.pop bounce

                        rows_ =
                            model.rows
                                |> List.setAt row
                                    { input = input
                                    , bounce = newBounce
                                    , output =
                                        if Bounce.steady newBounce then
                                            parseOrError input

                                        else
                                            toTyping output
                                    }
                    in
                    ( { model
                        | rows =
                            List.filterNot (\r -> String.isEmpty r.input && Bounce.steady r.bounce) rows_
                                ++ [ Model.emptyRow ]
                      }
                    , rows_
                        |> List.indexedMap (\i r -> save { key = String.fromInt i, value = r.input })
                        |> Cmd.batch
                    )

        Width width ->
            ( { model | pageWidth = width }, Cmd.none )


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


toTyping : Output -> Output
toTyping output =
    Typing <|
        case output of
            Empty ->
                Nothing

            ParseError _ ->
                Nothing

            Typing e ->
                e

            Parsed e ->
                Just e


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w _ -> Width w)
