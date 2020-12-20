port module UI exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Codec exposing (Codec, Value)
import Dict
import Element exposing (Element, fill, height, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import Expression.Parser
import List.Extra as List
import Model exposing (Document, Flags, Model, Msg(..), Output(..))
import Task
import UI.RowView
import UI.Theme as Theme
import Zipper exposing (Zipper)


port save : Value -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view =
            Element.layout
                [ width fill
                , height fill
                , Font.size Theme.fontSize
                ]
                << view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        documents =
            case Codec.decodeValue Model.documentsCodec flags of
                Ok docs ->
                    docs

                Err _ ->
                    let
                        raw =
                            case Codec.decodeValue (Codec.dict Codec.string) flags of
                                Ok rowsDict ->
                                    rowsDict
                                        |> Dict.toList
                                        |> List.filterMap (\( k, v ) -> Maybe.map (\ik -> ( ik, v )) (String.toInt k))
                                        |> List.sortBy Tuple.first
                                        |> List.map Tuple.second
                                        |> List.filterNot String.isEmpty

                                Err _ ->
                                    []

                        rows =
                            if List.isEmpty raw then
                                default ++ [ "" ]

                            else
                                raw ++ [ "" ]
                    in
                    Just <|
                        Zipper.singleton
                            { name = "Untitled"
                            , rows = List.map emptyRow rows
                            , changed = False
                            }

        emptyRow x =
            { input = x
            , output = Empty
            }

        default =
            [ "{{plotsinx, plot(x<y), plot(x²+y²=3)}, {[zx+iy]plotexp(1/z), plot(x²+y²+z²=3), plot{sinx,x,-sinhx,-x,x²+y²=3,cosx,sinhx,-cosx,-sinx,x²+y²=4}}}"
            , "[zx+iy]{plot(z³-1),plotabs(z³-1),plotarg(z³-1)}"
            , "plot((sinx)²+(siny)²+(sinz)² = pw(z>1,-1,.5sinsqrt(x²+y²+z²)"
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
    ( { documents = documents
      , size = { width = 1024, height = 768 }
      }
    , Task.perform Resized measure
    )


view : Model -> Element Msg
view model =
    let
        documentPickerView =
            let
                btn selected msg label =
                    Input.button
                        [ Element.padding Theme.spacing
                        , Border.widthEach
                            { left = 1
                            , top = 1
                            , right =
                                if msg == NewDocument then
                                    1

                                else
                                    0
                            , bottom = 0
                            }
                        , Background.color <|
                            if selected then
                                Theme.colors.selectedDocument

                            else
                                Theme.colors.unselectedDocument
                        , Element.focused
                            [ Background.color <|
                                Theme.darken <|
                                    if selected then
                                        Theme.colors.selectedDocument

                                    else
                                        Theme.colors.unselectedDocument
                            ]
                        ]
                        { onPress = Just msg
                        , label = text label
                        }

                toBtn selected index doc =
                    btn selected (SelectDocument index) doc.name

                buttons =
                    Maybe.withDefault [] (Maybe.map (Zipper.map toBtn) model.documents)
                        ++ [ btn False NewDocument "+" ]
            in
            Element.row
                [ Element.paddingEach { top = Theme.spacing, left = Theme.spacing, right = Theme.spacing, bottom = 0 }
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , width fill
                ]
                buttons

        documentView =
            model.documents
                |> Maybe.map (Zipper.selected >> viewDocument model.size)
                |> Maybe.withDefault (Element.text "Select a document")
    in
    Theme.column [ width fill, height fill ]
        [ -- documentPickerView,
          documentView
        ]


viewDocument : { width : Int, height : Int } -> Document -> Element Msg
viewDocument size { rows } =
    Element.column
        [ Element.spacing <| 2 * Theme.spacing
        , Element.padding Theme.spacing
        , width fill
        ]
    <|
        List.indexedMap (\index row -> Element.Lazy.lazy3 UI.RowView.view size index row) rows


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input row input ->
            updateCurrent
                (\curr ->
                    let
                        rows_ =
                            curr.rows
                                |> List.updateAt row (\r -> { r | input = input })
                    in
                    { curr | rows = rows_, changed = True }
                )
                model

        Calculate row ->
            updateCurrent
                (\curr ->
                    let
                        rows_ =
                            List.updateAt row
                                (\r -> { r | output = parseOrError r.input })
                                curr.rows
                    in
                    { curr
                        | rows =
                            List.filterNot (String.isEmpty << .input) rows_
                                ++ [ Model.emptyRow ]
                    }
                )
                model

        NewDocument ->
            let
                newDocument =
                    { name = "Untitled"
                    , rows = [ Model.emptyRow ]
                    , changed = False
                    }
            in
            ( { model
                | documents =
                    Just <|
                        case model.documents of
                            Nothing ->
                                Zipper.singleton newDocument

                            Just docs ->
                                Zipper.append newDocument docs
                                    |> Zipper.allRight
              }
            , Cmd.none
            )

        SelectDocument i ->
            ( { model | documents = Maybe.map (Zipper.right i) model.documents }, Cmd.none )

        CloseDocument i ->
            ( { model | documents = Maybe.andThen (Zipper.removeAt i) model.documents }, Cmd.none )

        Resized size ->
            ( { model | size = size }, Cmd.none )


updateCurrent : (Document -> Document) -> Model -> ( Model, Cmd msg )
updateCurrent f model =
    case model.documents of
        Nothing ->
            ( model, Cmd.none )

        Just docs ->
            let
                doc =
                    f <| Zipper.selected docs

                documents =
                    Just <| Zipper.setSelected doc docs
            in
            ( { model | documents = documents }
            , save <| Codec.encodeToValue Model.documentsCodec documents
            )


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
