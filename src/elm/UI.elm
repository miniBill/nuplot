port module UI exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Codec exposing (Value)
import Element exposing (Element, alignRight, centerX, centerY, el, fill, height, padding, paddingEach, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Expression exposing (Expression(..), RelationOperation(..))
import Expression.Parser
import Html
import List.Extra as List
import Model exposing (Document, Flags, Modal(..), Model, Msg(..), Output(..))
import Task
import UI.RowView
import UI.Theme as Theme exposing (onEnter)
import Zipper


port save : Value -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view =
            Element.layout
                [ width fill
                , Font.size Theme.fontSize
                , Background.color Theme.colors.background
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
                    Just <|
                        Zipper.singleton
                            { name = "Untitled"
                            , rows = List.map emptyRow <| default ++ [ "" ]
                            , changed = False
                            }

        emptyRow x =
            { input = x
            , output = Empty
            }

        default =
            [ "{{plotsinx, plot(x<y), plot(x²+y²=3)}, {[zx+iy]plotexp(1/z), plot(x²+y²+z²=3), plot{sinx,x,-sinhx,-x,x²+y²=3,cosx,sinhx,-cosx,-sinx,x²+y²=4}}}"
            , "[zx+iy]{plot(z³-1),plotabs(z³-1),plotarg(z³-1)}"
            , "plot(sin²x+sin²y+sin²z = pw(z>1,-1,.5sinsqrt(x²+y²+z²"
            , "plot{z=sin(x²+y²),x²+y²+(z-2)² = 3}"
            , "simplifydet{{a,b,c},{0,1,0},{1,2,3"
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
      , modal = Nothing
      }
    , Task.perform Resized measure
    )


view : Model -> Element Msg
view model =
    let
        documentPickerView =
            let
                focusStyle selected =
                    Element.focused
                        [ Background.color <|
                            Theme.darken <|
                                if selected then
                                    Theme.colors.selectedDocument

                                else
                                    Theme.colors.unselectedDocument
                        ]

                documentTabButton selected msg closeMsg label =
                    Input.button
                        [ Border.widthEach
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
                        , focusStyle selected
                        ]
                        { onPress = Just msg
                        , label =
                            case closeMsg of
                                Nothing ->
                                    el [ padding Theme.spacing ] <| text label

                                Just _ ->
                                    Element.row
                                        [ padding <| Theme.spacing // 2
                                        ]
                                        [ el [ padding <| Theme.spacing // 2 ] <| text label
                                        , Input.button
                                            [ padding <| Theme.spacing // 2
                                            , focusStyle selected
                                            , Background.color <|
                                                if selected then
                                                    Theme.colors.selectedDocument

                                                else
                                                    Theme.colors.unselectedDocument
                                            ]
                                            { onPress = closeMsg
                                            , label = text "X"
                                            }
                                        ]
                        }

                toDocumentTabButton selected index doc =
                    documentTabButton selected
                        (if selected then
                            RenameDocument Nothing

                         else
                            SelectDocument index
                        )
                        (Just <| CloseDocument { ask = True, index = index })
                        doc.name

                buttons =
                    Maybe.withDefault [] (Maybe.map (Zipper.map toDocumentTabButton) model.documents)
                        ++ [ documentTabButton False NewDocument Nothing "+" ]
            in
            Element.row
                [ paddingEach { top = Theme.spacing, left = Theme.spacing, right = Theme.spacing, bottom = 0 }
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , width fill
                ]
                buttons

        documentView =
            model.documents
                |> Maybe.map (Zipper.selected >> viewDocument model.size)
                |> Maybe.withDefault (Element.text "Select a document")
    in
    Theme.column
        [ width fill
        , height fill
        , Element.inFront <| viewModal model
        ]
        [ Element.html <|
            Html.node "style"
                []
                [ -- preserve whitespace in errors display
                  Html.text "div.pre > div {white-space: pre-wrap !important;}"
                ]
        , documentPickerView
        , documentView
        ]


viewModal : Model -> Element Msg
viewModal model =
    let
        wrap onOk e =
            el
                [ width fill
                , height fill
                , padding <| Theme.spacing * 8
                , Background.color <| Element.rgba 0.5 0.5 0.5 0.5
                ]
            <|
                el [ centerX, centerY, Background.color Theme.colors.background ] <|
                    el [ padding Theme.spacing ] <|
                        Theme.column []
                            (e
                                ++ [ Theme.row [ alignRight ]
                                        [ Input.button []
                                            { onPress = Just onOk
                                            , label = text "Ok"
                                            }
                                        , Input.button [] { onPress = Just DismissModal, label = text "Cancel" }
                                        ]
                                   ]
                            )
    in
    case ( model.modal, model.documents ) of
        ( Nothing, _ ) ->
            Element.none

        ( Just (ModalClose _), Nothing ) ->
            Element.none

        ( Just (ModalRename _), Nothing ) ->
            Element.none

        ( Just (ModalClose i), Just docs ) ->
            case Zipper.get i docs of
                Nothing ->
                    Element.none

                Just { name } ->
                    wrap (CloseDocument { ask = False, index = i })
                        [ text <| "Close document " ++ name ++ "?"
                        ]

        ( Just (ModalRename name), Just docs ) ->
            wrap (RenameDocument <| Just name)
                [ Input.text [ onEnter <| RenameDocument <| Just name ]
                    { onChange = SetModal << ModalRename
                    , text = name
                    , placeholder = Nothing
                    , label = Input.labelAbove [] <| text <| "Rename document \"" ++ (Zipper.selected docs).name ++ "\" to"
                    }
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
                                |> List.filterNot (String.isEmpty << .input)
                                |> (\rs -> rs ++ [ Model.emptyRow ])
                    in
                    { curr | rows = rows_, changed = True }
                )
                model

        Calculate row ->
            updateCurrent
                (\curr ->
                    let
                        rows_ =
                            curr.rows
                                |> List.updateAt row (\r -> { r | output = parseOrError r.input })
                                |> List.filterNot (String.isEmpty << .input)
                                |> (\rs -> rs ++ [ Model.emptyRow ])
                    in
                    { curr | rows = rows_ }
                )
                model

        NewDocument ->
            let
                newDocument =
                    { name = "Untitled"
                    , rows = [ Model.emptyRow ]
                    , changed = False
                    }

                documents =
                    Just <|
                        case model.documents of
                            Nothing ->
                                Zipper.singleton newDocument

                            Just docs ->
                                Zipper.append newDocument docs
                                    |> Zipper.allRight
            in
            ( { model | documents = documents }
            , save <| Codec.encodeToValue Model.documentsCodec documents
            )

        SelectDocument i ->
            let
                documents =
                    Maybe.map (Zipper.right i) model.documents
            in
            ( { model | documents = documents }
            , save <| Codec.encodeToValue Model.documentsCodec documents
            )

        CloseDocument { ask, index } ->
            if ask then
                if model.modal /= Nothing then
                    ( model, Cmd.none )

                else
                    ( { model | modal = Just <| ModalClose index }
                    , Cmd.none
                    )

            else
                let
                    documents =
                        Maybe.andThen (Zipper.removeAt index) model.documents
                in
                ( { model | documents = documents, modal = Nothing }
                , save <| Codec.encodeToValue Model.documentsCodec documents
                )

        RenameDocument n ->
            case n of
                Nothing ->
                    if model.modal /= Nothing then
                        ( model, Cmd.none )

                    else
                        case model.documents of
                            Nothing ->
                                ( model, Cmd.none )

                            Just docs ->
                                ( { model | modal = Just <| ModalRename (Zipper.selected docs).name }
                                , Cmd.none
                                )

                Just name ->
                    updateCurrent (\doc -> { doc | name = name }) { model | modal = Nothing }

        Resized size ->
            ( { model | size = size }, Cmd.none )

        DismissModal ->
            ( { model | modal = Nothing }, Cmd.none )

        SetModal m ->
            ( { model | modal = Just m }, Cmd.none )


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
