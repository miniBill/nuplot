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
import File
import File.Select
import Html
import List.Extra as List
import Model exposing (Document, Flags, Menu(..), Modal(..), Model, Msg(..), Output(..), Row(..))
import Task
import UI.RowView
import UI.Theme as Theme exposing (onEnter)
import Zipper exposing (Zipper)


port persist : Value -> Cmd msg


port copy : String -> Cmd msg


port save : String -> Cmd msg


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
init { saved, hasClipboard } =
    let
        documents =
            case Codec.decodeValue Model.documentsCodec saved of
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
            CodeRow
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
      , hasClipboard = hasClipboard
      , openMenu = Nothing
      }
    , Task.perform Resized measure
    )


view : Model -> Element Msg
view model =
    let
        documentView =
            model.documents
                |> Maybe.map (Zipper.selected >> viewDocument model.hasClipboard model.size)
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
        , toolbar model
        , documentPicker model.documents
        , documentView
        ]


toolbar : Model -> Element Msg
toolbar { openMenu } =
    Theme.row [ width fill ]
        [ Input.button
            [ Border.widthEach { top = 0, left = 0, right = 1, bottom = 1 }
            , padding Theme.spacing
            , Element.below <|
                if openMenu == Just File then
                    Element.column [ Background.color Theme.colors.background ]
                        [ Input.button
                            [ Border.widthEach { top = 1, left = 0, right = 1, bottom = 1 }
                            , padding Theme.spacing
                            ]
                            { onPress = Just Open
                            , label = text "Open"
                            }
                        ]

                else
                    Element.none
            ]
            { onPress = Just <| ToggleMenu File
            , label = text "File"
            }
        ]


documentPicker : Maybe (Zipper Document) -> Element Msg
documentPicker documents =
    let
        toDocumentTabButton selected index doc =
            documentTabButton
                { selected = selected
                , onPress =
                    if selected then
                        RenameDocument Nothing

                    else
                        SelectDocument index
                , closeMsg = Just <| CloseDocument { ask = True, index = index }
                , label = doc.name
                , index = index
                }

        buttons =
            Maybe.withDefault [] (Maybe.map (Zipper.map toDocumentTabButton) documents)
                ++ [ documentTabButton
                        { selected = False
                        , onPress = NewDocument
                        , closeMsg = Nothing
                        , label = "+"
                        , index = -1
                        }
                   ]
    in
    Element.row
        [ Element.paddingXY Theme.spacing 0
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , width fill
        ]
        buttons


documentTabButton : { a | selected : Bool, onPress : msg, closeMsg : Maybe msg, label : String, index : number } -> Element msg
documentTabButton { selected, onPress, closeMsg, label, index } =
    let
        focusStyle s =
            Element.focused
                [ Background.color <|
                    Theme.darken <|
                        if s then
                            Theme.colors.selectedDocument

                        else
                            Theme.colors.unselectedDocument
                ]
    in
    Input.button
        [ Border.roundEach
            { topLeft =
                if index == 0 then
                    Theme.roundness

                else
                    0
            , topRight =
                if index < 0 then
                    Theme.roundness

                else
                    0
            , bottomLeft = 0
            , bottomRight = 0
            }
        , Border.widthEach
            { left = 1
            , top = 1
            , right =
                if index < 0 then
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
        { onPress = Just onPress
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


viewDocument : Bool -> { width : Int, height : Int } -> Document -> Element Msg
viewDocument hasClipboard size { rows } =
    let
        rowsViews =
            List.indexedMap (\index row -> Element.Lazy.lazy4 UI.RowView.view hasClipboard size index row) rows
    in
    Element.column
        [ Element.spacing <| 2 * Theme.spacing
        , Element.padding Theme.spacing
        , width fill
        , height fill
        , Element.scrollbarY
        ]
        (rowsViews ++ [ el [ height fill ] Element.none ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateCalcRow row =
            List.filterNot
                (\r ->
                    case r of
                        CodeRow { input } ->
                            String.isEmpty input

                        MarkdownRow i ->
                            String.isEmpty i
                )
                >> (\rs -> rs ++ [ Model.emptyRow ])
    in
    case msg of
        Input row input ->
            updateCurrent
                (\curr ->
                    { curr
                        | rows =
                            curr.rows
                                |> List.updateAt row
                                    (\r ->
                                        case r of
                                            CodeRow cr ->
                                                CodeRow { cr | input = input }

                                            MarkdownRow _ ->
                                                if String.startsWith ">" input then
                                                    CodeRow
                                                        { input = String.dropLeft 1 input
                                                        , output = Empty
                                                        }

                                                else
                                                    MarkdownRow input
                                    )
                                |> updateCalcRow row
                        , changed = True
                    }
                )
                model

        Calculate row ->
            updateCurrent
                (\curr ->
                    { curr
                        | rows =
                            curr.rows
                                |> List.updateAt row
                                    (\r ->
                                        case r of
                                            CodeRow cr ->
                                                CodeRow { cr | output = parseOrError cr.input }

                                            MarkdownRow _ ->
                                                r
                                    )
                                |> updateCalcRow row
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
            , persist <| Codec.encodeToValue Model.documentsCodec documents
            )

        SelectDocument i ->
            let
                documents =
                    Maybe.map (Zipper.right i) model.documents
            in
            ( { model | documents = documents }
            , persist <| Codec.encodeToValue Model.documentsCodec documents
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
                , persist <| Codec.encodeToValue Model.documentsCodec documents
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

        Copy id ->
            ( model, copy id )

        Save id ->
            ( model, save id )

        ToggleMenu menu ->
            if model.openMenu == Just menu then
                ( { model | openMenu = Nothing }, Cmd.none )

            else
                ( { model | openMenu = Just menu }, Cmd.none )

        Open ->
            ( model, File.Select.file [ "text/plain", ".txt" ] SelectedFileForOpen )

        SelectedFileForOpen file ->
            ( model, Task.perform (ReadFile (File.name file)) (File.toString file) )

        ReadFile name file ->
            let
                document =
                    Model.documentFromFile name file
            in
            ( { model
                | documents =
                    model.documents
                        |> Maybe.map (Zipper.append document)
                        |> Maybe.withDefault (Zipper.singleton document)
                        |> Just
              }
            , Cmd.none
            )


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
            , persist <| Codec.encodeToValue Model.documentsCodec documents
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
