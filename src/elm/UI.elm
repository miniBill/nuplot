port module UI exposing (main)

import Ant.Icons as Icons
import Browser
import Browser.Dom
import Browser.Events
import Codec exposing (Value)
import Element.WithContext as Element exposing (alignRight, centerX, centerY, el, fill, height, padding, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Element.WithContext.Lazy as Lazy
import Expression exposing (Expression(..), RelationOperation(..))
import Expression.Parser
import File
import File.Download
import File.Select
import Html
import Html.Attributes
import List.Extra as List
import Model exposing (CellMsg(..), Context, Document, Flags, Language(..), Modal(..), Model, Msg(..), Output(..), RowData(..))
import Task
import UI.L10N as L10N exposing (L10N, text, title)
import UI.RowView
import UI.Theme as Theme exposing (onKey)
import Zipper


type alias Element msg =
    Element.Element Context msg


port persist : Value -> Cmd msg


port copy : String -> Cmd msg


port save : String -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view =
            \model ->
                Element.layout model.context
                    [ width fill
                    , Font.size Theme.fontSize
                    , Background.color Theme.colors.background
                    ]
                    (view model)
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { saved, hasClipboard, languages } =
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
            { input = x
            , editing = False
            , data = CodeRow Empty
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

        parseLanguage lang =
            case String.split "-" lang of
                "it" :: _ ->
                    Just It

                "en" :: _ ->
                    Just En

                _ ->
                    Nothing
    in
    ( { documents = documents
      , size = { width = 1024, height = 768 }
      , modal = Nothing
      , openMenu = False
      , context =
            { hasClipboard = hasClipboard
            , language =
                languages
                    |> List.map parseLanguage
                    |> List.filterMap identity
                    |> List.head
                    |> Maybe.withDefault En
            }
      }
    , Task.perform Resized measure
    )


view : Model -> Element Msg
view model =
    let
        documentView =
            model.documents
                |> Maybe.map (Zipper.selected >> viewDocument model.size)
                |> Maybe.withDefault (text { en = "Select a document", it = "Seleziona un documento" })
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
        , documentPicker model
        , documentView
        ]


toolbar : Model -> Element Msg
toolbar { openMenu } =
    let
        moreButton =
            Input.button
                [ alignRight
                , padding <| Theme.spacing // 2
                , Border.rounded 999
                ]
                { onPress = Just <| ToggleMenu <| not openMenu
                , label =
                    Element.element <| Icons.moreOutlined Theme.smallDarkIconAttrs
                }

        topBorder =
            Border.widthEach { top = 1, left = 0, right = 0, bottom = 0 }

        dropdown () =
            let
                btn msg lbl =
                    Input.button [ padding Theme.spacing ]
                        { onPress = Just msg
                        , label = text lbl
                        }
            in
            Element.column [ alignRight, Background.color Theme.colors.background, Border.width 1 ]
                [ btn OpenFile { en = "Open", it = "Apri" }
                , btn SaveFile { en = "Save", it = "Salva" }
                , Element.row [ topBorder ]
                    [ btn (Language En) <| L10N.invariant "🇬🇧"
                    , btn (Language It) <| L10N.invariant "🇮🇹"
                    ]
                ]
    in
    el
        [ width fill
        , padding <| Theme.spacing // 2
        , if openMenu then
            Element.below <| dropdown ()

          else
            title { en = "Menu", it = "Menù" }
        ]
        moreButton


documentPicker : Model -> Element Msg
documentPicker ({ documents } as model) =
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
                , label = text <| L10N.invariant <| ellipsize doc.name
                , index = index
                , title = L10N.invariant doc.name
                }

        buttons =
            Maybe.withDefault [] (Maybe.map (Zipper.map toDocumentTabButton) documents)
                ++ [ documentTabButton
                        { selected = False
                        , onPress = NewDocument
                        , closeMsg = Nothing
                        , label =
                            Element.row []
                                [ -- This text element forces the button height
                                  text <| L10N.invariant "\u{200B}"
                                , Element.element <| Icons.fileAddTwoTone Theme.smallDarkIconAttrs
                                ]
                        , index = -1
                        , title = { en = "New document", it = "Nuovo documento" }
                        }
                   ]
    in
    Element.row
        [ Element.paddingEach { left = Theme.spacing, right = Theme.spacing, top = Theme.spacing, bottom = 0 }
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , width fill
        ]
    <|
        buttons
            ++ [ toolbar model ]


ellipsize : String -> String
ellipsize s =
    let
        maxLen =
            15
    in
    if String.length s < maxLen then
        s

    else
        String.left maxLen s ++ "..."


documentTabButton : { a | selected : Bool, onPress : msg, closeMsg : Maybe msg, label : Element msg, index : number, title : L10N String } -> Element msg
documentTabButton { selected, onPress, closeMsg, label, title, index } =
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
        , L10N.title title
        ]
        { onPress = Just onPress
        , label =
            Element.row
                [ padding <| Theme.spacing // 2
                ]
                [ el [ padding <| Theme.spacing // 2 ] label
                , case closeMsg of
                    Nothing ->
                        Element.none

                    Just _ ->
                        Input.button
                            [ padding <| Theme.spacing // 2
                            , focusStyle selected
                            , Background.color <|
                                if selected then
                                    Theme.colors.selectedDocument

                                else
                                    Theme.colors.unselectedDocument
                            ]
                            { onPress = closeMsg
                            , label = Element.element <| Icons.closeOutlined Theme.smallDarkIconAttrs
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
                , Background.color Theme.colors.modalTransparentBackground
                , Element.htmlAttribute <| Html.Attributes.style "backdrop-filter" "blur(2px)"
                ]
            <|
                el
                    [ centerX
                    , centerY
                    , Background.color Theme.colors.background
                    , Border.rounded Theme.roundness
                    ]
                <|
                    el [ padding Theme.spacing ] <|
                        Theme.column []
                            (e
                                ++ [ Theme.row [ alignRight ]
                                        [ Input.button [ padding Theme.spacing ]
                                            { onPress = Just onOk
                                            , label = text { en = "Ok", it = "Ok" }
                                            }
                                        , Input.button [ padding Theme.spacing ]
                                            { onPress = Just DismissModal
                                            , label = text { en = "Cancel", it = "Annulla" }
                                            }
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
                        [ text
                            { en = "Close document \"" ++ name ++ "\"?"
                            , it = "Chiudi il documento \"" ++ name ++ "\"?"
                            }
                        ]

        ( Just (ModalRename name), Just docs ) ->
            wrap (RenameDocument <| Just name)
                [ Input.text
                    [ Input.focusedOnLoad
                    , onKey <|
                        \key ->
                            case key of
                                "Enter" ->
                                    Just <| RenameDocument <| Just name

                                "Escape" ->
                                    Just DismissModal

                                _ ->
                                    Nothing
                    ]
                    { onChange = SetModal << ModalRename
                    , text = name
                    , placeholder = Nothing
                    , label =
                        Input.labelAbove [] <|
                            text
                                { en = "Rename document \"" ++ (Zipper.selected docs).name ++ "\" to"
                                , it = "Rinomina il documento \"" ++ (Zipper.selected docs).name ++ "\" a"
                                }
                    }
                ]


viewDocument : { width : Int, height : Int } -> Document -> Element Msg
viewDocument size { rows } =
    let
        rowsViews =
            List.indexedMap (\index row -> Lazy.lazy3 UI.RowView.view size index row) rows
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
update msg =
    let
        filterEmpty row =
            List.filterNot (.input >> String.isEmpty)
                >> (\rs -> rs ++ [ Model.emptyRow ])

        inner model =
            case msg of
                CellMsg row cmsg ->
                    let
                        updateRow changed rowF =
                            updateCurrent
                                (\curr ->
                                    { curr
                                        | rows =
                                            curr.rows
                                                |> List.updateAt row rowF
                                                |> filterEmpty row
                                        , changed = curr.changed || changed
                                    }
                                )
                                model
                    in
                    case cmsg of
                        Copy id ->
                            ( model, copy id )

                        Save id ->
                            ( model, save id )

                        Input input ->
                            updateRow True (\r -> { r | input = input })

                        Calculate ->
                            updateRow False
                                (\r ->
                                    case r.data of
                                        CodeRow _ ->
                                            { r | editing = False, data = CodeRow <| parseOrError r.input }

                                        MarkdownRow ->
                                            { r | editing = False }
                                )

                        StartEditing ->
                            updateRow False (\r -> { r | editing = True })

                        EndEditing ->
                            updateRow False (\r -> { r | editing = False })

                        ToCode ->
                            updateRow True (\r -> { r | data = CodeRow Empty })

                        ToMarkdown ->
                            updateRow True (\r -> { r | data = MarkdownRow })

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

                ToggleMenu openMenu ->
                    ( { model | openMenu = openMenu }, Cmd.none )

                OpenFile ->
                    ( model, File.Select.file [ "text/plain", ".txt" ] SelectedFileForOpen )

                SaveFile ->
                    case model.documents of
                        Nothing ->
                            ( model, Cmd.none )

                        Just docs ->
                            let
                                doc =
                                    Zipper.selected docs
                            in
                            ( model, File.Download.string "export.txt" "text/plain" <| Model.documentToFile doc )

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

                Language language ->
                    let
                        context =
                            model.context
                    in
                    ( { model | context = { context | language = language } }, Cmd.none )
    in
    \model -> inner { model | openMenu = False }


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
