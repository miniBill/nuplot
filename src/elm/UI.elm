module UI exposing (main)

import Ant.Icon as Icon
import Ant.Icons as Icons
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key)
import Codec
import Element.WithContext as Element exposing (DeviceClass(..), alignBottom, alignRight, centerX, centerY, el, fill, height, inFront, padding, scrollbarX, shrink, spacing, width)
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
import Google
import Html
import Html.Attributes
import List.Extra as List
import Model exposing (CellMsg(..), Context, Document, Flags, Modal(..), Model, Msg(..), Output(..), RowData(..))
import Task
import UI.L10N as L10N exposing (L10N, Language(..), text, title)
import UI.Ports
import UI.RowView
import UI.Theme as Theme exposing (onKey)
import Url exposing (Url)
import Url.Parser
import Zipper


type alias Element msg =
    Element.Element Context msg


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                { title = "nuPlot"
                , body =
                    [ Element.layout model.context
                        [ width fill
                        , Font.size Theme.fontSize
                        , Background.color Theme.colors.background
                        ]
                        (view model)
                    ]
                }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = always <| Nop "onUrlChange"
        , onUrlRequest = always <| Nop "onUrlRequest"
        }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init ({ saved, hasClipboard, languages } as flags) url key =
    let
        documents =
            case Codec.decodeValue Model.documentsCodec saved of
                Ok docs ->
                    docs

                Err _ ->
                    Just <|
                        Zipper.singleton
                            { rows = List.map emptyRow <| default ++ [ "" ]
                            , metadata = Model.emptyMetadata
                            }

        emptyRow x =
            { input = x
            , editing = False
            , data = CodeRow []
            }

        default =
            [ "{{plotsinx, plot(x<y), plot(x²+y²=3)}, {[zx+iy]plotexp(1/z), plot(x²+y²+z²=3), plot{sinx,x,-sinhx,-x,x²+y²=3,cosx,sinhx,-cosx,-sinx,x²+y²=4}}}"
            , "[zx+iy]{plot(z³-1),plotabs(z³-1),plotarg(z³-1)}"
            , "plot(sin²x+sin²y+sin²z = pw(z>1,-1,.5sinsqrt(x²+y²+z²"
            , "plot{z=sin(x²+y²),x²+y²+(z-2)² = 3}"
            , "simplifydet{{a,b,c},{0,1,0},{1,2,3"
            , "solve(x+2(x-(x²-6)+1),x"
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

        rootUrl =
            String.concat
                [ case url.protocol of
                    Url.Http ->
                        "http"

                    Url.Https ->
                        "https"
                , "://"
                , url.host
                , case url.port_ of
                    Nothing ->
                        ""

                    Just p ->
                        ":" ++ String.fromInt p
                ]

        googleAccessTokenFromUrl =
            Url.Parser.parse accessTokenParser url |> Maybe.andThen identity

        googleAccessToken =
            googleAccessTokenFromUrl
                |> Maybe.withDefault flags.googleAccessToken
                |> (\s ->
                        if String.isEmpty s then
                            Nothing

                        else
                            Just s
                   )

        accessTokenParser =
            Url.Parser.fragment <|
                Maybe.andThen
                    (String.split "&"
                        >> List.filterMap
                            (\p ->
                                case String.split "=" p of
                                    [ "access_token", v ] ->
                                        Just v

                                    _ ->
                                        Nothing
                            )
                        >> List.head
                    )

        defaultSize =
            { width = 1024, height = 768 }
    in
    ( { documents = documents
      , size = defaultSize
      , modal = Nothing
      , openMenu = False
      , rootUrl = rootUrl
      , key = key
      , googleAccessToken = googleAccessToken
      , pendingGoogleDriveSave = Nothing
      , context =
            { hasClipboard = hasClipboard
            , language =
                languages
                    |> List.map parseLanguage
                    |> List.filterMap identity
                    |> List.head
                    |> Maybe.withDefault En
            , expandIntervals = True

            -- The other classes have faster processors so should update quicker
            , deviceClass = Phone
            }
      }
    , case googleAccessTokenFromUrl of
        Nothing ->
            Task.perform Resized measure

        Just token ->
            UI.Ports.saveGoogleAccessTokenAndCloseWindow token
    )


view : Model -> Element Msg
view model =
    let
        documentView =
            model.documents
                |> Maybe.map (Zipper.selected >> viewDocument model.size)
                |> Maybe.withDefault (text { en = "Select a document", it = "Seleziona un documento" })
    in
    Element.column
        [ width fill
        , height fill
        , Element.inFront <| viewModal model
        ]
        [ Element.html <|
            Html.node "style"
                []
                [ -- preserve whitespace in errors display
                  Html.text
                    """
                        div.pre > div {
                            white-space: pre-wrap !important;
                        }

                        textarea.input {
                            word-break: break-word !important;
                        }"""
                ]
        , documentPicker model
        , documentView
        ]


toolbar : Model -> Element Msg
toolbar { openMenu } =
    let
        toolbarButton msg icon label =
            Input.button
                [ alignRight
                , centerY
                , padding Theme.spacing
                , Border.rounded 999
                , Element.mouseOver [ Background.color <| Theme.darken Theme.colors.background ]
                , title label
                ]
                { onPress = Just msg
                , label = Element.element <| icon Theme.darkIconAttrs
                }

        moreButton =
            toolbarButton
                (ToggleMenu <| not openMenu)
                Icons.moreOutlined
                { en = "Menu", it = "Menù" }

        runButton =
            toolbarButton
                CalculateAll
                Icons.playSquareOutlined
                { en = "Run document", it = "Esegui documento" }

        clearButton =
            toolbarButton
                ClearAll
                Icons.stopOutlined
                { en = "Clear document", it = "Pulisci documento" }

        bigStop =
            let
                bigWidth =
                    Theme.iconSize.standard * 4 // 3

                delta =
                    Theme.iconSize.standard / 6
            in
            el
                [ Element.moveUp delta
                , Element.moveLeft delta
                ]
            <|
                Element.element <|
                    Icons.stopOutlined <|
                        Theme.darkIconAttrs
                            ++ [ Icon.width bigWidth ]

        dropdown () =
            let
                btn msg icon lbl =
                    Input.button
                        [ padding Theme.spacing
                        , width fill
                        ]
                        { onPress = Just msg
                        , label =
                            Theme.row []
                                [ case icon of
                                    [] ->
                                        Element.none

                                    mainIcon :: layers ->
                                        List.foldl (\e a -> el [ inFront <| e ] a) mainIcon layers
                                , text lbl
                                ]
                        }

                simpleBtn msg icon lbl =
                    btn msg
                        [ Element.element <| icon Theme.darkIconAttrs ]
                        lbl

                hr =
                    el
                        [ width fill
                        , Border.widthEach
                            { top = 1
                            , left = 0
                            , right = 0
                            , bottom = 0
                            }
                        ]
                        Element.none

                vr =
                    el
                        [ height fill
                        , Border.widthEach
                            { top = 0
                            , bottom = 0
                            , right = 1
                            , left = 0
                            }
                        ]
                        Element.none

                languageButton lang flag =
                    el [ width fill ] <|
                        el [ centerX ] <|
                            btn (Language lang) [] <|
                                L10N.invariant flag

                languagesRow =
                    Element.row [ width fill ]
                        [ languageButton En "🇬🇧"
                        , vr
                        , languageButton It "🇮🇹"
                        ]

                expandIntervalsButton expandIntervals =
                    if expandIntervals then
                        btn (ExpandIntervals False)
                            [ Element.element <| Icons.funnelPlotOutlined Theme.darkIconAttrs
                            , bigStop
                            ]
                            { en = "Do not apply noise reduction", it = "Non applicare riduzione rumore" }

                    else
                        simpleBtn (ExpandIntervals True) Icons.funnelPlotOutlined { en = "Apply noise reduction", it = "Applica riduzione rumore" }
            in
            Element.column
                [ alignRight
                , Background.color Theme.colors.background
                , Border.width 1
                ]
            <|
                List.intersperse
                    hr
                    [ simpleBtn OpenFile Icons.folderOpenOutlined { en = "Open", it = "Apri" }
                    , simpleBtn SaveFile Icons.saveOutlined { en = "Save", it = "Salva" }
                    , Element.with .expandIntervals expandIntervalsButton

                    --, simpleBtn GoogleSave Icons.uploadOutlined { en = "Save on Google Drive", it = "Salva su Google Drive" }
                    , languagesRow
                    ]
    in
    Element.row
        [ padding <| Theme.spacing // 4
        , if openMenu then
            Element.below <| dropdown ()

          else
            width shrink
        ]
        [ runButton, clearButton, moreButton ]


documentPicker : Model -> Element Msg
documentPicker ({ documents } as model) =
    let
        toDocumentTabButton : Bool -> Int -> Document -> Element Msg
        toDocumentTabButton selected index doc =
            let
                name =
                    -- TODO: Localize
                    Maybe.withDefault "Untitled" <| doc.metadata.name
            in
            documentTabButton
                { selected = selected
                , onPress =
                    if selected then
                        RenameDocument Nothing

                    else
                        SelectDocument index
                , closeMsg = Just <| CloseDocument { ask = True, index = index }
                , label = text <| L10N.invariant <| ellipsize name
                , index = index
                , title = L10N.invariant name
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
        , spacing <| Theme.spacing // 2
        ]
        [ Element.row
            [ alignBottom
            , spacing <| Theme.spacing // 2
            , scrollbarX
            , width fill
            ]
            buttons
        , toolbar model
        ]


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


documentTabButton : { a | selected : Bool, onPress : msg, closeMsg : Maybe msg, label : Element msg, title : L10N String } -> Element msg
documentTabButton { selected, onPress, closeMsg, label, title } =
    let
        focusStyle =
            Element.focused
                [ Background.color <|
                    Theme.darken <|
                        if selected then
                            Theme.colors.selectedDocument

                        else
                            Theme.colors.unselectedDocument
                ]
    in
    Input.button
        [ Border.roundEach
            { topLeft = Theme.roundness
            , topRight = Theme.roundness
            , bottomLeft = 0
            , bottomRight = 0
            }
        , Border.widthEach
            { left = 1
            , top = 1
            , right = 1
            , bottom = 0
            }
        , Background.color <|
            if selected then
                Theme.colors.selectedDocument

            else
                Theme.colors.unselectedDocument
        , focusStyle
        , L10N.title title
        , alignBottom
        ]
        { onPress = Just onPress
        , label =
            Element.row
                [ padding <| Theme.spacing // 2 ]
                [ el [ padding <| Theme.spacing // 2 ] label
                , case closeMsg of
                    Nothing ->
                        Element.none

                    Just _ ->
                        Input.button
                            [ padding <| Theme.spacing // 2
                            , focusStyle
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

                Just { metadata } ->
                    wrap (CloseDocument { ask = False, index = i })
                        [ text <|
                            case metadata.name of
                                Just name ->
                                    { en = "Close document \"" ++ name ++ "\"?"
                                    , it = "Chiudi il documento \"" ++ name ++ "\"?"
                                    }

                                Nothing ->
                                    { en = "Close the document?"
                                    , it = "Chiudere il documento?"
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
                        let
                            metadata =
                                (Zipper.selected docs).metadata
                        in
                        Input.labelAbove [] <|
                            text <|
                                case metadata.name of
                                    Just n ->
                                        { en = "Rename document \"" ++ n ++ "\" to"
                                        , it = "Rinomina il documento \"" ++ n ++ "\" in"
                                        }

                                    Nothing ->
                                        { en = "Rename the document to"
                                        , it = "Rinomina il documento in"
                                        }
                    }
                ]

        ( Just ModalGoogleAuth, _ ) ->
            wrap GoogleAuth
                [ text
                    { en = "Before completing this action you will need to authenticate with Google"
                    , it = "Prima di poter effettuare questa azione è necessario effettuare l'autenticazione con Google"
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
        calculateRow r =
            case r.data of
                CodeRow _ ->
                    { r | editing = False, data = CodeRow <| List.filterMap parseOrError <| String.split "\n" r.input }

                MarkdownRow ->
                    { r | editing = False }

        filterEmpty =
            List.filterNot (.input >> String.isEmpty)
                >> (\rs -> rs ++ [ Model.emptyRow ])

        inner model =
            case msg of
                CellMsg row cmsg ->
                    let
                        updateRow rowF =
                            updateCurrent
                                (\curr ->
                                    { curr
                                        | rows =
                                            curr.rows
                                                |> List.updateAt row rowF
                                                |> filterEmpty
                                    }
                                )
                                model
                    in
                    case cmsg of
                        Copy id ->
                            ( model, UI.Ports.copy id )

                        Save id ->
                            ( model, UI.Ports.save id )

                        Input input ->
                            updateRow (\r -> { r | input = input })

                        Calculate ->
                            updateRow calculateRow

                        StartEditing ->
                            updateRow (\r -> { r | editing = True })

                        EndEditing ->
                            updateRow (\r -> { r | editing = False })

                        ToCode ->
                            updateRow (\r -> { r | data = CodeRow [] })

                        ToMarkdown ->
                            updateRow (\r -> { r | data = MarkdownRow })

                        Clear ->
                            updateRow (\r -> { r | data = CodeRow [] })

                NewDocument ->
                    let
                        newDocument =
                            { rows = [ Model.emptyRow ]
                            , metadata = Model.emptyMetadata
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
                    , UI.Ports.persist <| Codec.encodeToValue Model.documentsCodec documents
                    )

                SelectDocument i ->
                    let
                        documents =
                            Maybe.map (Zipper.right i) model.documents
                    in
                    ( { model | documents = documents }
                    , UI.Ports.persist <| Codec.encodeToValue Model.documentsCodec documents
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
                        , UI.Ports.persist <| Codec.encodeToValue Model.documentsCodec documents
                        )

                CalculateAll ->
                    updateCurrent (\doc -> { doc | rows = List.map calculateRow doc.rows }) model

                ClearAll ->
                    updateCurrent
                        (\doc ->
                            { doc
                                | rows =
                                    List.map
                                        (\row ->
                                            case row.data of
                                                CodeRow _ ->
                                                    { row | data = CodeRow [] }

                                                MarkdownRow ->
                                                    row
                                        )
                                        doc.rows
                            }
                        )
                        model

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
                                        ( { model
                                            | modal =
                                                Just <|
                                                    ModalRename <|
                                                        Maybe.withDefault ""
                                                            (Zipper.selected docs).metadata.name
                                          }
                                        , Cmd.none
                                        )

                        Just name ->
                            updateCurrent
                                (\doc ->
                                    let
                                        meta =
                                            doc.metadata
                                    in
                                    { doc | metadata = { meta | name = Just name } }
                                )
                                { model | modal = Nothing }

                Resized size ->
                    let
                        context_ =
                            model.context
                    in
                    ( { model | size = size, context = { context_ | deviceClass = (Element.classifyDevice size).class } }, Cmd.none )

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
                            -- TODO: ask the user for a name if empty
                            ( model, File.Download.string (Maybe.withDefault "Untitled" doc.metadata.name ++ ".txt") "text/plain" <| Model.documentToFile doc )

                SelectedFileForOpen file ->
                    ( model, Task.perform (ReadFile (File.name file)) (File.toString file) )

                ReadFile name file ->
                    let
                        { errors, document } =
                            Model.documentFromFile
                                (Just <|
                                    if String.endsWith ".txt" name then
                                        String.dropRight 4 name

                                    else
                                        name
                                )
                                file
                    in
                    ( { model
                        | documents =
                            model.documents
                                |> Maybe.map (Zipper.append document)
                                |> Maybe.withDefault (Zipper.singleton document)
                                |> Zipper.allRight
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

                ExpandIntervals expandIntervals ->
                    let
                        context =
                            model.context
                    in
                    ( { model | context = { context | expandIntervals = expandIntervals } }, Cmd.none )

                GoogleAuth ->
                    ( { model | modal = Nothing }, Google.startAuthenticationFlow model.rootUrl )

                GoogleSave ->
                    googleSave model

                GoogleSaved res ->
                    let
                        _ =
                            Debug.log "GoogleSaved" res
                    in
                    ( model, Cmd.none )

                GotGoogleAccessToken token ->
                    let
                        model_ =
                            { model
                                | googleAccessToken =
                                    if token == "" then
                                        Nothing

                                    else
                                        Just token
                                , pendingGoogleDriveSave = Nothing
                            }
                    in
                    case model.pendingGoogleDriveSave of
                        Nothing ->
                            ( model_, Cmd.none )

                        Just data ->
                            ( model_, doGoogleSave { googleAccessToken = token } data )

                Nop _ ->
                    ( model, Cmd.none )
    in
    \model -> inner { model | openMenu = False }


googleSave : Model -> ( Model, Cmd Msg )
googleSave model =
    case model.documents of
        Nothing ->
            ( model, Cmd.none )

        Just docs ->
            let
                document =
                    Zipper.selected docs

                content =
                    Model.documentToFile document

                name =
                    -- TODO warn the user and ask for a name
                    Maybe.withDefault "Untitled" document.metadata.name

                data =
                    { name = name
                    , content = content
                    }
            in
            case model.googleAccessToken of
                Nothing ->
                    ( { model
                        | modal = Just ModalGoogleAuth
                        , pendingGoogleDriveSave = Just data
                      }
                    , Cmd.none
                    )

                Just googleAccessToken ->
                    ( model
                    , doGoogleSave { googleAccessToken = googleAccessToken } data
                    )


doGoogleSave : { a | googleAccessToken : String } -> { b | name : String, content : String } -> Cmd Msg
doGoogleSave { googleAccessToken } { name, content } =
    let
        cmd =
            Google.generateId { googleAccessToken = googleAccessToken }
                |> Task.andThen
                    (\id ->
                        Google.uploadFile
                            { id = id
                            , name = name
                            , content = content
                            , googleAccessToken = googleAccessToken
                            }
                    )
                |> Task.attempt GoogleSaved
    in
    cmd


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
            , UI.Ports.persist <| Codec.encodeToValue Model.documentsCodec documents
            )


parseOrError : String -> Maybe Output
parseOrError input =
    if String.isEmpty input then
        Nothing

    else
        Just <|
            case Expression.Parser.parse input of
                Ok e ->
                    Parsed e

                Err e ->
                    ParseError <| Expression.Parser.errorsToString input e


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\w h -> Resized { width = w, height = h })
        , UI.Ports.gotGoogleAccessToken GotGoogleAccessToken
        ]
