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
import Google exposing (AccessToken(..), Request(..))
import Html
import Html.Attributes
import List
import List.Extra as List
import Maybe
import Model exposing (CellMsg(..), Context, DocumentId(..), Flags, Output(..), RowData(..))
import Task
import UI.L10N as L10N exposing (L10N, Language(..), text, title)
import UI.Model exposing (Document, Modal(..), Model, Msg(..))
import UI.Ports
import UI.RowView
import UI.Theme as Theme exposing (onKey)
import Url exposing (Url)
import Url.Parser
import Zipper exposing (Zipper)


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


defaultDocument : List String
defaultDocument =
    [ "{{plotsinx, plot(x<y), plot(x²+y²=3)}, {[zx+iy]plotexp(1/z), plot(x²+y²+z²=3), plot{sinx,x,-sinhx,-x,x²+y²=3,cosx,sinhx,-cosx,-sinx,x²+y²=4}}}"
    , "[zx+iy]{plot(z³-1),plotabs(z³-1),plotarg(z³-1)}"
    , "plot(sin²x+sin²y+sin²z = pw(z>1,-1,.5sinsqrt(x²+y²+z²"
    , "plot{z=sin(x²+y²),x²+y²+(z-2)² = 3}"
    , "simplifydet{{a,b,c},{0,1,0},{1,2,3"
    , "solve(x+2(x-(x²-6)+1),x"
    , ""
    ]


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init ({ saved, hasClipboard, languages } as flags) url key =
    let
        documents : Maybe (Zipper { id : DocumentId, document : Document })
        documents =
            case Codec.decodeValue UI.Model.documentsCodec saved of
                Ok docs ->
                    docs
                        |> Maybe.map
                            (Zipper.map
                                (\i d ->
                                    { document = d
                                    , id =
                                        DocumentId <|
                                            if i < 0 then
                                                -i * 2 - 1

                                            else
                                                i * 2
                                    }
                                )
                            )

                Err _ ->
                    Just <|
                        Zipper.singleton
                            { id = DocumentId 0
                            , document =
                                { rows = List.map emptyRow defaultDocument
                                , metadata = UI.Model.emptyMetadata
                                }
                            }

        nextId =
            documents
                |> Maybe.map Zipper.toList
                |> Maybe.withDefault []
                |> List.map
                    (\d ->
                        let
                            (DocumentId i) =
                                d.id
                        in
                        i
                    )
                |> List.maximum
                |> Maybe.withDefault -1
                |> (+) 1

        emptyRow x =
            { input = x
            , editing = False
            , data = CodeRow []
            }

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
                            Missing

                        else
                            Present s
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

        google : Google.Model
        google =
            { accessToken = googleAccessToken
            , waitingId = []
            , waitingSave = []
            , errors = []
            }

        model : Model
        model =
            { documents = documents
            , nextId = nextId
            , size = defaultSize
            , modal = Nothing
            , openMenu = False
            , showPendingActions = False
            , rootUrl = rootUrl
            , key = key
            , google = google
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
    in
    ( model
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
                |> Maybe.map (Zipper.selected >> .document >> viewDocument model.size)
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
toolbar { google, openMenu, showPendingActions } =
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

        pendingActions =
            if
                List.any (\{ request } -> request == WaitingAccessToken) google.waitingId
                    || List.any (\{ request } -> request == WaitingAccessToken) google.waitingSave
            then
                [ ( { en = "Waiting for user to authenticate with Google"
                    , it = "In attesa che l'utente si autentichi con Google"
                    }
                  , Just GoogleAuth
                  )
                ]

            else
                List.map
                    (\{ name } ->
                        ( { en = "Waiting for an id " ++ name
                          , it = "In attesa di id " ++ name
                          }
                        , Nothing
                        )
                    )
                    google.waitingId
                    ++ List.map
                        (\{ name } ->
                            ( { en = "Waiting for save " ++ name
                              , it = "In attesa di salvataggio " ++ name
                              }
                            , Nothing
                            )
                        )
                        google.waitingSave

        pendingButton =
            if List.isEmpty pendingActions then
                let
                    _ =
                        Debug.log "no pending" {}
                in
                Element.none

            else
                let
                    _ =
                        Debug.log "pending" pendingActions
                in
                toolbarButton
                    (ToggleShowPendingActions <| not showPendingActions)
                    Icons.cloudSyncOutlined
                    { en = "Pending actions", it = "Azioni pendenti" }

        clearButton =
            toolbarButton
                ClearAll
                Icons.stopOutlined
                { en = "Clear document", it = "Pulisci documento" }
    in
    Element.row
        [ padding <| Theme.spacing // 4
        , if openMenu then
            Element.below <| dropdown ()

          else
            width shrink
        , if showPendingActions then
            Element.below <| viewPendingActions pendingActions

          else
            width shrink
        ]
        [ pendingButton, runButton, clearButton, moreButton ]


viewPendingActions : List ( L10N String, Maybe msg ) -> Element msg
viewPendingActions actions =
    let
        viewPendingAction ( txt, msg ) =
            case msg of
                Nothing ->
                    text txt

                Just m ->
                    Theme.row [ width fill ]
                        [ text txt
                        , Input.button
                            [ alignRight
                            , Element.htmlAttribute <| Html.Attributes.title "Do it"
                            ]
                            { onPress = Just m
                            , label = Element.element <| Icons.playSquareOutlined Theme.darkIconAttrs
                            }
                        ]
    in
    Element.column
        [ alignRight
        , Background.color Theme.colors.background
        , Border.width 1
        , padding Theme.spacing
        ]
        (List.intersperse hr <| List.map viewPendingAction actions)


hr : Element msg
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


dropdown : () -> Element Msg
dropdown () =
    let
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
                                List.foldl (\e -> el [ inFront e ]) mainIcon layers
                        , text lbl
                        ]
                }

        simpleBtn msg icon lbl =
            btn msg
                [ Element.element <| icon Theme.darkIconAttrs ]
                lbl

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

        buttons =
            [ simpleBtn OpenFile Icons.folderOpenOutlined { en = "Open", it = "Apri" }
            , simpleBtn SaveFile Icons.saveOutlined { en = "Save", it = "Salva" }
            , Element.with .expandIntervals expandIntervalsButton
            , simpleBtn GoogleSave Icons.uploadOutlined { en = "Save on Google Drive", it = "Salva su Google Drive" }
            , languagesRow
            ]
    in
    Element.column
        [ alignRight
        , Background.color Theme.colors.background
        , Border.width 1
        ]
        (List.intersperse hr buttons)


documentPicker : Model -> Element Msg
documentPicker ({ documents } as model) =
    let
        toDocumentTabButton : Int -> { a | document : Document } -> Element Msg
        toDocumentTabButton index { document } =
            let
                selected =
                    index == 0

                name =
                    -- TODO: Localize
                    Maybe.withDefault "Untitled" <| document.metadata.name
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
            Maybe.withDefault [] (Maybe.map (Zipper.toList << Zipper.map toDocumentTabButton) documents)
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

                Just { document } ->
                    wrap (CloseDocument { ask = False, index = i })
                        [ text <|
                            case document.metadata.name of
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
                                (Zipper.selected docs).document.metadata
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
                >> (\rs -> rs ++ [ UI.Model.emptyRow ])

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
                    addDocument model
                        { rows = [ UI.Model.emptyRow ]
                        , metadata = UI.Model.emptyMetadata
                        }

                SelectDocument i ->
                    let
                        documents =
                            Maybe.map (Zipper.right i) model.documents
                    in
                    ( { model | documents = documents }
                    , persist documents
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
                        ( { model
                            | documents = documents
                            , modal = Nothing
                          }
                        , persist documents
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
                                                            (Zipper.selected docs).document.metadata.name
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

                ToggleShowPendingActions showPendingActions ->
                    ( { model | showPendingActions = showPendingActions }, Cmd.none )

                OpenFile ->
                    ( model, File.Select.file [ "text/plain", ".txt" ] SelectedFileForOpen )

                SaveFile ->
                    case model.documents of
                        Nothing ->
                            ( model, Cmd.none )

                        Just docs ->
                            let
                                { document } =
                                    Zipper.selected docs
                            in
                            -- TODO: ask the user for a name if empty
                            ( model
                            , File.Download.string
                                (Maybe.withDefault "Untitled" document.metadata.name ++ ".txt")
                                "text/plain"
                                (UI.Model.documentToFile document)
                            )

                SelectedFileForOpen file ->
                    ( model, Task.perform (ReadFile (File.name file)) (File.toString file) )

                ReadFile name file ->
                    let
                        { errors, document } =
                            UI.Model.documentFromFile
                                (Just <|
                                    if String.endsWith ".txt" name then
                                        String.dropRight 4 name

                                    else
                                        name
                                )
                                file
                    in
                    addDocument model document

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

                GotGoogleFileIdFor docId res ->
                    case res of
                        Ok googleId ->
                            let
                                google =
                                    model.google

                                maybeDoc =
                                    model.google.waitingId
                                        |> List.find (\{ id } -> id == docId)

                                google_ { name, content } request =
                                    { google
                                        | waitingId = List.filter (\{ id } -> id /= docId) google.waitingId
                                        , waitingSave =
                                            { id = docId
                                            , name = name
                                            , content = content
                                            , googleId = googleId
                                            , request = request
                                            }
                                                :: google.waitingSave
                                    }

                                documents_ =
                                    Maybe.map
                                        (Zipper.map
                                            (\_ ({ id } as doc) ->
                                                if id == docId then
                                                    let
                                                        document =
                                                            doc.document

                                                        metadata =
                                                            doc.document.metadata
                                                    in
                                                    { doc
                                                        | document =
                                                            { document
                                                                | metadata =
                                                                    { metadata
                                                                        | googleId = Just googleId
                                                                    }
                                                            }
                                                    }

                                                else
                                                    doc
                                            )
                                        )
                                        model.documents
                            in
                            case maybeDoc of
                                Nothing ->
                                    ( model
                                    , Cmd.none
                                    )

                                Just data ->
                                    case google.accessToken of
                                        Missing ->
                                            ( { model
                                                | google = google_ data WaitingAccessToken
                                                , documents = documents_
                                              }
                                            , persist documents_
                                            )

                                        Expired ->
                                            ( { model
                                                | google = google_ data WaitingAccessToken
                                                , documents = documents_
                                              }
                                            , persist documents_
                                            )

                                        Present token ->
                                            ( { model
                                                | google = google_ data Running
                                                , documents = documents_
                                              }
                                            , Cmd.batch
                                                [ Task.attempt (GotGoogleSaveResultFor docId)
                                                    (Google.uploadFile
                                                        { id = googleId
                                                        , name = data.name
                                                        , content = data.content
                                                        , accessToken = token
                                                        }
                                                    )
                                                , persist documents_
                                                ]
                                            )

                        Err e ->
                            let
                                google =
                                    model.google

                                google_ =
                                    { google
                                        | waitingId =
                                            List.map
                                                (\({ id } as data) ->
                                                    if id == docId then
                                                        { data | request = Google.Errored e }

                                                    else
                                                        data
                                                )
                                                google.waitingId
                                    }
                            in
                            if e == Google.Unauthorized then
                                ( { model
                                    | google = { google_ | accessToken = Expired }
                                  }
                                , UI.Ports.saveGoogleAccessToken ""
                                )

                            else
                                ( { model | google = google_ }, Cmd.none )

                GotGoogleSaveResultFor docId res ->
                    let
                        google =
                            model.google

                        google_ =
                            { google
                                | waitingSave =
                                    List.map
                                        (\({ id } as data) ->
                                            if id == docId then
                                                { data
                                                    | request =
                                                        case res of
                                                            Ok () ->
                                                                Google.Succeded ()

                                                            Err e ->
                                                                Google.Errored e
                                                }

                                            else
                                                data
                                        )
                                        google.waitingSave
                            }
                    in
                    if res == Err Google.Unauthorized then
                        ( { model
                            | google = { google_ | accessToken = Expired }
                          }
                        , UI.Ports.saveGoogleAccessToken ""
                        )

                    else
                        ( { model | google = google_ }, Cmd.none )

                GotGoogleAccessToken token ->
                    let
                        google =
                            model.google

                        do ({ request } as data) =
                            if request == Google.WaitingAccessToken then
                                { data | request = Google.Running }

                            else
                                data

                        google_ =
                            { google
                                | waitingId = List.map do google.waitingId
                                , waitingSave = List.map do google.waitingSave
                                , accessToken = Google.Present token
                            }

                        model_ =
                            { model | google = google_ }

                        idRequests =
                            google.waitingId
                                |> List.filter (\{ request } -> request == Google.WaitingAccessToken)
                                |> List.map
                                    (\{ id } ->
                                        Task.attempt
                                            (GotGoogleFileIdFor id)
                                            (Google.generateId { accessToken = token })
                                    )

                        saveRequests =
                            google.waitingSave
                                |> List.filter (\{ request } -> request == Google.WaitingAccessToken)
                                |> List.map
                                    (\{ id, googleId, name, content } ->
                                        Task.attempt (GotGoogleSaveResultFor id)
                                            (Google.uploadFile
                                                { id = googleId
                                                , name = name
                                                , content = content
                                                , accessToken = token
                                                }
                                            )
                                    )
                    in
                    ( model_, Cmd.batch <| idRequests ++ saveRequests )

                Nop _ ->
                    ( model, Cmd.none )
    in
    -- TODO: Only do this for clicky actions
    \model -> inner { model | openMenu = False, showPendingActions = False }


addDocument : Model -> Document -> ( Model, Cmd Msg )
addDocument model newDocument =
    let
        documents =
            Just <|
                case model.documents of
                    Nothing ->
                        Zipper.singleton { id = DocumentId model.nextId, document = newDocument }

                    Just docs ->
                        Zipper.append { document = newDocument, id = DocumentId model.nextId } docs
                            |> Zipper.allRight
    in
    ( { model
        | documents = documents
        , nextId = model.nextId + 1
      }
    , persist documents
    )


persist : Maybe (Zipper { a | document : Document }) -> Cmd msg
persist documents =
    documents
        |> Maybe.map (Zipper.map (\_ { document } -> document))
        |> Codec.encodeToValue UI.Model.documentsCodec
        |> UI.Ports.persist


googleSave : Model -> ( Model, Cmd Msg )
googleSave model =
    case model.documents of
        Nothing ->
            ( model, Cmd.none )

        Just docs ->
            let
                { document, id } =
                    Zipper.selected docs

                content =
                    UI.Model.documentToFile document

                name =
                    -- TODO warn the user and ask for a name
                    Maybe.withDefault "Untitled" document.metadata.name

                data request =
                    { id = id
                    , name = name
                    , content = content
                    , request = request
                    }

                google =
                    model.google

                google_ request =
                    case document.metadata.googleId of
                        Nothing ->
                            { google
                                | waitingId =
                                    data request :: google.waitingId
                            }

                        Just googleId ->
                            { google
                                | waitingSave =
                                    { googleId = googleId
                                    , id = id
                                    , name = name
                                    , content = content
                                    , request = Google.mapRequest (\_ -> ()) request
                                    }
                                        :: google.waitingSave
                            }
            in
            case google.accessToken of
                Missing ->
                    ( { model
                        | modal = Just ModalGoogleAuth
                        , google = google_ WaitingAccessToken
                      }
                    , Cmd.none
                    )

                Expired ->
                    ( { model | google = google_ WaitingAccessToken }
                    , Cmd.none
                    )

                Present token ->
                    ( { model | google = google_ Running }
                    , case document.metadata.googleId of
                        Nothing ->
                            Task.attempt (GotGoogleFileIdFor id) <| Google.generateId { accessToken = token }

                        Just googleId ->
                            Task.attempt (GotGoogleSaveResultFor id) <|
                                Google.uploadFile
                                    { id = googleId
                                    , name = name
                                    , content = content
                                    , accessToken = token
                                    }
                    )


updateCurrent : (Document -> Document) -> Model -> ( Model, Cmd msg )
updateCurrent f model =
    case model.documents of
        Nothing ->
            ( model, Cmd.none )

        Just docs ->
            let
                selected =
                    Zipper.selected docs

                doc =
                    f selected.document

                documents =
                    Just <| Zipper.setSelected { document = doc, id = selected.id } docs
            in
            ( { model | documents = documents }
            , persist documents
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
