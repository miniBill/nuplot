module UI exposing (main)

import API.Google as Google exposing (AccessToken(..), Request(..))
import Ant.Icon as Icon
import Ant.Icons as Icons
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key)
import Codec
import Document exposing (Modal(..), Output(..), Row, RowData(..), StoredDocument, UIDocument)
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
import Html
import Html.Attributes
import List.Extra as List
import Maybe.MyExtra as Maybe
import Task
import UI.L10N as L10N exposing (L10N, Language(..), text, title)
import UI.Model as Model exposing (CellMsg(..), Context, DocumentMsg(..), Flags, Model, Msg(..))
import UI.Ports
import UI.RowView
import UI.Theme as Theme exposing (hr, onKey)
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
    [ "{{plotsin, plot(x<y), plot(x²+y²=3)},{[zx+iy]plotexp(1/z), plot(x²+y²+z²=3), plot{sinx,x,-sinhx,-x,x²+y²=3,cosx,sinhx,-cosx,-sinx,x²+y²=4}}, {plot(r=3/(t+1)), plot{sin(3t),cos(5t)}, plot((x²-y²+1,2xy)}}"
    , "solve(x+2(x-(x²-6)+1),x"
    , "plot(sin²x+sin²y+sin²z = pw(z>1,-1,.5sinsqrt(x²+y²+z²"
    , "simplifydet{{a,b,c},{0,1,0},{1,2,3"
    , "plot{z=sin(x²+y²),x²+y²+(z-2)² = 3}"
    , "stepsimplify(sin(pi/2-x))"
    , "[zx+iy]{plot(z³-1),plotabs(z³-1),plotarg(z³-1)}"
    , ""
    ]


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init ({ saved, hasClipboard, hasFullscreen, languages } as flags) url key =
    let
        indexToId index =
            let
                id =
                    if index < 0 then
                        -index * 2 - 1

                    else
                        index * 2
            in
            Document.Id id

        toUi index document =
            Document.toUI (indexToId index) document

        documents : Maybe (Zipper UIDocument)
        documents =
            case Codec.decodeValue Model.documentsCodec saved of
                Ok docs ->
                    Maybe.map (Zipper.map toUi) docs

                Err _ ->
                    { rows = List.map emptyRow defaultDocument
                    , name = Nothing
                    , googleId = Nothing
                    }
                        |> Document.toUI (Document.Id 0)
                        |> Zipper.singleton
                        |> Just

        nextId =
            documents
                |> Maybe.map Zipper.toList
                |> Maybe.withDefault []
                |> List.map
                    (\d ->
                        let
                            (Document.Id i) =
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
                |> Task.perform Resized

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

        google : Model.Google
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
            , openMenu = False
            , showPendingActions = False
            , rootUrl = rootUrl
            , key = key
            , google = google
            , context =
                { hasClipboard = hasClipboard
                , hasFullscreen = hasFullscreen
                , isFullscreen = False
                , language =
                    languages
                        |> List.map parseLanguage
                        |> List.filterMap identity
                        |> List.head
                        |> Maybe.withDefault En
                , expandIntervals = False
                , rayDifferentials = True

                -- The other classes have faster processors so should update quicker
                , deviceClass = Phone
                }
            }
    in
    ( model
    , case googleAccessTokenFromUrl of
        Nothing ->
            measure

        Just token ->
            Cmd.batch
                [ measure
                , UI.Ports.saveGoogleAccessTokenAndCloseWindow token
                ]
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
        , model.documents
            |> Maybe.map (Zipper.selected >> viewDocumentModal)
            |> Maybe.withDefault Element.none
            |> Element.inFront
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
                        }

                        @media (hover: hover) {
                            div.hover-parent div.on-parent-hover {
                                visibility: hidden;
                            }

                            div.hover-parent:hover div.on-parent-hover {
                                visibility: visible;
                            }
                        }
                    """
                ]
        , documentPicker model
        , documentView
        ]


selectedDocumentId : { a | documents : Maybe (Zipper { b | id : id }) } -> Maybe id
selectedDocumentId { documents } =
    documents |> Maybe.map (Zipper.selected >> .id)


toolbar : Model -> Element Msg
toolbar ({ google, openMenu, showPendingActions } as model) =
    let
        simpleToolbarButton =
            toolbarButton []

        toolbarButton buttonAttrs msg icon label =
            Input.button
                ([ alignRight
                 , centerY
                 , padding Theme.spacing
                 , Border.rounded 999
                 , Element.mouseOver [ Background.color <| Theme.darken Theme.colors.background ]
                 , title label
                 ]
                    ++ buttonAttrs
                )
                { onPress = Just msg
                , label = Element.element <| icon Theme.darkIconAttrs
                }

        moreButton =
            simpleToolbarButton (ToggleMenu <| not openMenu)
                Icons.moreOutlined
                { en = "Menu", it = "Menù" }

        pendingActions =
            let
                viewGoogleAction { en, it } { name, request } =
                    case request of
                        Google.WaitingAccessToken ->
                            let
                                waitingAccessTokenLabel =
                                    { en = "Waiting for user to authenticate with Google"
                                    , it = "In attesa che l'utente si autentichi con Google"
                                    }
                            in
                            ( text
                                { en = name ++ ": " ++ waitingAccessTokenLabel.en
                                , it = name ++ ": " ++ waitingAccessTokenLabel.it
                                }
                            , Nothing
                            )

                        Running ->
                            ( text
                                { en = name ++ ": " ++ en
                                , it = name ++ ": " ++ it
                                }
                            , Nothing
                            )

                        Errored error ->
                            ( Element.paragraph []
                                [ text
                                    { en =
                                        name
                                            ++ ": "
                                            ++ (case error of
                                                    Google.UnexpectedResponse r ->
                                                        "unexpected response " ++ r

                                                    _ ->
                                                        -- Debug.toString error
                                                        "Unexpected error"
                                               )
                                    , it =
                                        name
                                            ++ ": "
                                            ++ (case error of
                                                    Google.UnexpectedResponse r ->
                                                        "risposta inattesa " ++ r

                                                    _ ->
                                                        -- Debug.toString error
                                                        "Unexpected error"
                                               )
                                    }
                                ]
                            , Nothing
                            )

                        Succeded _ ->
                            ( text
                                { en = name ++ ": done"
                                , it = name ++ ": fatto"
                                }
                            , Nothing
                            )
            in
            List.map (viewGoogleAction { en = "Waiting for an id", it = "In attesa di id" }) google.waitingId
                ++ List.map (viewGoogleAction { en = "Waiting for save", it = "In attesa di salvataggio" }) google.waitingSave

        pendingButton =
            if List.isEmpty pendingActions then
                Element.none

            else if List.any (\( _, msg ) -> msg /= Nothing) pendingActions then
                toolbarButton [ Background.color Theme.colors.warning, Border.width 1 ]
                    (ToggleShowPendingActions <| not showPendingActions)
                    Icons.cloudSyncOutlined
                    { en = "Pending actions", it = "Azioni in coda" }

            else
                simpleToolbarButton (ToggleShowPendingActions <| not showPendingActions)
                    Icons.cloudSyncOutlined
                    { en = "Pending actions", it = "Azioni in coda" }

        runButton =
            selectedDocumentId model
                |> Maybe.map
                    (\id ->
                        simpleToolbarButton (DocumentMsg id DocumentCalculateAll)
                            Icons.playSquareOutlined
                            { en = "Run document", it = "Esegui documento" }
                    )

        clearButton =
            selectedDocumentId model
                |> Maybe.map
                    (\id ->
                        simpleToolbarButton (DocumentMsg id DocumentClearAll)
                            Icons.stopOutlined
                            { en = "Clear document", it = "Pulisci documento" }
                    )
    in
    Element.row
        [ padding <| Theme.spacing // 4
        , if openMenu then
            Element.below <| dropdown model

          else
            width shrink
        , if showPendingActions then
            Element.below <| viewPendingActions pendingActions

          else
            width shrink
        ]
    <|
        List.filterMap identity
            [ Just pendingButton
            , runButton
            , clearButton
            , Just moreButton
            ]


viewPendingActions : List ( Element msg, Maybe msg ) -> Element msg
viewPendingActions actions =
    let
        viewPendingAction ( label, msg ) =
            case msg of
                Nothing ->
                    label

                Just m ->
                    Theme.row [ width fill ]
                        [ label
                        , Input.button
                            [ alignRight
                            , title { en = "Run", it = "Esegui" }
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


dropdown : Model -> Element Msg
dropdown model =
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

        languageButton lang flag =
            el [ width fill ] <|
                el [ centerX ] <|
                    btn (Language lang) [] <|
                        L10N.invariant flag

        expandIntervalsButton expandIntervals =
            if expandIntervals then
                btn (ToggleExpandIntervals False)
                    [ Element.element <| Icons.funnelPlotOutlined Theme.darkIconAttrs
                    , bigStop
                    ]
                    { en = "Do not apply noise reduction"
                    , it = "Non applicare riduzione rumore"
                    }

            else
                simpleBtn (ToggleExpandIntervals True)
                    Icons.funnelPlotOutlined
                    { en = "Apply noise reduction"
                    , it = "Applica riduzione rumore"
                    }

        rayDifferentialsButton rayDifferentials =
            if rayDifferentials then
                btn (ToggleRayDifferentials False)
                    [ Element.element <| Icons.funnelPlotOutlined Theme.darkIconAttrs
                    , bigStop
                    ]
                    { en = "Do not apply ray differentials"
                    , it = "Non applicare differenziali raggi"
                    }

            else
                simpleBtn (ToggleRayDifferentials True)
                    Icons.funnelPlotOutlined
                    { en = "Apply ray differentials"
                    , it = "Applica differenziali raggi"
                    }

        buttons =
            let
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

                languagesRow =
                    Element.row [ width fill ]
                        [ languageButton En "🇬🇧"
                        , vr
                        , languageButton It "🇮🇹"
                        ]

                sid =
                    selectedDocumentId model
            in
            [ Just <| simpleBtn DocumentOpen Icons.folderOpenOutlined { en = "Open", it = "Apri" }
            , sid
                |> Maybe.map
                    (\id ->
                        simpleBtn (DocumentMsg id DocumentDownload)
                            Icons.saveOutlined
                            { en = "Save", it = "Salva" }
                    )
            , Just <| Element.with .expandIntervals expandIntervalsButton
            , Just <| Element.with .rayDifferentials rayDifferentialsButton

            {- , sid
               |> Maybe.map
                   (\id ->
                       simpleBtn (DocumentMsg id DocumentGoogleSave)
                           Icons.uploadOutlined
                           { en = "Save on Google Drive", it = "Salva su Google Drive" }
                   )
            -}
            , Just <| languagesRow
            ]
    in
    Element.column
        [ alignRight
        , Background.color Theme.colors.background
        , Border.width 1
        ]
        (List.intersperse hr <| List.filterMap identity buttons)


documentPicker : Model -> Element Msg
documentPicker ({ documents } as model) =
    let
        toDocumentTabButton : Int -> UIDocument -> Element Msg
        toDocumentTabButton index document =
            let
                selected =
                    index == 0

                name =
                    document.name
                        |> Maybe.withDefaultMaybe
                            (document.rows
                                |> List.head
                                |> Maybe.map rowToTitle
                                |> Maybe.andThen
                                    (\s ->
                                        if String.isEmpty s then
                                            Nothing

                                        else
                                            Just s
                                    )
                            )
                        |> Maybe.map L10N.invariant
                        |> Maybe.withDefault { en = "Untitled", it = "Senza Titolo" }

                label : Element msg
                label =
                    text <| L10N.map ellipsize name
            in
            Element.map (DocumentMsg document.id) <|
                documentTabButton
                    { selected = selected
                    , onPress =
                        if selected then
                            DocumentPushModal <| ModalRename <| Maybe.withDefault "" document.name

                        else
                            DocumentSelect
                    , closeMsg = Just <| DocumentClose { ask = True }
                    , label = label
                    , index = index
                    , title = name
                    }

        buttons =
            Maybe.withDefault [] (Maybe.map (Zipper.toList << Zipper.map toDocumentTabButton) documents)
                ++ [ documentTabButton
                        { selected = False
                        , onPress = DocumentNew
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


rowToTitle : Row -> String
rowToTitle { input } =
    ellipsize input


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

        focusStyleChild =
            Element.focused []
    in
    Element.row
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
        , L10N.title title
        , alignBottom
        , focusStyle
        ]
        [ Input.button
            [ width fill
            , height fill
            , focusStyleChild
            ]
            { onPress = Just onPress
            , label =
                el
                    [ case closeMsg of
                        Nothing ->
                            Element.padding Theme.spacing

                        Just _ ->
                            Element.paddingEach
                                { left = Theme.spacing
                                , top = Theme.spacing
                                , bottom = Theme.spacing
                                , right = Theme.spacing // 2
                                }
                    ]
                    label
            }
        , case closeMsg of
            Nothing ->
                Element.none

            Just _ ->
                Input.button
                    [ Element.paddingEach
                        { left = Theme.spacing // 2
                        , top = Theme.spacing
                        , bottom = Theme.spacing
                        , right = Theme.spacing
                        }
                    , focusStyleChild
                    , L10N.title
                        { en = "Close"
                        , it = "Chiudi"
                        }
                    ]
                    { onPress = closeMsg
                    , label = Element.element <| Icons.closeOutlined Theme.smallDarkIconAttrs
                    }
        ]


viewDocumentModal : UIDocument -> Element Msg
viewDocumentModal document =
    let
        wrap onOk e =
            Element.map (DocumentMsg document.id) <|
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
                                                { onPress = Just DocumentPopModal
                                                , label = text { en = "Cancel", it = "Annulla" }
                                                }
                                            ]
                                       ]
                                )
    in
    case document.modals of
        [] ->
            Element.none

        ModalClose :: _ ->
            wrap (DocumentClose { ask = False })
                [ text <|
                    case document.name of
                        Just name ->
                            { en = "Close document \"" ++ name ++ "\"?"
                            , it = "Chiudi il documento \"" ++ name ++ "\"?"
                            }

                        Nothing ->
                            { en = "Close the document?"
                            , it = "Chiudere il documento?"
                            }
                ]

        (ModalRename name) :: _ ->
            wrap (DocumentRename name)
                [ Input.text
                    [ Input.focusedOnLoad
                    , onKey <|
                        \key ->
                            case key of
                                "Enter" ->
                                    Just <| DocumentRename name

                                "Escape" ->
                                    Just DocumentPopModal

                                _ ->
                                    Nothing
                    ]
                    { onChange = DocumentReplaceModal << ModalRename
                    , text = name
                    , placeholder = Nothing
                    , label =
                        Input.labelAbove [] <|
                            text <|
                                case document.name of
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


viewDocument : { width : Int, height : Int } -> UIDocument -> Element Msg
viewDocument size { id, rows } =
    let
        rowsViews =
            List.indexedMap (\index row -> Lazy.lazy3 UI.RowView.view size index row) rows
    in
    Element.map (DocumentMsg id) <|
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
                    { r | editing = False, data = CodeRow <| List.filterMap parseOrError <| String.split "\n" <| String.replace "\\\n" "" r.input }

                MarkdownRow ->
                    { r | editing = False }

        inner model =
            case msg of
                DocumentMsg docId (DocumentCellMsg data) ->
                    let
                        updateRow rowF =
                            updateByDocId docId
                                (\curr ->
                                    { curr
                                        | rows =
                                            curr.rows
                                                |> List.updateAt data.index rowF
                                                |> List.filterNot (.input >> String.isEmpty)
                                                |> (\rs -> rs ++ [ Document.emptyRow ])
                                    }
                                )
                                model
                    in
                    case data.msg of
                        CopyCanvas canvasId ->
                            ( model, UI.Ports.copyCanvas canvasId )

                        SaveCanvas canvasId ->
                            ( model, UI.Ports.saveCanvas canvasId )

                        FullscreenCanvas canvasId ->
                            ( model, UI.Ports.fullscreenCanvas canvasId )

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

                        ExitFullscreenCanvas canvasId ->
                            ( model, UI.Ports.exitFullscreenCanvas canvasId )

                        ResetZoom canvasId ->
                            ( model, UI.Ports.resetZoomCanvas canvasId )

                DocumentNew ->
                    addDocument model
                        { rows = [ Document.emptyRow ]
                        , name = Nothing
                        , googleId = Nothing
                        }

                DocumentMsg docId DocumentSelect ->
                    let
                        documents =
                            Maybe.map
                                (Zipper.focusFind (\{ id } -> id == docId))
                                model.documents
                    in
                    persist { model | documents = documents }

                DocumentMsg docId (DocumentClose { ask }) ->
                    if ask then
                        updateByDocId docId
                            (\document ->
                                { document
                                    | modals = ModalClose :: document.modals
                                }
                            )
                            model

                    else
                        let
                            documents =
                                model.documents
                                    |> Maybe.andThen (Zipper.filter (\{ id } -> id /= docId))
                        in
                        persist { model | documents = documents }

                DocumentMsg docId DocumentCalculateAll ->
                    updateByDocId docId
                        (\doc ->
                            { doc | rows = List.map calculateRow doc.rows }
                        )
                        model

                DocumentMsg docId DocumentClearAll ->
                    updateByDocId docId
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

                DocumentMsg docId (DocumentRename name) ->
                    updateByDocId docId
                        (\doc ->
                            { doc
                                | name =
                                    if String.isEmpty name then
                                        Nothing

                                    else
                                        Just name
                                , modals = List.drop 1 doc.modals
                            }
                        )
                        model

                Resized size ->
                    let
                        context_ =
                            model.context
                    in
                    ( { model | size = size, context = { context_ | deviceClass = (Element.classifyDevice size).class } }, Cmd.none )

                ToggleMenu openMenu ->
                    ( { model | openMenu = openMenu }, Cmd.none )

                ToggleShowPendingActions showPendingActions ->
                    ( { model | showPendingActions = showPendingActions }, Cmd.none )

                DocumentOpen ->
                    ( model, File.Select.file [ "text/plain", ".txt" ] DocumentOpenSelected )

                DocumentMsg docId (DocumentPushModal modal) ->
                    updateByDocId docId (\document -> { document | modals = modal :: document.modals }) model

                DocumentMsg docId DocumentPopModal ->
                    updateByDocId docId (\document -> { document | modals = List.drop 1 document.modals }) model

                DocumentMsg docId (DocumentReplaceModal modal) ->
                    updateByDocId docId (\document -> { document | modals = modal :: List.drop 1 document.modals }) model

                DocumentMsg docId DocumentDownload ->
                    case List.find (\{ id } -> id == docId) <| Maybe.withDefault [] <| Maybe.map Zipper.toList model.documents of
                        Nothing ->
                            ( model, Cmd.none )

                        Just document ->
                            -- TODO: ask the user for a name if empty
                            ( model
                            , File.Download.string
                                (Maybe.withDefault "Untitled" document.name ++ ".txt")
                                "text/plain"
                                (Document.toFile document)
                            )

                DocumentOpenSelected file ->
                    ( model
                    , Task.perform
                        (\content ->
                            DocumentOpenContent
                                { name = File.name file
                                , content = content
                                }
                        )
                        (File.toString file)
                    )

                DocumentOpenContent { name, content } ->
                    let
                        { errors, document } =
                            Document.fromFile
                                (Just <|
                                    if String.endsWith ".txt" name then
                                        String.dropRight 4 name

                                    else
                                        name
                                )
                                content
                    in
                    addDocument model document

                Language language ->
                    let
                        context =
                            model.context
                    in
                    ( { model | context = { context | language = language } }, Cmd.none )

                ToggleExpandIntervals expandIntervals ->
                    let
                        context =
                            model.context
                    in
                    ( { model | context = { context | expandIntervals = expandIntervals } }, Cmd.none )

                ToggleRayDifferentials rayDifferentials ->
                    let
                        context =
                            model.context
                    in
                    ( { model | context = { context | rayDifferentials = rayDifferentials } }, Cmd.none )

                GoogleAuth ->
                    ( model, UI.Ports.openWindow <| Google.authenticationFlowUrl model.rootUrl )

                DocumentMsg docId DocumentGoogleSave ->
                    case Maybe.andThen (Zipper.find (\{ id } -> id == docId)) model.documents of
                        Just document ->
                            googleSave model document

                        Nothing ->
                            ( model, Cmd.none )

                DocumentMsg docId (DocumentGoogleId res) ->
                    documentGoogleId docId res model

                DocumentMsg docId (DocumentGoogleSaveResult res) ->
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

                IsFullscreen isFullscreen ->
                    let
                        context =
                            model.context
                    in
                    ( { model
                        | context =
                            { context
                                | isFullscreen = isFullscreen
                            }
                      }
                    , Cmd.none
                    )

                GoogleGotAccessToken token ->
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
                                            (DocumentMsg id << DocumentGoogleId)
                                            (Google.generateId { accessToken = token })
                                    )

                        saveRequests =
                            google.waitingSave
                                |> List.filter (\{ request } -> request == Google.WaitingAccessToken)
                                |> List.map
                                    (\{ id, googleId, name, content } ->
                                        Task.attempt (DocumentMsg id << DocumentGoogleSaveResult)
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


documentGoogleId : Document.Id -> Result Google.Error Google.FileId -> Model -> ( Model, Cmd Msg )
documentGoogleId docId res model =
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
            in
            case maybeDoc of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just data ->
                    let
                        documents_ =
                            Maybe.map
                                (Zipper.map
                                    (\_ ({ id } as document) ->
                                        if id == docId then
                                            { document
                                                | googleId = Just googleId
                                            }

                                        else
                                            document
                                    )
                                )
                                model.documents
                    in
                    case google.accessToken of
                        Missing ->
                            persist
                                { model
                                    | google = google_ data WaitingAccessToken
                                    , documents = documents_
                                }

                        Expired ->
                            persist
                                { model
                                    | google = google_ data WaitingAccessToken
                                    , documents = documents_
                                }

                        Present token ->
                            { model
                                | google = google_ data Running
                                , documents = documents_
                            }
                                |> persist
                                |> withCommand
                                    (Task.attempt (DocumentMsg docId << DocumentGoogleSaveResult) <|
                                        Google.uploadFile
                                            { id = googleId
                                            , name = data.name
                                            , content = data.content
                                            , accessToken = token
                                            }
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


withCommand : Cmd Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withCommand cmd ( model, cmds ) =
    ( model, Cmd.batch [ cmd, cmds ] )


addDocument : Model -> StoredDocument -> ( Model, Cmd Msg )
addDocument model { name, rows, googleId } =
    let
        toAppend =
            { id = Document.Id model.nextId
            , name = name
            , rows = rows
            , googleId = googleId
            , modals = []
            }

        documents =
            Just <|
                case model.documents of
                    Nothing ->
                        Zipper.singleton toAppend

                    Just docs ->
                        docs
                            |> Zipper.append toAppend
                            |> Zipper.allRight
    in
    persist
        { model
            | documents = documents
            , nextId = model.nextId + 1
        }


persist : Model -> ( Model, Cmd msg )
persist model =
    ( model
    , model.documents
        |> Maybe.map (Zipper.map (\_ -> Document.toStored))
        |> Codec.encodeToValue Model.documentsCodec
        |> UI.Ports.persist
    )


googleSave : Model -> UIDocument -> ( Model, Cmd Msg )
googleSave model document =
    let
        content =
            Document.toFile document

        name =
            -- TODO warn the user and ask for a name
            Maybe.withDefault "Untitled" document.name

        data request =
            { id = document.id
            , name = name
            , content = content
            , request = request
            }

        google =
            model.google

        google_ request =
            case document.googleId of
                Nothing ->
                    { google
                        | waitingId =
                            data request :: google.waitingId
                    }

                Just googleId ->
                    { google
                        | waitingSave =
                            { googleId = googleId
                            , id = document.id
                            , name = name
                            , content = content
                            , request = Google.mapRequest (\_ -> ()) request
                            }
                                :: google.waitingSave
                    }
    in
    case google.accessToken of
        Missing ->
            ( { model | google = google_ WaitingAccessToken }
            , Cmd.none
            )

        Expired ->
            ( { model | google = google_ WaitingAccessToken }
            , Cmd.none
            )

        Present token ->
            ( { model | google = google_ Running }
            , case document.googleId of
                Nothing ->
                    Task.attempt (DocumentMsg document.id << DocumentGoogleId) <| Google.generateId { accessToken = token }

                Just googleId ->
                    Task.attempt (DocumentMsg document.id << DocumentGoogleSaveResult) <|
                        Google.uploadFile
                            { id = googleId
                            , name = name
                            , content = content
                            , accessToken = token
                            }
            )


updateByDocId : Document.Id -> (UIDocument -> UIDocument) -> Model -> ( Model, Cmd msg )
updateByDocId docId f model =
    case model.documents of
        Nothing ->
            ( model, Cmd.none )

        Just docs ->
            let
                documents =
                    Zipper.map
                        (\_ ({ id } as doc) ->
                            if id == docId then
                                f doc

                            else
                                doc
                        )
                        docs
            in
            persist { model | documents = Just documents }


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
        , UI.Ports.gotGoogleAccessToken GoogleGotAccessToken
        , UI.Ports.isFullscreen IsFullscreen
        ]
