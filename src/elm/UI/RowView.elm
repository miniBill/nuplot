module UI.RowView exposing (view)

import Ant.Icons as Icons
import Complex
import Dict
import Document exposing (Output(..), Row, RowData(..))
import Element.WithContext as Element exposing (DeviceClass(..), alignBottom, alignRight, alignTop, centerX, el, fill, height, htmlAttribute, inFront, none, paddingXY, px, row, shrink, spacing, width)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Element.WithContext.Lazy as Lazy
import Expression exposing (Expression(..), SolutionTree(..), genericAsMatrix)
import Expression.Graph exposing (Graph(..))
import Expression.NumericRange
import Expression.Value exposing (Value(..))
import Html
import Html.Attributes
import Json.Encode
import Markdown.Parser
import Markdown.Renderer
import UI.Glsl exposing (getGlsl)
import UI.L10N exposing (L10N, invariant, text, title)
import UI.Model exposing (CanvasId(..), CellMsg(..), DocumentMsg(..))
import UI.Theme as Theme exposing (Element)


draw : { width : Int, height : Int } -> CanvasId -> { wdiv : Int, hdiv : Int } -> { graph : Graph, axes : Bool } -> Element CellMsg
draw size id { wdiv, hdiv } { graph, axes } =
    let
        isCompletelyReal g =
            case g of
                GraphList gs ->
                    List.all isCompletelyReal gs

                Contour e ->
                    Expression.NumericRange.isCompletelyReal e

                _ ->
                    False

        is3D g =
            case g of
                GraphList gs ->
                    List.any is3D gs

                Implicit3D _ ->
                    True

                _ ->
                    False

        boolToIntString b =
            if b then
                "1"

            else
                "0"

        rawImageWidth =
            min 1920 <| size.width - (8 + wdiv) * Theme.rythm

        rawImageHeight =
            min 1080 <| size.height - (14 + hdiv) * Theme.rythm

        imageWidth =
            min rawImageWidth <| rawImageHeight * 4 // 3

        imageHeight =
            min rawImageHeight <| rawImageWidth * 4 // 3

        iconButton msg rounds icon =
            Input.button
                [ Element.behindContent <|
                    el
                        [ width fill
                        , height fill
                        , Background.color <| Element.rgba 0 0 0 0.2
                        , Border.roundEach rounds
                        ]
                        none
                , Theme.padding
                , htmlAttribute <| Html.Attributes.class "on-parent-hover"
                ]
                { label = Element.element <| icon Theme.lightIconAttrs
                , onPress = Just <| msg id
                }

        noRound =
            { topLeft = 0, topRight = 0, bottomLeft = 0, bottomRight = 0 }

        brButtonsRow =
            Element.with identity <|
                \{ isFullscreen, hasFullscreen } ->
                    row [ alignRight, alignBottom ] <|
                        if isFullscreen then
                            [ iconButton ResetZoom { noRound | topLeft = Theme.rythm } Icons.aimOutlined
                            , iconButton ExitFullscreenCanvas noRound Icons.fullscreenExitOutlined
                            ]

                        else if hasFullscreen then
                            [ iconButton ResetZoom { noRound | topLeft = Theme.rythm } Icons.aimOutlined
                            , iconButton FullscreenCanvas noRound Icons.fullscreenOutlined
                            ]

                        else
                            [ iconButton ResetZoom { noRound | topLeft = Theme.rythm } Icons.aimOutlined
                            ]

        urButtonsRow =
            Element.with .hasClipboard <|
                \hasClipboard ->
                    row [ alignRight, alignTop ] <|
                        if hasClipboard then
                            [ iconButton CopyCanvas { noRound | bottomLeft = Theme.rythm } Icons.copyOutlined
                            , iconButton SaveCanvas noRound Icons.saveOutlined
                            ]

                        else
                            [ iconButton SaveCanvas { noRound | bottomLeft = Theme.rythm } Icons.saveOutlined
                            ]

        (CanvasId cid) =
            id

        attrs =
            [ inFront urButtonsRow
            , inFront brButtonsRow
            , htmlAttribute <| Html.Attributes.id <| cid ++ "-parent"
            , htmlAttribute <| Html.Attributes.class "hover-parent"
            ]
    in
    Element.with identity <|
        \{ rayDifferentials } ->
            el attrs <|
                Element.html <|
                    Html.node "nu-plot"
                        [ Html.Attributes.id cid
                        , Html.Attributes.property "exprSrc" <| Json.Encode.string <| getGlsl rayDifferentials graph
                        , Html.Attributes.attribute "canvas-width" <| String.fromInt <| imageWidth // wdiv
                        , Html.Attributes.attribute "canvas-height" <| String.fromInt <| imageHeight // hdiv
                        , Html.Attributes.attribute "white-lines" <| String.fromInt Theme.whiteLines
                        , Html.Attributes.attribute "completely-real" <| boolToIntString <| isCompletelyReal graph
                        , Html.Attributes.attribute "draw-axes" <| boolToIntString axes
                        , Html.Attributes.attribute "is-3d" <| boolToIntString <| is3D graph
                        , Html.Attributes.title <| Expression.toString <| Expression.Graph.toExpression graph
                        ]
                        []


view : { width : Int, height : Int } -> Int -> Row -> Element DocumentMsg
view size index { input, editing, data } =
    Element.map (\msg -> DocumentCellMsg { index = index, msg = msg }) <|
        case data of
            CodeRow output ->
                let
                    outputRows =
                        List.concatMap
                            (\o ->
                                [ Lazy.lazy3 outputBlock ("canvas" ++ String.fromInt index) size o
                                , statusLine size.width o
                                ]
                            )
                            output
                in
                Theme.column
                    [ width fill
                    , alignTop
                    ]
                    (codeInputLine size index input :: outputRows)

            MarkdownRow ->
                let
                    toCodeCellButton =
                        Input.button
                            [ alignRight
                            , title
                                { en = "Convert to Code cell"
                                , it = "Converti in cella con Codice"
                                }
                            ]
                            { label = Element.element <| Icons.swapOutlined Theme.darkIconAttrs
                            , onPress = Just ToCode
                            }
                in
                if editing then
                    Theme.row [ width fill ]
                        [ inputBox (size.width - 100) input
                        , Input.button
                            [ title
                                { en = "End editing"
                                , it = "Fine modifica"
                                }
                            ]
                            { onPress = Just EndEditing
                            , label = Element.element <| Icons.enterOutlined Theme.darkIconAttrs
                            }
                        , toCodeCellButton
                        ]

                else
                    input
                        |> Markdown.Parser.parse
                        |> Result.withDefault []
                        |> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer
                        |> Result.withDefault []
                        |> List.map (\e -> Element.paragraph [] [ Element.html e ])
                        |> Element.textColumn [ width fill, alignTop ]
                        |> (\e ->
                                Theme.row [ width fill ]
                                    [ e
                                    , editButton
                                    , toCodeCellButton
                                    ]
                           )


editButton : Element CellMsg
editButton =
    Input.button
        [ title
            { en = "Edit"
            , it = "Modifica"
            }
        ]
        { onPress = Just StartEditing
        , label = Element.element <| Icons.editOutlined Theme.darkIconAttrs
        }


inputBox : Int -> String -> Element CellMsg
inputBox maxWidth input =
    Input.multiline
        [ width <| Element.maximum maxWidth fill
        , htmlAttribute <| Html.Attributes.attribute "autocorrect" "off"
        , htmlAttribute <| Html.Attributes.attribute "autocapitalize" "none"
        , htmlAttribute <| Html.Attributes.spellcheck False
        , htmlAttribute <| Html.Attributes.class "input"
        , Theme.onCtrlEnter Calculate
        ]
        { label = Input.labelHidden "Input"
        , onChange = Input
        , placeholder = Nothing
        , text = input
        , spellcheck = False
        }


codeInputLine : { a | width : Int } -> Int -> String -> Element CellMsg
codeInputLine size index input =
    let
        buttons =
            [ Input.button
                [ alignRight
                , title
                    { en = "Press Ctrl+Enter to calculate"
                    , it = "Premere Ctrl+Invio per calcolare"
                    }
                ]
                { onPress = Just Calculate
                , label = Element.element <| Icons.playSquareOutlined Theme.darkIconAttrs
                }
            , Input.button
                [ alignRight
                , title
                    { en = "Convert to Markdown cell"
                    , it = "Converti in cella con Markdown"
                    }
                ]
                { label = Element.element <| Icons.swapOutlined Theme.darkIconAttrs
                , onPress = Just ToMarkdown
                }
            , Input.button
                [ alignRight
                , title
                    { en = "Clear result"
                    , it = "Pulisci risultato"
                    }
                ]
                { label = Element.element <| Icons.stopOutlined Theme.darkIconAttrs
                , onPress = Just Clear
                }
            ]

        label =
            text <| invariant <| "In [" ++ String.fromInt index ++ "]"
    in
    Element.with .deviceClass <|
        \deviceClass ->
            case deviceClass of
                Phone ->
                    Theme.column [ width fill ]
                        [ Theme.row [ width fill ]
                            (label :: buttons)
                        , inputBox (size.width - 2 * Theme.rythm) input
                        ]

                _ ->
                    Theme.row [ width fill ]
                        (label :: inputBox (size.width - 180) input :: buttons)


statusLine : Int -> Output -> Element msg
statusLine pageWidth output =
    case output of
        ParseError e ->
            viewError e

        Parsed e ->
            if False then
                Theme.column [ alignTop, width <| Element.maximum 200 fill ]
                    [ text { en = "Interpreted as: ", it = "Interpretato come: " }
                    , viewExpression pageWidth e
                    ]

            else
                none


viewError : L10N String -> Element msg
viewError e =
    let
        tuples =
            List.map2 (\en it -> { en = en, it = it })
                (String.split "\n" e.en)
                (String.split "\n" e.it)

        viewErrorLine l =
            Element.paragraph
                [ htmlAttribute <| Html.Attributes.class "pre" ]
                [ text l ]
    in
    tuples
        |> List.map viewErrorLine
        |> Element.textColumn
            [ alignTop
            , Font.family [ Font.monospace ]
            , Font.color Theme.colors.errorMessage
            , width fill
            ]


viewExpression : Int -> Expression -> Element msg
viewExpression pageWidth expr =
    viewLaTeX pageWidth <| Expression.toTeXString expr


viewSolutionTree : Int -> SolutionTree -> List (Element msg)
viewSolutionTree pageWidth tree =
    case tree of
        SolutionStep e c ->
            --Theme.row [] [
            viewLaTeX pageWidth (Expression.toTeXString e)
                --, Expression.toDebugTree Debug.todo e
                --]
                :: viewSolutionTree pageWidth c

        SolutionForall v ->
            [ viewLaTeX pageWidth <| "\\forall " ++ v ++ " \\in \\mathbb{R}" ]

        SolutionError e ->
            [ text
                { en = "Error: " ++ e.en
                , it = "Errore: " ++ e.it
                }
            ]

        SolutionNone v ->
            [ viewLaTeX pageWidth <| "\\not \\exists " ++ v ++ " \\in \\mathbb{R}" ]

        SolutionDone e ->
            [ --Theme.row [] [
              viewLaTeX pageWidth <| Expression.toTeXString e

            -- , Expression.toDebugTree Debug.todo e
            -- ]
            ]

        SolutionBranch children ->
            let
                br =
                    el [ width <| px 1, height fill, Border.width 1 ] none
            in
            [ row [ Theme.spacing ] <|
                List.intersperse br <|
                    List.map
                        (\c ->
                            Element.column
                                [ Theme.spacing, alignTop ]
                                (viewSolutionTree pageWidth c)
                        )
                        children
            ]


viewLaTeX : Int -> String -> Element msg
viewLaTeX pageWidth tex =
    el [ height shrink, width shrink ] <|
        Element.html <|
            Html.node "ka-tex"
                [ Html.Attributes.attribute "tex-src" tex
                , Html.Attributes.attribute "container-width" <| String.fromInt <| pageWidth - 2 * Theme.rythm
                ]
                []


roundBracketed : List (List (Element msg)) -> Element msg
roundBracketed =
    simplyBracketed
        { left = Theme.bracketBorderWidth, top = 0, bottom = 0, right = 0 }
        { topLeft = 999, bottomLeft = 999, topRight = 0, bottomRight = 0 }
        { left = 0, top = 0, bottom = 0, right = Theme.bracketBorderWidth }
        { topRight = 999, bottomRight = 999, topLeft = 0, bottomLeft = 0 }


simplyBracketed :
    { bottom : Int, left : Int, right : Int, top : Int }
    -> { topLeft : Int, topRight : Int, bottomLeft : Int, bottomRight : Int }
    -> { bottom : Int, left : Int, right : Int, top : Int }
    -> { topLeft : Int, topRight : Int, bottomLeft : Int, bottomRight : Int }
    -> List (List (Element msg))
    -> Element msg
simplyBracketed lw lr rw rr blocks =
    let
        bracket we re =
            el
                [ width <| px Theme.bracketWidth
                , height fill
                , Border.widthEach we
                , Border.roundEach re
                ]
                none
    in
    bracketed (bracket lw lr) (bracket rw rr) blocks


bracketed : Element msg -> Element msg -> List (List (Element msg)) -> Element msg
bracketed l r blocks =
    let
        grid =
            blocks
                |> Theme.grid [ alignBottom ]
    in
    row [ spacing 1, paddingXY 2 0 ] [ l, grid, r ]


outputBlock : String -> { height : Int, width : Int } -> Output -> Element CellMsg
outputBlock blockId size output =
    let
        showExpr : Expression -> ( Element CellMsg, Bool )
        showExpr e =
            e
                |> Expression.Value.value Dict.empty
                |> viewValue blockId { wdiv = 1, hdiv = 1 }
                |> Tuple.mapFirst
                    (\vv ->
                        --Theme.row [] [
                        vv
                     --, Expression.toDebugTree Debug.todo e
                     --]
                    )

        asExpression v =
            case v of
                SymbolicValue s ->
                    Just s

                ComplexValue c ->
                    Just <| Expression.Value.complexToSymbolic c

                ListValue ls ->
                    Maybe.map List <| List.foldr (Maybe.map2 (::)) (Just []) <| List.map asExpression ls

                ErrorValue _ ->
                    Nothing

                GraphValue _ ->
                    Nothing

                SolutionTreeValue _ ->
                    Nothing

                LambdaValue x f ->
                    Maybe.map (Lambda x) (asExpression f)

        viewValue : String -> { hdiv : Int, wdiv : Int } -> Value -> ( Element CellMsg, Bool )
        viewValue id coeffs v =
            case asExpression v of
                Just ex ->
                    ( viewExpression size.width ex, False )

                Nothing ->
                    case v of
                        SymbolicValue s ->
                            ( viewExpression size.width s, False )

                        SolutionTreeValue t ->
                            ( Element.column
                                [ Theme.spacing
                                , Element.scrollbarX
                                , width <| Element.maximum (size.width - 2 * Theme.rythm) fill
                                ]
                              <|
                                viewSolutionTree (size.width - 2 * Theme.rythm) t
                            , False
                            )

                        GraphValue g ->
                            ( draw size (CanvasId id) coeffs { axes = True, graph = g }, True )

                        ComplexValue c ->
                            ( text <| invariant <| Complex.toString c, False )

                        ErrorValue err ->
                            ( viewError err, False )

                        LambdaValue x f ->
                            case asExpression f of
                                Just e ->
                                    ( viewExpression size.width <| Lambda x e, False )

                                Nothing ->
                                    ( row []
                                        [ text <| invariant <| x ++ " => "
                                        , Tuple.first (viewValue id coeffs f)
                                        ]
                                    , False
                                    )

                        ListValue ls ->
                            let
                                unpack =
                                    List.unzip >> (\( es, gs ) -> ( es, List.any identity gs ))

                                valueAsList w =
                                    case w of
                                        ListValue us ->
                                            Just us

                                        _ ->
                                            Nothing
                            in
                            case genericAsMatrix valueAsList v of
                                Just m ->
                                    m
                                        |> List.indexedMap
                                            (\y ->
                                                List.indexedMap
                                                    (\x ->
                                                        viewValue (id ++ "_" ++ String.fromInt y ++ "_" ++ String.fromInt x)
                                                            { wdiv = coeffs.wdiv * Maybe.withDefault 1 (Maybe.map List.length <| List.head m)
                                                            , hdiv = coeffs.hdiv * List.length m
                                                            }
                                                    )
                                                    >> unpack
                                            )
                                        |> unpack
                                        |> Tuple.mapFirst roundBracketed

                                Nothing ->
                                    ls
                                        |> List.indexedMap
                                            (\i ->
                                                viewValue (id ++ "_" ++ String.fromInt i)
                                                    { coeffs | wdiv = coeffs.wdiv * List.length ls }
                                            )
                                        |> unpack
                                        |> Tuple.mapFirst (\e -> roundBracketed [ e ])
    in
    case output of
        ParseError _ ->
            none

        Parsed e ->
            let
                ( child, isPlot ) =
                    showExpr e
            in
            el
                [ if isPlot then
                    centerX

                  else
                    width fill
                ]
                child
