module UI.RowView exposing (view)

import Ant.Icons as Icons
import Complex
import Dict
import Element.WithContext as Element exposing (DeviceClass(..), Element, alignBottom, alignRight, alignTop, centerX, el, fill, height, htmlAttribute, inFront, none, padding, paddingXY, px, row, shrink, spacing, text, width)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Element.WithContext.Lazy as Lazy
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), RelationOperation(..), SolutionTree(..), UnaryOperation(..), genericAsMatrix)
import Expression.Graph exposing (Graph(..))
import Expression.NumericRange
import Expression.Value exposing (Value(..))
import Html
import Html.Attributes
import Json.Encode
import List.Extra as List
import Markdown.Parser
import Markdown.Renderer
import Model exposing (CellMsg(..), Context, Msg(..), Output(..), Row, RowData(..))
import UI.Glsl exposing (getGlsl)
import UI.Theme as Theme


type alias Element msg =
    Element.Element Context msg


draw : { width : Int, height : Int } -> String -> { wdiv : Int, hdiv : Int } -> Graph -> Element CellMsg
draw { width, height } id { wdiv, hdiv } graph =
    let
        isCompletelyReal g =
            case g of
                GraphList gs ->
                    List.all isCompletelyReal gs

                Contour e ->
                    if Expression.NumericRange.isCompletelyReal e then
                        True

                    else
                        False

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
            min 1920 <| width - (8 + wdiv) * Theme.spacing

        rawImageHeight =
            min 1080 <| height - (14 + hdiv) * Theme.spacing

        imageWidth =
            min rawImageWidth <| rawImageHeight * 4 // 3

        imageHeight =
            min rawImageHeight <| rawImageWidth * 4 // 3

        saveButton =
            Input.button []
                { label = Element.element <| Icons.saveOutlined Theme.lightIconAttrs
                , onPress = Just <| Save id
                }

        copyButton =
            Element.with .hasClipboard <|
                \hasClipboard ->
                    if hasClipboard then
                        Input.button []
                            { label = Element.element <| Icons.copyOutlined Theme.lightIconAttrs
                            , onPress = Just <| Copy id
                            }

                    else
                        none

        buttonsRow =
            Theme.row [ alignRight, alignTop, padding Theme.spacing ] [ copyButton, saveButton ]

        attrs =
            [ inFront buttonsRow
            ]
    in
    Element.with .expandIntervals <|
        \expandIntervals ->
            el attrs <|
                Element.html <|
                    Html.node "nu-plot"
                        [ Html.Attributes.id id
                        , Html.Attributes.property "exprSrc" <| Json.Encode.string <| getGlsl expandIntervals graph
                        , Html.Attributes.attribute "canvas-width" <| String.fromInt <| imageWidth // wdiv
                        , Html.Attributes.attribute "canvas-height" <| String.fromInt <| imageHeight // hdiv
                        , Html.Attributes.attribute "white-lines" <| String.fromInt Theme.whiteLines
                        , Html.Attributes.attribute "completely-real" <| boolToIntString <| isCompletelyReal graph
                        , Html.Attributes.attribute "de-noise" "0.000001"
                        , Html.Attributes.attribute "is-3d" <| boolToIntString <| is3D graph
                        , Html.Attributes.title <| Expression.toString <| Expression.Graph.toExpression graph
                        ]
                        []


view : { width : Int, height : Int } -> Int -> Row -> Element Msg
view size index { input, editing, data } =
    Element.map (CellMsg index) <|
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
                    [ Element.width fill
                    , alignTop
                    ]
                    (codeInputLine size index input :: outputRows)

            MarkdownRow ->
                if editing then
                    Theme.row [ Element.width fill ]
                        [ inputBox (size.width - 100) input
                        , Input.button [ htmlAttribute <| Html.Attributes.title "End editing" ]
                            { onPress = Just EndEditing
                            , label = Element.element <| Icons.enterOutlined Theme.darkIconAttrs
                            }
                        , Input.button [ alignRight, htmlAttribute <| Html.Attributes.title "Convert to Code cell" ]
                            { label = Element.element <| Icons.swapOutlined Theme.darkIconAttrs
                            , onPress = Just ToCode
                            }
                        ]

                else
                    input
                        |> Markdown.Parser.parse
                        |> Result.withDefault []
                        |> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer
                        |> Result.withDefault []
                        |> List.map (\e -> Element.paragraph [] [ Element.html e ])
                        |> Element.textColumn [ Element.width fill, alignTop ]
                        |> (\e ->
                                Theme.row [ Element.width fill ]
                                    [ e
                                    , Input.button [ htmlAttribute <| Html.Attributes.title "Edit" ]
                                        { onPress = Just StartEditing
                                        , label = Element.element <| Icons.editOutlined Theme.darkIconAttrs
                                        }
                                    , Input.button [ alignRight, htmlAttribute <| Html.Attributes.title "Convert to Code cell" ]
                                        { label = Element.element <| Icons.swapOutlined Theme.darkIconAttrs
                                        , onPress = Just ToCode
                                        }
                                    ]
                           )


inputBox : Int -> String -> Element CellMsg
inputBox maxWidth input =
    Input.multiline
        [ Element.width <| Element.maximum maxWidth fill
        , Element.htmlAttribute <| Html.Attributes.attribute "autocorrect" "off"
        , Element.htmlAttribute <| Html.Attributes.attribute "autocapitalize" "none"
        , Element.htmlAttribute <| Html.Attributes.spellcheck False
        , Element.htmlAttribute <| Html.Attributes.class "input"
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
            [ Input.button [ alignRight, htmlAttribute <| Html.Attributes.title "Press Ctrl+Enter to calculate" ]
                { onPress = Just Calculate
                , label = Element.element <| Icons.playSquareOutlined Theme.darkIconAttrs
                }
            , Input.button [ alignRight, htmlAttribute <| Html.Attributes.title "Convert to Markdown cell" ]
                { label = Element.element <| Icons.swapOutlined Theme.darkIconAttrs
                , onPress = Just ToMarkdown
                }
            , Input.button [ alignRight, htmlAttribute <| Html.Attributes.title "Clear result" ]
                { label = Element.element <| Icons.stopOutlined Theme.darkIconAttrs
                , onPress = Just Clear
                }
            ]

        label =
            text <| "In [" ++ String.fromInt index ++ "]"
    in
    Element.with .deviceClass <|
        \deviceClass ->
            case deviceClass of
                Phone ->
                    Theme.column [ width fill ]
                        [ Theme.row [ width fill ]
                            (label :: buttons)
                        , inputBox (size.width - 2 * Theme.spacing) input
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
                    [ text "Interpreted as: "
                    , viewExpression pageWidth e
                    ]

            else
                none


viewError : String -> Element msg
viewError e =
    let
        viewErrorLine l =
            Element.paragraph
                [ htmlAttribute <| Html.Attributes.class "pre" ]
                [ text l ]
    in
    e
        |> String.split "\n"
        |> List.map viewErrorLine
        |> Element.textColumn
            [ alignTop
            , Font.family [ Font.monospace ]
            , Font.color Theme.colors.errorMessage
            ]


viewExpression : Int -> Expression -> Element msg
viewExpression pageWidth expr =
    viewLaTeX pageWidth <| Expression.toTeXString expr


viewSolutionTree : Int -> SolutionTree -> List (Element msg)
viewSolutionTree pageWidth tree =
    case tree of
        SolutionStep e c ->
            -- Theme.row [] [
            viewLaTeX pageWidth (Expression.toTeXString e)
                --, Expression.toDebugTree Debug.todo e ]
                :: viewSolutionTree pageWidth c

        SolutionForall v ->
            [ viewLaTeX pageWidth <| "\\forall " ++ v ++ " \\in \\mathbb{R}" ]

        SolutionError e ->
            [ text <| "Error: " ++ e ]

        SolutionNone v ->
            [ viewLaTeX pageWidth <| "\\not \\exists " ++ v ++ " \\in \\mathbb{R}" ]

        SolutionDone e ->
            [ viewLaTeX pageWidth <| Expression.toTeXString e ]

        SolutionBranch children ->
            let
                br =
                    el [ width <| px 1, height fill, Border.width 1 ] none
            in
            [ Element.row [ spacing Theme.spacing ] <| List.intersperse br <| List.map (\c -> Element.column [ spacing Theme.spacing, alignTop ] <| viewSolutionTree pageWidth c) children ]


viewLaTeX : Int -> String -> Element msg
viewLaTeX pageWidth tex =
    el [ height shrink, width shrink ] <|
        Element.html <|
            Html.node "math-jax"
                [ Html.Attributes.attribute "tex-src" tex
                , Html.Attributes.attribute "container-width" <| String.fromInt <| pageWidth - 4 * Theme.spacing
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
                , Element.height fill
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
outputBlock blockId ({ width } as size) output =
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
                     --, Expression.toDebugTree Debug.todo e ]
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
                    ( viewExpression width ex, False )

                Nothing ->
                    case v of
                        SymbolicValue s ->
                            ( viewExpression width s, False )

                        SolutionTreeValue t ->
                            ( Element.column [ spacing Theme.spacing ] <| viewSolutionTree (width - 2 * Theme.spacing) t, False )

                        GraphValue g ->
                            ( draw size id coeffs g, True )

                        ComplexValue c ->
                            ( text <| Complex.toString c, False )

                        ErrorValue err ->
                            ( viewError err, False )

                        LambdaValue x f ->
                            case asExpression f of
                                Just e ->
                                    ( viewExpression width <| Lambda x e, False )

                                Nothing ->
                                    ( Element.row [] [ text <| x ++ " => ", Tuple.first (viewValue id coeffs f) ], False )

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
                    Element.width fill
                ]
                child
