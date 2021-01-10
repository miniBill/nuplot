module UI.RowView exposing (view)

import Ant.Icon as Icon
import Ant.Icons as Icons
import Complex
import Dict
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, el, fill, height, htmlAttribute, inFront, none, padding, paddingXY, px, rgb, row, shrink, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), RelationOperation(..), SolutionTree(..), UnaryOperation(..), genericAsMatrix)
import Expression.Graph exposing (Graph(..))
import Expression.NumericRange
import Expression.Value exposing (Value(..))
import Html
import Html.Attributes
import Json.Encode
import List.Extra as List
import Model exposing (Msg(..), Output(..), Row)
import UI.Glsl exposing (getGlsl)
import UI.Theme as Theme exposing (onEnter)


draw : Bool -> { width : Int, height : Int } -> String -> { wdiv : Int, hdiv : Int } -> Graph -> Element Msg
draw hasClipboard { width, height } id { wdiv, hdiv } graph =
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

        imageWidth =
            min 1920 <| width - (1 + wdiv) * 3 * Theme.spacing

        imageHeight =
            min 1080 <| height - (6 + hdiv) * 3 * Theme.spacing

        saveButton =
            Input.button []
                { label = Icons.saveOutlined Theme.lightIconAttrs
                , onPress = Just <| Save id
                }

        copyButton =
            if hasClipboard then
                Input.button []
                    { label = Icons.copyOutlined Theme.lightIconAttrs
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
    el attrs <|
        Element.html <|
            Html.node "nu-plot"
                [ Html.Attributes.id id
                , Html.Attributes.property "exprSrc" <| Json.Encode.string <| getGlsl graph
                , Html.Attributes.attribute "canvas-width" <| String.fromInt <| imageWidth // wdiv
                , Html.Attributes.attribute "canvas-height" <| String.fromInt <| imageHeight // hdiv
                , Html.Attributes.attribute "white-lines" <| String.fromInt Theme.whiteLines
                , Html.Attributes.attribute "completely-real" <| boolToIntString <| isCompletelyReal graph
                , Html.Attributes.attribute "is-3d" <| boolToIntString <| is3D graph
                ]
                []


view : Bool -> { width : Int, height : Int } -> Int -> Row -> Element Msg
view hasClipboard size index row =
    Theme.column
        [ Element.width fill
        , alignTop
        ]
        [ inputLine index row
        , Element.Lazy.lazy4 outputBlock ("canvas" ++ String.fromInt index) hasClipboard size row.output
        , statusLine size.width row
        ]


inputLine : Int -> Row -> Element Msg
inputLine index row =
    Theme.row [ width fill ]
        [ text <| "In [" ++ String.fromInt index ++ "]"
        , Input.text
            [ width fill
            , onEnter <| Calculate index
            , Element.htmlAttribute <| Html.Attributes.attribute "autocorrect" "off"
            , Element.htmlAttribute <| Html.Attributes.attribute "autocapitalize" "none"
            , Element.htmlAttribute <| Html.Attributes.spellcheck False
            ]
            { label = Input.labelHidden "Input"
            , onChange = Input index
            , placeholder = Nothing
            , text = row.input
            }
        , Input.button [ Font.bold, htmlAttribute <| Html.Attributes.title "Press Enter to calculate" ]
            { onPress = Just <| Calculate index
            , label = Icons.enterOutlined Theme.darkIconAttrs
            }
        ]


statusLine : Int -> Row -> Element msg
statusLine pageWidth row =
    case row.output of
        Empty ->
            none

        ParseError e ->
            viewError e

        Parsed e ->
            Theme.column [ alignTop, width <| Element.maximum 200 fill ]
                [ text "Interpreted as: "
                , viewExpression pageWidth e
                ]


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
            , Font.color <| rgb 1 0 0
            ]


label : String -> Element msg
label x =
    el [ alignBottom ] <| text x


viewExpression : Int -> Expression -> Element msg
viewExpression pageWidth expr =
    viewLaTeX pageWidth <| Expression.toTeXString expr


viewSolutionTree : Int -> SolutionTree -> List (Element msg)
viewSolutionTree pageWidth tree =
    case tree of
        SolutionStep e c ->
            viewLaTeX pageWidth (Expression.toTeXString e) :: viewSolutionTree pageWidth c

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


outputBlock : String -> Bool -> { height : Int, width : Int } -> Output -> Element Msg
outputBlock blockId hasClipboard ({ height, width } as size) output =
    let
        showExpr e =
            e
                |> Expression.Value.value Dict.empty
                |> viewValue blockId { wdiv = 1, hdiv = 1 }

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

        viewValue id coeffs v =
            case asExpression v of
                Just ex ->
                    viewExpression width ex

                Nothing ->
                    case v of
                        SymbolicValue s ->
                            viewExpression width s

                        SolutionTreeValue t ->
                            Element.column [ spacing Theme.spacing ] <| viewSolutionTree (width - 2 * Theme.spacing) t

                        GraphValue g ->
                            el [ centerX ] <| draw hasClipboard size id coeffs g

                        ComplexValue c ->
                            text <| Complex.toString c

                        ErrorValue err ->
                            viewError err

                        LambdaValue x f ->
                            case asExpression f of
                                Just e ->
                                    viewExpression width <| Lambda x e

                                Nothing ->
                                    Element.row [] [ text <| x ++ " => ", viewValue id coeffs f ]

                        ListValue ls ->
                            case
                                v
                                    |> genericAsMatrix
                                        (\w ->
                                            case w of
                                                ListValue us ->
                                                    Just us

                                                _ ->
                                                    Nothing
                                        )
                            of
                                Just m ->
                                    roundBracketed <|
                                        List.indexedMap
                                            (\y ->
                                                List.indexedMap
                                                    (\x ->
                                                        viewValue (id ++ "_" ++ String.fromInt y ++ "_" ++ String.fromInt x)
                                                            { wdiv = coeffs.wdiv * Maybe.withDefault 1 (Maybe.map List.length <| List.head m)
                                                            , hdiv = coeffs.hdiv * List.length m
                                                            }
                                                    )
                                            )
                                            m

                                Nothing ->
                                    roundBracketed
                                        [ List.intersperse (label ", ") <|
                                            List.indexedMap
                                                (\i ->
                                                    viewValue (id ++ "_" ++ String.fromInt i)
                                                        { coeffs | wdiv = coeffs.wdiv * List.length ls }
                                                )
                                                ls
                                        ]
    in
    case output of
        Empty ->
            none

        ParseError _ ->
            none

        Parsed e ->
            Theme.column [ alignTop, Element.width fill ]
                [ text "Value:"
                , showExpr e
                ]
