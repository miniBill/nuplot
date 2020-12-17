module UI.RowView exposing (view)

import Complex
import Dict
import Element exposing (Attribute, Element, alignBottom, alignTop, centerX, el, fill, height, htmlAttribute, none, paddingXY, px, rgb, row, shrink, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), Graph(..), KnownFunction(..), RelationOperation(..), UnaryOperation(..), Value(..))
import Expression.NumericRange
import Expression.Value
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra as List
import Model exposing (Msg(..), Output(..), Row)
import UI.Glsl exposing (getGlsl)
import UI.Theme as Theme


draw : { a | pageWidth : Int } -> Int -> Graph -> Element msg
draw { pageWidth } coeff graph =
    let
        isCompletelyReal =
            case graph of
                Contour e ->
                    if Expression.NumericRange.isCompletelyReal e then
                        "1"

                    else
                        "0"

                _ ->
                    "0"

        imageWidth =
            min 1200 <| pageWidth - (1 + coeff) * 3 * Theme.spacing

        imageHeight =
            imageWidth * 3 // 4
    in
    Element.html <|
        Html.node "nu-plot"
            [ Html.Attributes.attribute "expr-src" <| getGlsl graph
            , Html.Attributes.attribute "canvas-width" <| String.fromInt <| imageWidth // coeff
            , Html.Attributes.attribute "canvas-height" <| String.fromInt <| imageHeight // coeff
            , Html.Attributes.attribute "white-lines" <| String.fromInt Theme.whiteLines
            , Html.Attributes.attribute "completely-real" isCompletelyReal
            ]
            []


view : Int -> Int -> Row -> Element Msg
view pageWidth index row =
    Theme.column
        [ width fill
        , alignTop
        ]
        [ inputLine index row
        , outputBlock { pageWidth = pageWidth } row
        , statusLine pageWidth row
        ]


inputLine : Int -> Row -> Element Msg
inputLine index row =
    Theme.row [ width fill ]
        [ text <| "In[" ++ String.fromInt index ++ "]"
        , Input.text [ width <| Element.minimum 600 fill, onEnter <| Calculate index ]
            { label = Input.labelHidden "Input"
            , onChange = Input index
            , placeholder = Nothing
            , text = row.input
            }
        , Input.button [ htmlAttribute <| Html.Attributes.title "Press Enter to calculate" ]
            { onPress = Just <| Calculate index
            , label = text "⏎"
            }
        ]


onEnter : msg -> Attribute msg
onEnter msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\s ->
                if s == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail "ignored"
            )
        |> Html.Events.on "keyup"
        |> htmlAttribute


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
    Element.paragraph
        [ alignTop
        , Font.family [ Font.monospace ]
        , Font.color <| rgb 1 0 0
        ]
    <|
        List.map text <|
            String.split "\n" e


label : String -> Element msg
label x =
    el [ alignBottom ] <| text x


viewExpression : Int -> Expression -> Element msg
viewExpression pageWidth expr =
    el [ height shrink, width shrink ] <|
        Element.html <|
            Html.node "math-jax"
                [ Html.Attributes.attribute "tex-src" <| Expression.toTeXString expr
                , Html.Attributes.attribute "container-width" <| String.fromInt <| pageWidth - 2 * Theme.spacing
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


outputBlock : { a | pageWidth : Int } -> Row -> Element msg
outputBlock model row =
    let
        showExpr e =
            e
                |> Expression.Value.value Dict.empty
                |> viewValue 1

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

        viewValue inList v =
            case asExpression v of
                Just ex ->
                    viewExpression model.pageWidth ex

                Nothing ->
                    case v of
                        SymbolicValue s ->
                            viewExpression model.pageWidth s

                        GraphValue g ->
                            el [ centerX ] <| draw model inList g

                        ComplexValue c ->
                            text <| Complex.toString c

                        ErrorValue err ->
                            viewError err

                        ListValue ls ->
                            let
                                asList l =
                                    case l of
                                        ListValue u ->
                                            Just u

                                        _ ->
                                            Nothing

                                childLists =
                                    List.filterMap asList ls

                                block =
                                    if List.length ls == List.length childLists then
                                        let
                                            width =
                                                childLists
                                                    |> List.map List.length
                                                    |> List.maximum
                                                    |> Maybe.withDefault 0
                                        in
                                        roundBracketed <| List.map (List.map (viewValue <| inList * width)) childLists

                                    else
                                        let
                                            width =
                                                List.length ls
                                        in
                                        roundBracketed [ List.intersperse (label ", ") <| List.map (viewValue <| inList * width) ls ]
                            in
                            block
    in
    case row.output of
        Empty ->
            none

        ParseError _ ->
            none

        Parsed e ->
            Theme.column [ alignTop, width fill ]
                [ text "Value:"
                , showExpr e
                ]
