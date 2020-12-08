module UI.RowView exposing (view)

import Complex
import Dict
import Element exposing (Element, alignBottom, alignTop, centerX, centerY, column, el, fill, height, none, paddingEach, paddingXY, px, rgb, row, shrink, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), Graph(..), KnownFunction(..), RelationOperation(..), UnaryOperation(..), Value(..), greeks)
import Expression.NumericRange
import Expression.Value
import Html
import Html.Attributes
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
            pageWidth - (1 + coeff) * 3 * Theme.spacing

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
        , statusLine row
        , outputBlock { pageWidth = pageWidth } row
        ]


inputLine : Int -> Row -> Element Msg
inputLine index row =
    Theme.row [ width fill ]
        [ text <| "In[" ++ String.fromInt index ++ "]"
        , Input.text [ width <| Element.minimum 600 fill ]
            { label = Input.labelHidden "Input"
            , onChange =
                \newValue ->
                    Input
                        { row = index
                        , input = newValue
                        }
            , placeholder = Just <| Input.placeholder [] <| text "y = f(x)"
            , text = row.input
            }
        ]


statusLine : Row -> Element msg
statusLine row =
    case row.output of
        Empty ->
            none

        Typing _ ->
            text "Typing..."

        ParseError e ->
            viewError e

        Parsed e ->
            Theme.column [ width <| Element.maximum 200 shrink ]
                [ text "Interpreted as: "
                , viewExpression e
                ]


viewError : String -> Element msg
viewError e =
    el
        [ Font.family [ Font.monospace ]
        , Font.color <| rgb 1 0 0
        ]
        (Element.html <|
            Html.pre [] [ Html.text e ]
        )


type alias Block msg =
    { elements : List (Element msg)
    , height : Int
    }


blockRow : List (Block msg) -> Block msg
blockRow blocks =
    { elements = List.concatMap .elements blocks
    , height = Maybe.withDefault 0 <| List.maximum <| List.map .height blocks
    }


blockColumn : List (Block msg) -> Block msg
blockColumn blocks =
    { elements =
        [ column [ alignBottom ] <|
            List.map (\{ elements } -> row [] elements) blocks
        ]
    , height = List.sum <| List.map .height blocks
    }


label : String -> Block msg
label x =
    { elements = [ el [ alignBottom ] <| text x ]
    , height = 1
    }


viewExpression : Expression -> Element msg
viewExpression expr =
    el [ height shrink, width shrink ] <|
        Element.html <|
            Html.node "math-jax"
                [ Html.Attributes.attribute "tex-src" <| Expression.toTeXString expr
                ]
                []


viewRelationExpression : Expression -> Block msg
viewRelationExpression expr =
    case expr of
        RelationOperation o l r ->
            innerViewRelation o l r

        _ ->
            viewAddSubtraction expr


innerViewRelation : RelationOperation -> Expression -> Expression -> Block msg
innerViewRelation o l r =
    blockRow [ viewAddSubtraction l, viewRelation o, viewAddSubtraction r ]


viewRelation : RelationOperation -> Block msg
viewRelation op =
    label <| Expression.relationToString op


viewAddSubtraction : Expression -> Block msg
viewAddSubtraction expr =
    case expr of
        AssociativeOperation Addition l m r ->
            innerViewAddition l m r

        UnaryOperation Negate e ->
            innerViewNegate e

        _ ->
            viewMultiDivision expr


innerViewAddition : Expression -> Expression -> List Expression -> Block msg
innerViewAddition l m r =
    let
        helper e =
            case e of
                UnaryOperation Negate c ->
                    blockRow [ label " - ", viewMultiDivision c ]

                _ ->
                    blockRow [ label " + ", viewMultiDivision e ]
    in
    blockRow <| viewMultiDivision l :: List.map helper (m :: r)


innerViewNegate : Expression -> Block msg
innerViewNegate e =
    blockRow [ label "-", viewMultiDivision e ]


viewMultiDivision : Expression -> Block msg
viewMultiDivision expr =
    case expr of
        AssociativeOperation Multiplication l m r ->
            innerViewMultiplication l m r

        BinaryOperation Division n d ->
            innerViewDivision n d

        _ ->
            viewPower expr


innerViewMultiplication : Expression -> Expression -> List Expression -> Block msg
innerViewMultiplication l m r =
    blockRow <| List.intersperse (label "\u{2009}") <| List.map viewPower (l :: m :: r)


innerViewDivision : Expression -> Expression -> Block msg
innerViewDivision n d =
    let
        top =
            blockRow [ label " ", label " ", viewMultiDivision n, label " ", label " " ]

        hr =
            el [ width fill, paddingEach { top = 3, bottom = 1, left = 0, right = 0 } ] <|
                el
                    [ width fill
                    , height <| px 1
                    , Border.widthEach { top = 1, bottom = 1, right = 0, left = 0 }
                    ]
                    none

        bottom =
            blockRow [ label " ", label " ", viewMultiDivision d, label " ", label " " ]
    in
    { elements =
        [ column []
            [ row [ centerX ] <| top.elements
            , hr
            , row [ centerX ] <| bottom.elements
            ]
        ]
    , height = top.height + bottom.height
    }


viewPower : Expression -> Block msg
viewPower expr =
    case expr of
        BinaryOperation Power b e ->
            innerViewPower b e

        _ ->
            viewAtom expr


innerViewPower : Expression -> Expression -> Block msg
innerViewPower b e =
    let
        eBlock =
            viewPower e

        bBlock =
            viewAtom b
    in
    { elements =
        [ Theme.grid [ alignBottom, spacing 0 ]
            [ [ none, row [] <| eBlock.elements ]
            , [ row [] <| bBlock.elements, none ]
            ]
        ]
    , height = eBlock.height + bBlock.height
    }


wrap : (Block msg -> List (Element msg)) -> Block msg -> Block msg
wrap f block =
    { block | elements = f block }


viewAtom : Expression -> Block msg
viewAtom expr =
    case expr of
        Replace rs e ->
            blockRow
                [ squareBracketed <| List.map (\r -> [ viewReplacement r ]) <| Dict.toList rs
                , viewRelationExpression e
                ]

        List ls ->
            let
                childLists =
                    ls
                        |> List.filterMap
                            (\l ->
                                case l of
                                    List u ->
                                        Just u

                                    _ ->
                                        Nothing
                            )

                block =
                    if List.length ls == List.length childLists then
                        roundBracketed <| List.map (List.map viewRelationExpression) childLists

                    else
                        curlyBracketed [ List.intersperse (label ", ") <| List.map viewRelationExpression ls ]
            in
            block

        Integer n ->
            label <| String.fromInt n

        Float f ->
            label <| String.fromFloat f

        Variable v ->
            label <| Maybe.withDefault v <| Dict.get v greeks

        Apply (KnownFunction Abs) [ arg ] ->
            absBracketed [ [ viewRelationExpression arg ] ]

        Apply (KnownFunction Sqrt) [ arg ] ->
            blockRow
                [ label "√"
                , viewRelationExpression arg
                    |> wrap
                        (\{ elements } ->
                            [ row
                                [ Border.widthEach
                                    { top = 1
                                    , left = 0
                                    , right = 0
                                    , bottom = 0
                                    }
                                ]
                                elements
                            ]
                        )
                ]

        Apply name args ->
            blockRow
                [ { elements = [ el [ centerY ] <| text <| Expression.functionNameToString name ]
                  , height = 1
                  }
                , roundBracketed [ List.intersperse (label ",") (List.map viewRelationExpression args) ]
                ]

        BinaryOperation Power b e ->
            roundBracketed [ [ innerViewPower b e ] ]

        UnaryOperation Negate e ->
            roundBracketed [ [ innerViewNegate e ] ]

        AssociativeOperation Addition l m r ->
            roundBracketed [ [ innerViewAddition l m r ] ]

        AssociativeOperation Multiplication l m r ->
            roundBracketed [ [ innerViewMultiplication l m r ] ]

        BinaryOperation Division n d ->
            roundBracketed [ [ innerViewDivision n d ] ]

        RelationOperation o l r ->
            roundBracketed [ [ innerViewRelation o l r ] ]


viewReplacement : ( String, Expression ) -> Block msg
viewReplacement ( name, value ) =
    blockRow
        [ wrap (\{ elements } -> [ row [ centerY ] elements ]) <|
            blockRow
                [ viewAtom <| Variable name
                , label " ← "
                ]
        , viewRelationExpression value
        ]


roundBracketed : List (List (Block msg)) -> Block msg
roundBracketed =
    simplyBracketed
        "("
        { left = Theme.bracketBorderWidth, top = 0, bottom = 0, right = 0 }
        { topLeft = 999, bottomLeft = 999, topRight = 0, bottomRight = 0 }
        ")"
        { left = 0, top = 0, bottom = 0, right = Theme.bracketBorderWidth }
        { topRight = 999, bottomRight = 999, topLeft = 0, bottomLeft = 0 }


squareBracketed : List (List (Block msg)) -> Block msg
squareBracketed =
    simplyBracketed
        "["
        { left = Theme.bracketBorderWidth, top = Theme.bracketBorderWidth, bottom = Theme.bracketBorderWidth, right = 0 }
        { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }
        "]"
        { left = 0, top = Theme.bracketBorderWidth, bottom = Theme.bracketBorderWidth, right = Theme.bracketBorderWidth }
        { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }


absBracketed : List (List (Block msg)) -> Block msg
absBracketed blocks =
    let
        space =
            { height = 1
            , elements = [ el [ height <| px 4 ] none ]
            }
    in
    simplyBracketed
        "|"
        { left = Theme.bracketBorderWidth, top = 0, bottom = 0, right = 0 }
        { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }
        "|"
        { left = 0, top = 0, bottom = 0, right = Theme.bracketBorderWidth }
        { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }
        [ [ blockColumn <| [ space ] ++ List.map blockRow blocks ++ [ space ] ] ]


curlyBracketed : List (List (Block msg)) -> Block msg
curlyBracketed blocks =
    let
        e =
            el [ height fill, width <| px <| Theme.bracketWidth // 2 - 1 ] none

        no =
            { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }

        l =
            { left = Theme.bracketBorderWidth, right = 0, top = 0, bottom = 0 }

        r =
            { right = Theme.bracketBorderWidth, left = 0, top = 0, bottom = 0 }

        blocksHeight =
            List.sum <| List.map (Maybe.withDefault 0 << List.maximum << List.map .height) blocks

        piece we re =
            el
                [ Border.widthEach we
                , Border.roundEach re
                , width <| px <| Theme.bracketWidth // 2 + 1
                , height <| px <| (Theme.fontSize + 4) * blocksHeight // 4
                ]
                none

        ur =
            piece l { no | topLeft = 999 }

        ul =
            piece r { no | topRight = 999 }

        br =
            piece l { no | bottomLeft = 999 }

        bl =
            piece r { no | bottomRight = 999 }

        bra =
            column [ height fill, width <| px Theme.bracketWidth ] << List.map (row [])

        lbra =
            bra
                [ [ e, ur ], [ bl, e ], [ ul, e ], [ e, br ] ]

        rbra =
            bra
                [ [ ul, e ], [ e, br ], [ e, ur ], [ bl, e ] ]
    in
    bracketed "{" lbra "}" rbra blocks


simplyBracketed :
    String
    -> { bottom : Int, left : Int, right : Int, top : Int }
    -> { topLeft : Int, topRight : Int, bottomLeft : Int, bottomRight : Int }
    -> String
    -> { bottom : Int, left : Int, right : Int, top : Int }
    -> { topLeft : Int, topRight : Int, bottomLeft : Int, bottomRight : Int }
    -> List (List (Block msg))
    -> Block msg
simplyBracketed ls lw lr rs rw rr blocks =
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
    bracketed ls (bracket lw lr) rs (bracket rw rr) blocks


bracketed : String -> Element msg -> String -> Element msg -> List (List (Block msg)) -> Block msg
bracketed ls l rs r blocks =
    let
        rowHeight =
            Maybe.withDefault 0 << List.maximum << List.map .height

        height =
            List.sum <| List.map rowHeight blocks

        prepareRow =
            List.map (\{ elements } -> row [ width shrink ] elements)

        grid =
            blocks
                |> List.map prepareRow
                |> Theme.grid [ alignBottom ]

        wrapped =
            if height == 1 then
                [ row [ spacing 2 ]
                    [ el [ alignBottom, Font.unitalicized ] <| text ls
                    , grid
                    , el [ alignBottom, Font.unitalicized ] <| text rs
                    ]
                ]

            else
                [ row [ spacing 1, paddingXY 2 0 ] [ l, grid, r ] ]
    in
    { elements = wrapped
    , height = height
    }


blockToElement : Block msg -> Element msg
blockToElement { elements } =
    Element.row [ Font.italic ] elements


outputBlock : { a | pageWidth : Int } -> Row -> Element msg
outputBlock model row =
    let
        showExpr =
            Expression.Value.value Dict.empty
                >> showValue 1
                >> blockToElement

        showValue inList v =
            case v of
                SymbolicValue s ->
                    viewRelationExpression s

                GraphValue g ->
                    { elements = [ el [ centerX ] <| draw model inList g ]
                    , height = 1
                    }

                ComplexValue c ->
                    label <| Complex.toString c

                ErrorValue err ->
                    { elements = [ viewError err ]
                    , height = 1
                    }

                ListValue ls ->
                    let
                        childLists =
                            ls
                                |> List.filterMap
                                    (\l ->
                                        case l of
                                            ListValue u ->
                                                Just u

                                            _ ->
                                                Nothing
                                    )

                        block =
                            if List.length ls == List.length childLists then
                                let
                                    width =
                                        childLists
                                            |> List.map List.length
                                            |> List.maximum
                                            |> Maybe.withDefault 0
                                in
                                roundBracketed <| List.map (List.map (showValue width)) childLists

                            else
                                let
                                    width =
                                        List.length ls
                                in
                                curlyBracketed [ List.intersperse (label ", ") <| List.map (showValue width) ls ]
                    in
                    block
    in
    case row.output of
        Empty ->
            none

        ParseError _ ->
            none

        Typing e ->
            Maybe.map showExpr e
                |> Maybe.withDefault none

        Parsed e ->
            Theme.column []
                [ text "Output:"
                , showExpr e
                ]
