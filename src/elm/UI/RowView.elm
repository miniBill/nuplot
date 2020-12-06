module UI.RowView exposing (view)

import Complex
import Dict
import Element exposing (Element, alignBottom, alignTop, centerX, centerY, column, el, fill, height, none, paddingEach, px, rgb, row, scale, shrink, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), Graph(..), KnownFunction(..), RelationOperation(..), UnaryOperation(..), Value(..), greeks)
import Expression.NumericRange
import Expression.Utils
import Expression.Value
import Html
import Html.Attributes
import List.Extra as List
import Model exposing (Msg(..), Output(..), Row)
import UI.Glsl exposing (getGlsl)
import UI.Theme as Theme


draw : Bool -> Graph -> Element msg
draw inList graph =
    let
        coeff =
            if inList then
                4

            else
                1

        isCompletelyReal =
            case graph of
                Contour e ->
                    if Expression.NumericRange.isCompletelyReal e then
                        "1"

                    else
                        "0"

                _ ->
                    "0"
    in
    Element.html <|
        Html.node "nu-plot"
            [ Html.Attributes.attribute "expr-src" <| getGlsl graph
            , Html.Attributes.attribute "canvas-width" <| String.fromInt <| Theme.imageWidth // coeff
            , Html.Attributes.attribute "canvas-height" <| String.fromInt <| Theme.imageHeight // coeff
            , Html.Attributes.attribute "white-lines" <| String.fromInt Theme.whiteLines
            , Html.Attributes.attribute "completely-real" isCompletelyReal
            ]
            []


view : Int -> Row -> Element Msg
view index row =
    Theme.column
        [ width fill
        , alignTop
        ]
        [ inputLine index row
        , statusLine row
        , outputBlock row
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
            Element.row [ width <| Element.maximum 200 shrink ]
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
    blockToElement <|
        viewRelationExpression expr


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
                [ squareBracketed (blockColumn <| List.map viewReplacement <| Dict.toList rs)
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

                viewRow us =
                    blockRow <| List.intersperse (label " ") <| List.map viewRelationExpression us

                block =
                    if List.length ls == List.length childLists then
                        roundBracketed <| (blockColumn <| List.map viewRow childLists)

                    else
                        curlyBracketed <| blockRow <| List.intersperse (label ", ") <| List.map viewRelationExpression ls
            in
            block

        Integer n ->
            label <| String.fromInt n

        Float f ->
            label <| String.fromFloat f

        Variable v ->
            label <| Maybe.withDefault v <| Dict.get v greeks

        Apply (KnownFunction Abs) [ arg ] ->
            absBracketed <| viewRelationExpression arg

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
                , roundBracketed <| blockRow <| List.intersperse (label ",") (List.map viewRelationExpression args)
                ]

        BinaryOperation Power b e ->
            roundBracketed <| innerViewPower b e

        UnaryOperation Negate e ->
            roundBracketed <| innerViewNegate e

        AssociativeOperation Addition l m r ->
            roundBracketed <| innerViewAddition l m r

        AssociativeOperation Multiplication l m r ->
            roundBracketed <| innerViewMultiplication l m r

        BinaryOperation Division n d ->
            roundBracketed <| innerViewDivision n d

        RelationOperation o l r ->
            roundBracketed <| innerViewRelation o l r


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


roundBracketed : Block msg -> Block msg
roundBracketed =
    bracketed "(" "⎛" "⎜" "⎜" "⎝" ")" "⎞" "⎟" "⎟" "⎠"


squareBracketed : Block msg -> Block msg
squareBracketed =
    bracketed "[" "⎡" "⎢" "⎢" "⎣" "]" "⎤" "⎥" "⎥" "⎦"


curlyBracketed : Block msg -> Block msg
curlyBracketed =
    bracketed "{" "⎧" "⎨" "⎪" "⎩" "}" "⎫" "⎬" "⎪" "⎭"


absBracketed : Block msg -> Block msg
absBracketed =
    bracketed "⎪" "⎪" "⎪" "⎪" "⎪" "⎪" "⎪" "⎪" "⎪" "⎪"


bracketed : String -> String -> String -> String -> String -> String -> String -> String -> String -> String -> Block msg -> Block msg
bracketed sl ul cl exl bl sr ur cr exr br =
    wrap
        (\{ height, elements } ->
            case height of
                0 ->
                    el [ alignBottom ] (text sl) :: elements ++ [ el [ alignBottom ] (text sr) ]

                1 ->
                    el [ alignBottom ] (text sl) :: elements ++ [ el [ alignBottom ] (text sr) ]

                2 ->
                    if cl == exl then
                        column [ alignBottom ] [ text ul, text bl ]
                            :: elements
                            ++ [ column [ alignBottom ] [ text ur, text br ] ]

                    else
                        el [ alignBottom, scale <| toFloat height ] (text sl)
                            :: elements
                            ++ [ el [ alignBottom, scale <| toFloat height ] (text sr) ]

                _ ->
                    let
                        col u c e b =
                            column [ alignBottom ] <|
                                List.concat <|
                                    let
                                        halfway =
                                            List.repeat ((height - 3) // 2) (text e)
                                    in
                                    if modBy 2 height == 1 then
                                        [ [ text u ]
                                        , halfway
                                        , [ text c ]
                                        , halfway
                                        , [ text b ]
                                        ]

                                    else
                                        [ [ text u ]
                                        , halfway
                                        , [ text e ]
                                        , [ text c ]
                                        , halfway
                                        , [ text b ]
                                        ]
                    in
                    col ul cl exl bl
                        :: elements
                        ++ [ col ur cr exr br ]
        )


blockToElement : Block msg -> Element msg
blockToElement { elements } =
    Element.row [ Font.italic ] elements


outputBlock : Row -> Element msg
outputBlock row =
    let
        showExpr =
            Expression.Value.value Dict.empty
                >> showValue False
                >> blockToElement

        showValue inList v =
            case v of
                SymbolicValue s ->
                    viewRelationExpression s

                GraphValue g ->
                    { elements = [ el [ centerX ] <| draw inList g ]
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

                        viewRow us =
                            blockRow <| List.intersperse (label " ") <| List.map (showValue True) us

                        block =
                            if List.length ls == List.length childLists then
                                roundBracketed <| (blockColumn <| List.map viewRow childLists)

                            else
                                curlyBracketed <| blockRow <| List.intersperse (label ", ") <| List.map (showValue True) ls
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
                |> el [ centerX ]

        Parsed e ->
            Theme.column []
                [ showExpr e

                --, Element.paragraph [] [ text <| Debug.toString <| Expression.Value.value Dict.empty e ]
                ]
