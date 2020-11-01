module UI.View exposing (view)

import Complex
import Dict
import Element exposing (Element, centerX, column, el, fill, padding, spacing, text, width)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import Expression.Parser
import Model exposing (Model, Msg(..), Row)
import Svg exposing (Svg)
import Svg.Attributes as Svg
import UI.Theme as Theme


marchingSquare : Float -> Float -> { x : Float, val : Bool, lastrowVal : Bool } -> { lastrowVal : Bool, val : Bool, x : Float } -> List ( ( Float, Float ), ( Float, Float ) )
marchingSquare y lastrowY { x, val, lastrowVal } left =
    case ( ( left.lastrowVal, lastrowVal ), ( val, left.val ) ) of
        ( ( True, True ), ( True, True ) ) ->
            []

        ( ( False, False ), ( False, False ) ) ->
            []

        ( ( True, True ), ( True, False ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, y ) ) ]

        ( ( True, True ), ( False, True ) ) ->
            [ ( ( (left.x + x) / 2, y ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( True, False ), ( True, True ) ) ->
            [ ( ( (left.x + x) / 2, lastrowY ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( False, True ), ( True, True ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, lastrowY ) ) ]

        ( ( False, False ), ( False, True ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, y ) ) ]

        ( ( False, False ), ( True, False ) ) ->
            [ ( ( (left.x + x) / 2, y ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( False, True ), ( False, False ) ) ->
            [ ( ( (left.x + x) / 2, lastrowY ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( True, False ), ( False, False ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, lastrowY ) ) ]

        ( ( True, True ), ( False, False ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( False, False ), ( True, True ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( x, (lastrowY + y) / 2 ) ) ]

        ( ( True, False ), ( True, False ) ) ->
            [ ( ( (left.x + x) / 2, lastrowY ), ( (left.x + x) / 2, y ) ) ]

        ( ( False, True ), ( False, True ) ) ->
            [ ( ( (left.x + x) / 2, lastrowY ), ( (left.x + x) / 2, y ) ) ]

        ( ( True, False ), ( False, True ) ) ->
            [ ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, lastrowY ) )
            , ( ( (left.x + x) / 2, y ), ( x, (lastrowY + y) / 2 ) )
            ]

        ( ( False, True ), ( True, False ) ) ->
            [ ( ( (left.x + x) / 2, lastrowY ), ( x, (lastrowY + y) / 2 ) )
            , ( ( left.x, (lastrowY + y) / 2 ), ( (left.x + x) / 2, y ) )
            ]


implicit2d : Float -> List Float -> List Float -> Expression -> Expression -> List (Svg msg)
implicit2d scale xrange yrange l r =
    case yrange of
        [] ->
            []

        miny :: resty ->
            let
                e : Expression
                e =
                    RelationOperation Equals l r

                calculateRow y =
                    List.map (\x -> dvalue x y e > 0) xrange
            in
            resty
                |> List.foldl
                    (\y ( lastrow, lastrowY, acc ) ->
                        let
                            row =
                                calculateRow y

                            lines =
                                case List.map3 (\x val lastrowVal -> { x = x, val = val, lastrowVal = lastrowVal }) xrange row lastrow of
                                    [] ->
                                        []

                                    t :: restrow ->
                                        restrow
                                            |> List.foldl
                                                (\curr ( left, racc ) ->
                                                    ( curr, marchingSquare y lastrowY curr left :: racc )
                                                )
                                                ( t, [] )
                                            |> Tuple.second
                                            |> List.concat
                        in
                        ( row, y, lines :: acc )
                    )
                    ( calculateRow miny, miny, [] )
                |> (\( _, _, a ) -> a)
                |> List.concat
                |> List.map
                    (\( ( x1, y1 ), ( x2, y2 ) ) ->
                        Svg.line
                            [ Svg.x1 <| String.fromFloat <| scale * x1
                            , Svg.y1 <| String.fromFloat <| scale * y1
                            , Svg.x2 <| String.fromFloat <| scale * x2
                            , Svg.y2 <| String.fromFloat <| scale * y2
                            ]
                            []
                    )


dvalue : Float -> Float -> Expression -> Float
dvalue x y e =
    Complex.real <|
        Expression.value
            (Dict.fromList
                [ ( "x", Complex.fromReal x )
                , ( "y", Complex.fromReal y )
                ]
            )
            e


draw : Graph -> Element msg
draw graph =
    let
        halfSize =
            250

        steps =
            250

        minx =
            -5

        maxx =
            5

        miny =
            -5

        maxy =
            5

        scale =
            100

        xrange () =
            List.range 0 (steps - 1)
                |> List.map toFloat
                |> List.map (\x -> (x / (steps - 1)) * (maxx - minx) + minx)

        yrange () =
            List.range 0 (steps - 1)
                |> List.map toFloat
                |> List.map (\y -> (y / (steps - 1)) * (maxy - miny) + miny)

        content =
            case graph of
                Explicit2D e ->
                    -- text <| "y = " ++ Expression.toString e
                    xrange ()
                        |> List.map
                            (\x ->
                                ( x
                                , Complex.real <| Expression.value (Dict.singleton "x" <| Complex.fromReal x) e
                                )
                            )
                        |> List.foldl
                            (\( x, y ) ( old, acc ) ->
                                ( Just ( x, y )
                                , case old of
                                    Nothing ->
                                        acc

                                    Just ( ox, oy ) ->
                                        Svg.line
                                            [ Svg.x1 <| String.fromFloat <| scale * ox
                                            , Svg.y1 <| String.fromFloat <| scale * -oy
                                            , Svg.x2 <| String.fromFloat <| scale * x
                                            , Svg.y2 <| String.fromFloat <| scale * -y
                                            ]
                                            []
                                            :: acc
                                )
                            )
                            ( Nothing, [] )
                        |> Tuple.second

                Relation2D rop l r ->
                    xrange ()
                        |> List.concatMap
                            (\x ->
                                yrange ()
                                    |> List.concatMap
                                        (\y ->
                                            if 0 < (dvalue x y <| RelationOperation rop l r) then
                                                [ Svg.rect
                                                    [ Svg.x <| String.fromFloat <| scale * x
                                                    , Svg.y <| String.fromFloat <| scale * y
                                                    , Svg.width <| String.fromFloat <| scale * (maxx - minx) / steps
                                                    , Svg.height <| String.fromFloat <| scale * (maxy - miny) / steps
                                                    ]
                                                    []
                                                ]

                                            else
                                                []
                                        )
                            )

                Implicit2D l r ->
                    implicit2d scale (xrange ()) (yrange ()) l r

        axes =
            case graph of
                Explicit2D _ ->
                    axes2 ()

                Implicit2D _ _ ->
                    axes2 ()

                Relation2D _ _ _ ->
                    axes2 ()

        axes2 () =
            [ Svg.line
                [ Svg.stroke "red"
                , Svg.x1 <| String.fromFloat <| scale * minx
                , Svg.y1 "0"
                , Svg.x2 <| String.fromFloat <| scale * maxx
                , Svg.y2 "0"
                ]
                []
            , Svg.line
                [ Svg.stroke "green"
                , Svg.x1 "0"
                , Svg.y1 <| String.fromFloat <| scale * miny
                , Svg.x2 "0"
                , Svg.y2 <| String.fromFloat <| scale * maxy
                ]
                []
            ]
    in
    Element.html <|
        Svg.svg
            [ Svg.viewBox <| String.join " " <| List.map (String.fromFloat << (*) scale) [ minx, miny, maxx - minx, maxy - miny ]
            , Svg.width <| String.fromInt <| 2 * halfSize
            , Svg.height <| String.fromInt <| 2 * halfSize
            , Svg.stroke "black"
            , Svg.strokeWidth <| String.fromFloat <| Basics.min (maxy - miny) (maxx - minx) / 10
            ]
            (axes ++ content)


viewRow : Int -> Row -> Element Msg
viewRow index { input, graph } =
    let
        inputLine =
            Input.text [ width fill ]
                { label = Input.labelHidden "Input"
                , onChange =
                    \newValue ->
                        Input
                            { row = index
                            , value = newValue
                            }
                , placeholder = Just <| Input.placeholder [] <| text "y = f(x)"
                , text = input
                }

        outputBlock =
            if String.isEmpty input then
                []

            else
                let
                    parsed =
                        Expression.Parser.parse input
                in
                case parsed of
                    Err e ->
                        [ el [ Font.family [ Font.monospace ] ] <| text <| Expression.Parser.errorsToString input e ]

                    Ok expr ->
                        let
                            gg =
                                Expression.Parser.parseGraph expr
                        in
                        [ text <| "Interpreted as: " ++ Expression.toString expr
                        , Element.Keyed.el [ centerX ] ( input, draw gg )
                        ]
    in
    column
        [ padding Theme.spacing
        , spacing Theme.spacing
        , width fill
        ]
        (inputLine :: outputBlock)


view : Model -> Element Msg
view model =
    column
        [ spacing <| 2 * Theme.spacing
        , width fill
        ]
    <|
        List.indexedMap (\index row -> Element.Lazy.lazy2 viewRow index row) model
