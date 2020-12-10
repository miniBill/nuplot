module UI.Theme exposing (bracketBorderWidth, bracketWidth, column, fontSize, grid, row, spacing, whiteLines, wrappedRow)

import Element exposing (Attribute, Element, none, shrink)


fontSize : number
fontSize =
    20


spacing : number
spacing =
    10


whiteLines : number
whiteLines =
    6


bracketWidth : number
bracketWidth =
    8


bracketBorderWidth : number
bracketBorderWidth =
    2


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row (Element.spacing spacing :: attrs)


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs =
    Element.wrappedRow (Element.spacing spacing :: attrs)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column (Element.spacing spacing :: attrs)


grid : List (Attribute msg) -> List (List (Element msg)) -> Element msg
grid attrs rows =
    if List.isEmpty rows then
        none

    else
        let
            toColumn i =
                { width = shrink
                , header = none
                , view =
                    \x ->
                        x
                            |> List.drop i
                            |> List.head
                            |> Maybe.withDefault none
                            |> Element.el [ Element.alignBottom ]
                }

            w =
                List.map List.length rows
                    |> List.maximum
                    |> Maybe.withDefault 0
        in
        Element.table (Element.spacing spacing :: attrs)
            { columns = List.map toColumn <| List.range 0 (w - 1)
            , data = rows
            }
