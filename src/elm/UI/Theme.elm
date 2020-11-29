module UI.Theme exposing (column, grid, imageHeight, imageWidth, paintInForegroud, row, spacing)

import Element exposing (Attribute, Element, none, shrink)


paintInForegroud : Bool
paintInForegroud =
    True


spacing : number
spacing =
    10


imageWidth : number
imageWidth =
    1024


imageHeight : number
imageHeight =
    768


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row (Element.spacing spacing :: attrs)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column (Element.spacing spacing :: attrs)


grid : List (Attribute msg) -> List (List (Element msg)) -> Element msg
grid attrs rows =
    case rows of
        [] ->
            none

        r :: _ ->
            let
                toColumn i _ =
                    { width = shrink
                    , header = none
                    , view =
                        \x ->
                            x
                                |> List.drop i
                                |> List.head
                                |> Maybe.withDefault none
                    }
            in
            Element.table (Element.spacing spacing :: attrs)
                { columns = List.indexedMap toColumn r
                , data = rows
                }
