module AssumptionParser exposing (processAssumptions)

import Expression exposing (Expression)
import ParserModel exposing (..)


processAssumptions : (String -> Result ParserError Expression) -> String -> Result ParserError Expression
processAssumptions parse string =
    let
        close =
            String.indexes "]" string
                |> List.head
                |> Maybe.withDefault -1
    in
    if close < 0 then
        Err <| Other "Opening [ with no corresponding closing ]"

    else
        let
            open =
                String.indexes "[" string
                    |> List.head
                    |> Maybe.withDefault -1
        in
        if open /= 0 && not (String.startsWith "'" string) then
            Err <| Other "[ is only allowed as the first char (or second char following ')"

        else if close == open + 1 then
            parse <| String.dropLeft 2 string

        else
            let
                assume =
                    String.slice (open + 1) close string

                rest =
                    String.dropLeft (close + 1) string

                assumed =
                    parseAssumptions assume

                parsed =
                    parse rest
            in
            Expression.partialSubstitute assumed parsed


parseAssumptions =
    String.split ";" >> List.map parseAssumption


parseAssumption inner =
    case String.uncons inner of
        Nothing ->
            Err "Empty assumption"

        Just ( letter, rest ) ->
            if String.length rest == 0 || rest == "=" then
                Err "Valueless assumption"

            else
                Ok <|
                    if String.startsWith "=" rest then
                        ( letter, String.dropLeft 1 rest )

                    else
                        ( letter, rest )
