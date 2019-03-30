module ParserModel exposing (ParserError(..), parserErrorToString)


type ParserError
    = EmptyInput
    | Other String


parserErrorToString : ParserError -> String
parserErrorToString =
    Debug.todo "parserErrorToString"
