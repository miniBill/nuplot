module ParserModel exposing (ParserError(..))


type ParserError
    = EmptyInput
    | Other String
