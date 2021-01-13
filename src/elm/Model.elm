module Model exposing (Document, Flags, Menu(..), Modal(..), Model, Msg(..), Output(..), Row(..), Size, documentFromFile, documentsCodec, emptyRow)

import Codec exposing (Codec)
import Expression exposing (Expression)
import File exposing (File)
import Html.Attributes exposing (type_)
import Json.Decode as JD
import Parser exposing ((|.), (|=), Parser)
import Zipper exposing (Zipper)


type alias Flags =
    { saved : JD.Value
    , hasClipboard : Bool
    }


type alias Model =
    { documents : Maybe (Zipper Document)
    , modal : Maybe Modal
    , size : Size
    , hasClipboard : Bool
    , openMenu : Maybe Menu
    }


type Modal
    = ModalClose Int
    | ModalRename String


documentsCodec : Codec (Maybe (Zipper Document))
documentsCodec =
    -- We convert to a list to prevent failures to be read as a valid empty value
    Codec.list (Zipper.codec documentCodec)
        |> Codec.map List.head
            (\c ->
                case c of
                    Nothing ->
                        []

                    Just v ->
                        [ v ]
            )


type alias Document =
    { name : String
    , changed : Bool
    , rows : List Row
    }


documentCodec : Codec Document
documentCodec =
    Codec.object (\n rs -> { name = n, changed = False, rows = rs })
        |> Codec.field "name" .name Codec.string
        |> Codec.field "rows" .rows (Codec.list rowCodec)
        |> Codec.buildObject


documentFromFile : String -> String -> Document
documentFromFile name file =
    Parser.run
        (Parser.succeed (\rows -> { name = name, changed = False, rows = rows })
            |= rowsParser
        )
        file
        |> Result.withDefault
            { name = "Untitled"
            , changed = False
            , rows = []
            }


rowsParser : Parser (List Row)
rowsParser =
    Parser.sequence
        { start = ""
        , end = ""
        , item = rowParser
        , separator = "\n"
        , spaces = Parser.succeed ()
        , trailing = Parser.Optional
        }


rowParser : Parser Row
rowParser =
    let
        codeRow =
            Parser.succeed
                (\input ->
                    CodeRow
                        { input =
                            input
                                |> String.trim
                                |> String.replace "\\n" "\n"
                                |> String.replace "\\\\" "\\"
                        , output = Empty
                        }
                )
                |. Parser.symbol ">"
                |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))

        markdownRow =
            Parser.succeed (MarkdownRow << String.trim)
                |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
    in
    Parser.oneOf
        [ codeRow
        , markdownRow
        ]


type alias Size =
    { width : Int
    , height : Int
    }


type Row
    = CodeRow
        { input : String
        , output : Output
        }
    | MarkdownRow String


rowCodec : Codec Row
rowCodec =
    Codec.map
        (\i ->
            if String.startsWith ">" i then
                CodeRow
                    { input = String.trim <| String.dropLeft 1 i
                    , output = Empty
                    }

            else
                MarkdownRow i
        )
        (\r ->
            case r of
                MarkdownRow mr ->
                    mr

                CodeRow { input } ->
                    "> " ++ input
        )
        Codec.string


emptyRow : Row
emptyRow =
    CodeRow
        { input = ""
        , output = Empty
        }


type Output
    = Empty
    | ParseError String
    | Parsed Expression


type Msg
    = Input Int String
    | Calculate Int
    | NewDocument
    | SelectDocument Int
    | RenameDocument (Maybe String)
    | CloseDocument { ask : Bool, index : Int }
    | Resized Size
    | DismissModal
    | SetModal Modal
    | Save String
    | Copy String
    | ToggleMenu Menu
    | Open
    | SelectedFileForOpen File
    | ReadFile String String


type Menu
    = File
