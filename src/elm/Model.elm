module Model exposing (CellMsg(..), Document, Flags, Modal(..), Model, Msg(..), Output(..), Row, RowData(..), Size, documentFromFile, documentsCodec, emptyRow)

import Codec exposing (Codec)
import Expression exposing (Expression)
import File exposing (File)
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
    , openMenu : Bool
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


unescape : String -> String
unescape =
    String.trim
        >> String.replace "\\n" "\n"
        >> String.replace "\\\\" "\\"


escape : String -> String
escape =
    String.replace "\\" "\\\\"
        >> String.replace "\n" "\\n"


rowParser : Parser Row
rowParser =
    let
        codeRow =
            Parser.succeed (CodeRow Empty)
                |. Parser.symbol ">"

        markdownRow =
            Parser.succeed MarkdownRow
    in
    Parser.succeed
        (\data input ->
            { input = input
            , editing = False
            , data = data
            }
        )
        |= Parser.oneOf
            [ codeRow
            , markdownRow
            ]
        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))


type alias Size =
    { width : Int
    , height : Int
    }


type alias Row =
    { input : String
    , editing : Bool
    , data : RowData
    }


type RowData
    = CodeRow Output
    | MarkdownRow


rowCodec : Codec Row
rowCodec =
    Codec.map
        (\i ->
            if String.startsWith ">" i then
                { input = String.trim <| String.dropLeft 1 i
                , editing = False
                , data = CodeRow Empty
                }

            else
                { input = i
                , editing = False
                , data = MarkdownRow
                }
        )
        (\r ->
            case r.data of
                MarkdownRow ->
                    r.input

                CodeRow _ ->
                    "> " ++ r.input
        )
        Codec.string


emptyRow : Row
emptyRow =
    { input = ""
    , editing = True
    , data = CodeRow Empty
    }


type Output
    = Empty
    | ParseError String
    | Parsed Expression


type Msg
    = NewDocument
    | SelectDocument Int
    | RenameDocument (Maybe String)
    | CloseDocument { ask : Bool, index : Int }
    | Resized Size
    | DismissModal
    | SetModal Modal
    | ToggleMenu Bool
    | Open
    | SelectedFileForOpen File
    | ReadFile String String
    | CellMsg Int CellMsg


type CellMsg
    = EndEditing
    | ToCode
    | ToMarkdown
    | StartEditing
    | Calculate
    | Input String
    | Save String
    | Copy String
