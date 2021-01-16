module Model exposing (CellMsg(..), Context, Document, Flags, Language(..), Modal(..), Model, Msg(..), Output(..), Row, RowData(..), Size, documentFromFile, documentToFile, documentsCodec, emptyRow)

import Codec exposing (Codec)
import Expression exposing (Expression)
import File exposing (File)
import Json.Decode as JD
import Zipper exposing (Zipper)


type alias Flags =
    { saved : JD.Value
    , hasClipboard : Bool
    , languages : List String
    }


type alias Model =
    { documents : Maybe (Zipper Document)
    , modal : Maybe Modal
    , size : Size
    , openMenu : Bool
    , context : Context
    }


type alias Context =
    { language : Language
    , hasClipboard : Bool
    }


type Language
    = En
    | It


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


documentToFile : Document -> String
documentToFile { rows } =
    let
        rowToString { input, data } =
            case data of
                MarkdownRow ->
                    input

                CodeRow _ ->
                    input
                        |> String.split "\n"
                        |> List.map (\l -> "> " ++ l)
                        |> String.join "\n"
    in
    String.join "\n\n" <| List.map rowToString rows


documentFromFile : String -> String -> Document
documentFromFile name file =
    file
        |> String.split "\n\n"
        |> List.concatMap parseRow
        |> (\rows -> { name = name, changed = False, rows = rows })


parseRow : String -> List Row
parseRow row =
    let
        parseFragment f =
            if String.startsWith ">" f then
                { input = String.trim (String.dropLeft 1 f)
                , editing = False
                , data = CodeRow Empty
                }

            else
                { input = f
                , editing = False
                , data = MarkdownRow
                }

        combine e ( l, a ) =
            case l of
                Nothing ->
                    ( Just e, a )

                Just last ->
                    case ( e.data, last.data ) of
                        ( MarkdownRow, MarkdownRow ) ->
                            ( Just
                                { input = e.input ++ "\n" ++ last.input
                                , editing = False
                                , data = MarkdownRow
                                }
                            , a
                            )

                        _ ->
                            ( Just e, last :: a )
    in
    row
        |> String.split "\n"
        |> List.map parseFragment
        |> List.foldr combine ( Nothing, [] )
        |> (\( l, a ) ->
                case l of
                    Nothing ->
                        a

                    Just e ->
                        e :: a
           )


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
    | OpenFile
    | SaveFile
    | SelectedFileForOpen File
    | ReadFile String String
    | CellMsg Int CellMsg
    | Language Language


type CellMsg
    = EndEditing
    | ToCode
    | ToMarkdown
    | StartEditing
    | Calculate
    | Input String
    | Save String
    | Copy String
