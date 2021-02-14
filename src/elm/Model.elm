module Model exposing (CellMsg(..), Context, Document, Flags, Metadata, Modal(..), Model, Msg(..), Output(..), Row, RowData(..), Size, documentFromFile, documentToFile, documentsCodec, emptyMetadata, emptyRow)

import Browser.Navigation exposing (Key)
import Codec exposing (Codec)
import Element.WithContext exposing (DeviceClass)
import Expression exposing (Expression)
import File exposing (File)
import Json.Decode as JD
import List.Extra as List
import List.MyExtra as List exposing (LeftOrRight(..))
import String
import UI.L10N as L10N exposing (L10N, Language)
import Zipper exposing (Zipper)


type alias Flags =
    { saved : JD.Value
    , hasClipboard : Bool
    , languages : List String
    , rootUrl : String
    }


type alias Model =
    { documents : Maybe (Zipper Document)
    , modal : Maybe Modal
    , size : Size
    , openMenu : Bool
    , context : Context
    , rootUrl : String
    , accessToken : Maybe String
    , key : Key
    }


type alias Context =
    { language : Language
    , hasClipboard : Bool
    , expandIntervals : Bool
    , deviceClass : DeviceClass
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
    { rows : List Row
    , metadata : Metadata
    }


type alias Metadata =
    { name : Maybe String
    , googleId : Maybe String
    }


documentCodec : Codec Document
documentCodec =
    Codec.object
        (\r n gid ->
            { rows = r
            , metadata =
                { name = n
                , googleId = gid
                }
            }
        )
        |> Codec.field "rows" .rows rowsCodec
        |> Codec.maybeField "name" (.metadata >> .name) Codec.string
        |> Codec.maybeField "googleId" (.metadata >> .googleId) Codec.string
        |> Codec.buildObject


rowsCodec : Codec (List Row)
rowsCodec =
    let
        v0Codec =
            Codec.list rowCodec

        v1Codec =
            Codec.map
                (documentFromFile Nothing >> .document >> .rows)
                (\r -> documentToFile { rows = r })
                Codec.string
    in
    Codec.oneOf v1Codec [ v0Codec ]


documentToFile : { a | rows : List Row } -> String
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
    rows
        |> List.filterNot (.input >> String.isEmpty)
        |> List.map rowToString
        |> String.join "\n\n"


documentFromFile :
    Maybe String
    -> String
    ->
        { errors : List (L10N String)
        , document : Document
        }
documentFromFile name file =
    file
        |> String.split "\n\n"
        |> List.map parseRow
        |> List.foldr (\( m, r ) ( ma, ra ) -> ( m ++ ma, r ++ ra )) ( [], [] )
        |> Tuple.mapSecond (List.filterNot (.input >> String.isEmpty))
        |> (\( metadataWithErrorsList, rows ) ->
                let
                    { errors, metadata } =
                        List.foldr combineMetadataWithErrors { errors = [], metadata = emptyMetadata } metadataWithErrorsList
                in
                { errors = errors
                , document =
                    { rows = rows ++ [ emptyRow ]
                    , metadata =
                        { name =
                            metadata.name
                                |> withDefaultMaybe name
                        , googleId = metadata.googleId
                        }
                    }
                }
           )


withDefaultMaybe : Maybe a -> Maybe a -> Maybe a
withDefaultMaybe r l =
    case l of
        Just _ ->
            l

        Nothing ->
            r


combineMetadataWithErrors : MetadataWithErrors -> MetadataWithErrors -> MetadataWithErrors
combineMetadataWithErrors l r =
    { errors = l.errors ++ r.errors
    , metadata =
        { name = l.metadata.name |> withDefaultMaybe r.metadata.name
        , googleId = l.metadata.googleId |> withDefaultMaybe r.metadata.googleId
        }
    }


emptyMetadata : Metadata
emptyMetadata =
    { name = Nothing
    , googleId = Nothing
    }


type ParsedRow
    = NormalRow Row
    | MetadataRow MetadataWithErrors


type alias MetadataWithErrors =
    { errors : List (L10N String)
    , metadata : Metadata
    }


parseRow : String -> ( List MetadataWithErrors, List Row )
parseRow row =
    let
        metadataMarker =
            "[//]: # (NUPLOT INTERNAL DATA -- "

        parseFragment f =
            if String.startsWith ">" f then
                NormalRow
                    { input = String.trim (String.dropLeft 1 f)
                    , editing = False
                    , data = CodeRow []
                    }

            else if String.startsWith metadataMarker f then
                let
                    extracted =
                        String.dropRight 1 <| String.dropLeft (String.length metadataMarker) f
                in
                MetadataRow
                    { errors = [ L10N.invariant <| "TODO:" ++ extracted ]
                    , metadata =
                        { name = Nothing
                        , googleId = Nothing
                        }
                    }

            else
                NormalRow
                    { input = f
                    , editing = False
                    , data = MarkdownRow
                    }

        combine e last =
            case ( e.data, last.data ) of
                ( MarkdownRow, MarkdownRow ) ->
                    Just
                        { input = e.input ++ "\n" ++ last.input
                        , editing = False
                        , data = MarkdownRow
                        }

                ( CodeRow _, CodeRow _ ) ->
                    Just
                        { input = e.input ++ "\n" ++ last.input
                        , editing = False
                        , data = CodeRow []
                        }

                _ ->
                    Nothing
    in
    row
        |> String.split "\n"
        |> List.map parseFragment
        |> List.categorize
            (\f ->
                case f of
                    MetadataRow m ->
                        Left m

                    NormalRow n ->
                        Right n
            )
        |> Tuple.mapSecond (List.groupOneWith combine)


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
    = CodeRow (List Output)
    | MarkdownRow


rowCodec : Codec Row
rowCodec =
    Codec.map
        (\i ->
            if String.startsWith ">" i then
                { input = String.trim <| String.dropLeft 1 i
                , editing = False
                , data = CodeRow []
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
    , data = CodeRow []
    }


type Output
    = ParseError String
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
    | ExpandIntervals Bool
    | SaveFile
    | SelectedFileForOpen File
    | ReadFile String String
    | CellMsg Int CellMsg
    | Language Language
    | GoogleSave
    | Nop String
    | CalculateAll
    | ClearAll


type CellMsg
    = EndEditing
    | ToCode
    | ToMarkdown
    | Clear
    | StartEditing
    | Calculate
    | Input String
    | Save String
    | Copy String
