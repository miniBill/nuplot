module UI.Model exposing (Document, Modal(..), Model, Msg(..), documentFromFile, documentToFile, documentsCodec, emptyMetadata, emptyRow)

import Browser.Navigation exposing (Key)
import Codec exposing (Codec)
import File exposing (File)
import Google
import List.Extra as List
import List.MyExtra as List exposing (LeftOrRight(..))
import Model exposing (CellMsg, Context, DocumentId, Row, RowData(..), Size)
import String
import UI.L10N as L10N exposing (L10N, Language)
import Zipper exposing (Zipper)


type alias Model =
    { documents : Maybe (Zipper { id : DocumentId, document : Document })
    , nextId : Int
    , modal : Maybe Modal
    , size : Size
    , openMenu : Bool
    , showPendingActions : Bool
    , context : Context
    , rootUrl : String
    , google : Google.Model
    , key : Key
    }


type alias Document =
    { rows : List Row
    , metadata : Metadata
    }


type alias Metadata =
    { name : Maybe String
    , googleId : Maybe Google.FileId
    }


type alias MetadataWithErrors =
    { errors : List (L10N String)
    , metadata : Metadata
    }


type ParsedRow
    = NormalRow Row
    | MetadataRow MetadataWithErrors


type Msg
    = NewDocument
    | SelectDocument Int
    | RenameDocument (Maybe String)
    | CloseDocument { ask : Bool, index : Int }
    | Resized Size
    | DismissModal
    | SetModal Modal
    | ToggleMenu Bool
    | ToggleShowPendingActions Bool
    | OpenFile
    | ExpandIntervals Bool
    | SaveFile
    | SelectedFileForOpen File
    | ReadFile String String
    | CellMsg Int CellMsg
    | Language Language
    | GoogleAuth
    | GoogleSave
    | GotGoogleSaveResultFor DocumentId (Result Google.Error ())
    | GotGoogleFileIdFor DocumentId (Result Google.Error Google.FileId)
    | GotGoogleAccessToken String
    | Nop String
    | CalculateAll
    | ClearAll


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


documentCodec : Codec Document
documentCodec =
    Codec.object
        (\r n gid ->
            let
                metadata : Metadata
                metadata =
                    { name = n
                    , googleId = gid
                    }
            in
            { rows = r
            , metadata = metadata
            }
        )
        |> Codec.field "rows" .rows rowsCodec
        |> Codec.maybeField "name" (.metadata >> .name) Codec.string
        |> Codec.maybeField "googleId" (.metadata >> .googleId) Google.fileIdCodec
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


emptyRow : Row
emptyRow =
    { input = ""
    , editing = True
    , data = CodeRow []
    }


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


type Modal
    = ModalClose Int
    | ModalRename String
    | ModalGoogleAuth
