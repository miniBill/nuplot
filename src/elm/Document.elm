module Document exposing (Id(..), Modal(..), Output(..), Row, RowData(..), StoredDocument, UIDocument, codec, emptyRow, fromFile, toFile, toStored, toUI)

import API.Google as Google
import Codec exposing (Codec)
import Expression exposing (Expression)
import List.Extra as List
import List.MyExtra as List exposing (LeftOrRight(..))
import Maybe.MyExtra as Maybe
import UI.L10N exposing (L10N)



-----------
-- TYPES --
-----------


type alias UIDocument =
    { id : Id
    , name : Maybe String
    , rows : List Row
    , googleId : Maybe Google.FileId
    , modals : List Modal
    }


type alias StoredDocument =
    { name : Maybe String
    , rows : List Row
    , googleId : Maybe Google.FileId
    }


type Id
    = Id Int


type alias Row =
    { input : String
    , editing : Bool
    , data : RowData
    }


type RowData
    = CodeRow (List Output)
    | MarkdownRow


type Output
    = ParseError String
    | Parsed Expression


type Modal
    = ModalClose
    | ModalRename String



--------------------
-- TO / FROM FILE --
--------------------


toStored : UIDocument -> StoredDocument
toStored { name, rows, googleId } =
    { name = name
    , rows = rows
    , googleId = googleId
    }


toUI : Id -> StoredDocument -> UIDocument
toUI id { name, rows, googleId } =
    { id = id
    , name = name
    , rows = rows
    , googleId = googleId
    , modals = []
    }


toFile : { a | rows : List Row, googleId : Maybe Google.FileId } -> String
toFile { rows, googleId } =
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
        |> (\l ->
                case googleId of
                    Nothing ->
                        l

                    Just gid ->
                        l ++ [ metadataMarker ++ Google.fileIdToMetadata gid ++ ")" ]
           )
        |> String.join "\n\n"


metadataMarker : String
metadataMarker =
    "[//]: # (NUPLOT INTERNAL DATA -- "


fromFile :
    Maybe String
    -> String
    ->
        { errors : List (L10N String)
        , document : StoredDocument
        }
fromFile name file =
    file
        |> String.split "\n\n"
        |> List.map parseRow
        |> List.foldr (\( m, r ) ( ma, ra ) -> ( m ++ ma, r ++ ra )) ( [], [] )
        |> Tuple.mapSecond (List.filterNot (.input >> String.isEmpty))
        |> (\( metadataWithErrorsList, rows ) ->
                let
                    { errors, googleId } =
                        List.foldr combineMetadataWithErrors
                            { errors = []
                            , googleId = Nothing
                            }
                            metadataWithErrorsList
                in
                { errors = errors
                , document =
                    { rows = rows ++ [ emptyRow ]
                    , name = Maybe.withDefaultMaybe name name
                    , googleId = googleId
                    }
                }
           )


emptyRow : Row
emptyRow =
    { input = ""
    , editing = True
    , data = CodeRow []
    }


parseRow : String -> ( List MetadataWithErrors, List Row )
parseRow row =
    let
        combine curr last =
            case ( curr.data, last.data ) of
                ( MarkdownRow, MarkdownRow ) ->
                    Just
                        { input = curr.input ++ "\n" ++ last.input
                        , editing = False
                        , data = MarkdownRow
                        }

                ( CodeRow _, CodeRow _ ) ->
                    Just
                        { input = curr.input ++ "\n" ++ last.input
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


parseFragment : String -> ParsedRow
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
        case Google.fileIdFromMetadata extracted of
            Just gid ->
                MetadataRow
                    { errors = []
                    , googleId = Just gid
                    }

            Nothing ->
                MetadataRow
                    { errors = [ { en = "Unrecognized metadata: " ++ extracted, it = "Metadati non riconosciuti: " ++ extracted } ]
                    , googleId = Nothing
                    }

    else
        NormalRow
            { input = f
            , editing = False
            , data = MarkdownRow
            }


type ParsedRow
    = NormalRow Row
    | MetadataRow MetadataWithErrors


combineMetadataWithErrors : MetadataWithErrors -> MetadataWithErrors -> MetadataWithErrors
combineMetadataWithErrors l r =
    { errors = l.errors ++ r.errors
    , googleId = l.googleId |> Maybe.withDefaultMaybe r.googleId
    }


type alias MetadataWithErrors =
    { errors : List (L10N String)
    , googleId : Maybe Google.FileId
    }



------------
-- CODECS --
------------


codec : Codec StoredDocument
codec =
    let
        ctor : List Row -> Maybe String -> Maybe Google.FileId -> StoredDocument
        ctor r n gid =
            { rows = r
            , name = n
            , googleId = gid
            }
    in
    Codec.object
        ctor
        |> Codec.field "rows" .rows rowsCodec
        |> Codec.maybeField "name" .name Codec.string
        |> Codec.maybeField "googleId" .googleId Google.fileIdCodec
        |> Codec.buildObject


rowsCodec : Codec (List Row)
rowsCodec =
    let
        v0Codec =
            Codec.list rowCodec

        v1Codec =
            Codec.map
                (fromFile Nothing >> .document >> .rows)
                (\r -> toFile { rows = r, name = Nothing, googleId = Nothing })
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
