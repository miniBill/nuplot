module Google exposing (AccessToken(..), Error(..), FileId, Model, Request(..), fileIdCodec, fileIdToMetadata, generateId, mapRequest, metadataToFileId, redirectUrl, startAuthenticationFlow, uploadFile)

import Bytes.Encode as BE
import Codec exposing (Codec)
import Http exposing (Resolver, Response(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Model exposing (DocumentId)
import Platform exposing (Task)
import Task
import UI.Ports
import Url.Builder as B


type alias Model =
    { accessToken : AccessToken
    , waitingId : List { id : DocumentId, name : String, content : String, request : Request FileId }
    , waitingSave : List { id : DocumentId, googleId : FileId, name : String, content : String, request : Request () }
    , errors : List Error
    }


type Request a
    = WaitingAccessToken
    | Running
    | Errored Error
    | Succeded a


mapRequest : (a -> b) -> Request a -> Request b
mapRequest f req =
    case req of
        WaitingAccessToken ->
            WaitingAccessToken

        Running ->
            Running

        Errored e ->
            Errored e

        Succeded x ->
            Succeded (f x)


type AccessToken
    = Missing
    | Present String
    | Expired


scope : String
scope =
    "https://www.googleapis.com/auth/drive.file"


startAuthenticationFlow : String -> Cmd msg
startAuthenticationFlow =
    UI.Ports.openWindow << buildUrl


buildUrl : String -> String
buildUrl root =
    B.crossOrigin "https://accounts.google.com"
        [ "o", "oauth2", "v2", "auth" ]
        [ B.string "scope" scope
        , B.string "response_type" "token"
        , B.string "client_id" clientId
        , B.string "redirect_uri" root
        ]


clientId : String
clientId =
    "696268500736-k646rm6l6qcucv2t1qufdg31ngsfgmhu.apps.googleusercontent.com"


redirectUrl : String
redirectUrl =
    "https://nuplot.netlify.app"


type FileId
    = FileId String


fileIdCodec : Codec FileId
fileIdCodec =
    Codec.map FileId (\(FileId f) -> f) Codec.string


metadataKey : String
metadataKey =
    "GOOGLE_ID = "


fileIdToMetadata : FileId -> String
fileIdToMetadata (FileId fid) =
    Model.metadataMarker ++ metadataKey ++ fid ++ ")"


metadataToFileId : String -> Maybe FileId
metadataToFileId metadata =
    if String.startsWith metadataKey metadata then
        Just <| FileId <| String.dropLeft (String.length metadataKey) metadata

    else
        Nothing


uploadFile : { id : FileId, name : String, content : String, accessToken : String } -> Task Error ()
uploadFile params =
    let
        updateUrl =
            B.crossOrigin "https://www.googleapis.com" [ "upload", "drive", "v3", "files", id ] [ B.string "uploadType" "multipart" ]

        insertUrl =
            B.crossOrigin "https://www.googleapis.com" [ "upload", "drive", "v3", "files" ] [ B.string "uploadType" "multipart" ]

        (FileId id) =
            params.id

        updateMetadataEncoder =
            BE.string <|
                JE.encode 0 <|
                    JE.object
                        [ ( "mimeType", JE.string "text/markdown" )
                        , ( "name", JE.string params.name )
                        ]

        insertMetadataEncoder =
            BE.string <|
                JE.encode 0 <|
                    JE.object
                        [ ( "id", JE.string id )
                        , ( "mimeType", JE.string "text/markdown" )
                        , ( "name", JE.string params.name )

                        -- TODO: parents are a list of FileIDs, so it's Very Annoying and will be implemented later
                        --, ( "parents", JE.list JE.string [ "nuPlot" ] )
                        ]

        updateTask =
            Http.task
                { method = "PATCH"
                , url = updateUrl
                , headers = [ Http.header "Authorization" <| "Bearer " ++ params.accessToken ]
                , resolver = jsonResolver <| JD.succeed ()
                , body =
                    Http.multipartBody
                        [ Http.bytesPart "metadata" "application/json; charset=UTF-8" <| BE.encode updateMetadataEncoder
                        , Http.stringPart "content" params.content
                        ]
                , timeout = Nothing
                }

        insertTask =
            Http.task
                { method = "POST"
                , url = insertUrl
                , headers = [ Http.header "Authorization" <| "Bearer " ++ params.accessToken ]
                , resolver = jsonResolver <| JD.succeed ()
                , body =
                    Http.multipartBody
                        [ Http.bytesPart "metadata" "application/json; charset=UTF-8" <| BE.encode insertMetadataEncoder
                        , Http.stringPart "content" params.content
                        ]
                , timeout = Nothing
                }
    in
    updateTask
        |> Task.onError
            (\e ->
                if e == FileNotFound then
                    insertTask

                else
                    Task.fail e
            )


type Error
    = UnexpectedResponse String
    | Unauthorized
    | CodeBug
    | NetworkError
    | FileNotFound


generateId : { accessToken : String } -> Task Error FileId
generateId { accessToken } =
    generateIds { count = 1, accessToken = accessToken }
        |> Task.andThen
            (\l ->
                case List.head l of
                    Just h ->
                        Task.succeed h

                    Nothing ->
                        Task.fail <| UnexpectedResponse "Empty list when retrieving a new ID"
            )


generateIds : { count : Int, accessToken : String } -> Task Error (List FileId)
generateIds { count, accessToken } =
    let
        url =
            B.crossOrigin "https://www.googleapis.com" [ "drive", "v3", "files", "generateIds" ] [ B.int "count" count ]
    in
    Http.task
        { method = "GET"
        , url = url
        , headers = [ Http.header "Authorization" <| "Bearer " ++ accessToken ]
        , resolver = jsonResolver (JD.field "ids" <| JD.list <| JD.map FileId JD.string)
        , body = Http.emptyBody
        , timeout = Nothing
        }


jsonResolver : Decoder a -> Resolver Error a
jsonResolver decoder =
    Http.stringResolver
        (\response ->
            case response of
                BadUrl_ _ ->
                    Err CodeBug

                Timeout_ ->
                    Err NetworkError

                NetworkError_ ->
                    Err NetworkError

                BadStatus_ { statusCode } _ ->
                    case statusCode of
                        401 ->
                            Err Unauthorized

                        404 ->
                            Err FileNotFound

                        _ ->
                            Err (UnexpectedResponse <| Debug.toString response)

                GoodStatus_ _ body ->
                    JD.decodeString decoder body
                        |> Result.mapError (\e -> UnexpectedResponse <| Debug.toString e)
        )
