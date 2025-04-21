module API.Google exposing (AccessToken(..), Error(..), FileId, Request(..), authenticationFlowUrl, fileIdCodec, fileIdFromMetadata, fileIdToMetadata, generateId, mapRequest, uploadFile)

import Bytes.Encode as BE
import Codec exposing (Codec)
import Http exposing (Resolver, Response(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Platform exposing (Task)
import Task
import Url.Builder as B



-- FileId


type FileId
    = FileId String


fileIdCodec : Codec FileId
fileIdCodec =
    Codec.map FileId (\(FileId i) -> i) Codec.string


fileIdFromMetadata : String -> Maybe FileId
fileIdFromMetadata metadata =
    if String.startsWith googleMetadataKey metadata then
        Just <| FileId <| String.dropLeft (String.length googleMetadataKey) metadata

    else
        Nothing


googleMetadataKey : String
googleMetadataKey =
    "GOOGLE_ID = "


fileIdToMetadata : FileId -> String
fileIdToMetadata (FileId fid) =
    googleMetadataKey ++ fid


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


authenticationFlowUrl : String -> String
authenticationFlowUrl redirectUri =
    B.crossOrigin "https://accounts.google.com"
        [ "o", "oauth2", "v2", "auth" ]
        [ B.string "scope" scope
        , B.string "response_type" "token"
        , B.string "client_id" clientId
        , B.string "redirect_uri" redirectUri
        ]


clientId : String
clientId =
    "696268500736-k646rm6l6qcucv2t1qufdg31ngsfgmhu.apps.googleusercontent.com"


uploadFile : { id : FileId, name : String, content : String, accessToken : String } -> Task Error ()
uploadFile params =
    let
        updateUrl =
            B.crossOrigin "https://www.googleapis.com" [ "upload", "drive", "v3", "files", id ] [ B.string "uploadType" "multipart" ]

        (FileId id) =
            params.id

        updateMetadataEncoder =
            BE.string <|
                JE.encode 0 <|
                    JE.object
                        [ ( "mimeType", JE.string "text/markdown" )
                        , ( "name", JE.string params.name )
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
    in
    updateTask
        |> Task.onError
            (\e ->
                if e == FileNotFound then
                    let
                        insertUrl =
                            B.crossOrigin "https://www.googleapis.com" [ "upload", "drive", "v3", "files" ] [ B.string "uploadType" "multipart" ]

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
                    in
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

                BadStatus_ { statusCode } body ->
                    case statusCode of
                        401 ->
                            Err Unauthorized

                        404 ->
                            Err FileNotFound

                        _ ->
                            Err (UnexpectedResponse <| "BadStatus " ++ String.fromInt statusCode ++ ", body: " ++ body)

                GoodStatus_ _ body ->
                    JD.decodeString decoder body
                        |> Result.mapError (\e -> UnexpectedResponse <| JD.errorToString e)
        )
