module Google exposing (Error, generateId, redirectUrl, startAuthenticationFlow, uploadFile)

import Bytes.Encode as BE
import Http exposing (Resolver, Response(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Platform exposing (Task)
import Task
import UI.Ports
import Url.Builder as B


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


uploadFile : { id : FileId, name : String, content : String, googleAccessToken : String } -> Task Error ()
uploadFile params =
    let
        url =
            B.crossOrigin "https://www.googleapis.com" [ "upload", "drive", "v3", "files" ] [ B.string "uploadType" "multipart" ]

        (FileId id) =
            params.id

        metadataEncoder =
            BE.string <|
                JE.encode 0 <|
                    JE.object
                        [ ( "id", JE.string id )
                        , ( "mimeType", JE.string "text/markdown" )
                        , ( "name", JE.string params.name )
                        , ( "parents", JE.list JE.string [ "nuPlot" ] )
                        ]
    in
    Http.task
        { method = "POST"
        , url = url
        , headers = [ Http.header "Authorization" <| "Bearer " ++ params.googleAccessToken ]
        , resolver = jsonResolver <| JD.succeed ()
        , body =
            Http.multipartBody
                [ Http.bytesPart "metadata" "application/json; charset=UTF-8" <| BE.encode metadataEncoder
                , Http.stringPart "content" params.content
                ]
        , timeout = Nothing
        }


type Error
    = UnexpectedResponse String
    | CodeBug
    | NetworkError


generateId : { googleAccessToken : String } -> Task Error FileId
generateId { googleAccessToken } =
    generateIds { count = 1, googleAccessToken = googleAccessToken }
        |> Task.andThen
            (\l ->
                case List.head l of
                    Just h ->
                        Task.succeed h

                    Nothing ->
                        Task.fail <| UnexpectedResponse "Empty list when retrieving a new ID"
            )


generateIds : { count : Int, googleAccessToken : String } -> Task Error (List FileId)
generateIds { count, googleAccessToken } =
    let
        url =
            B.crossOrigin "https://www.googleapis.com" [ "drive", "v3", "files", "generateIds" ] [ B.int "count" count ]
    in
    Http.task
        { method = "GET"
        , url = url
        , headers = [ Http.header "Authorization" <| "Bearer " ++ googleAccessToken ]
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

                BadStatus_ _ _ ->
                    Err (UnexpectedResponse <| Debug.toString response)

                GoodStatus_ _ body ->
                    JD.decodeString decoder body
                        |> Result.mapError (\e -> UnexpectedResponse <| Debug.toString e)
        )
