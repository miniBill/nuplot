module UI.Model exposing (CanvasId(..), CellMsg(..), Context, DocumentMsg(..), Flags, Google, Model, Msg(..), documentsCodec)

import API.Google as Google
import Browser.Navigation exposing (Key)
import Codec exposing (Codec)
import Document as Document exposing (RowData(..), StoredDocument, UIDocument)
import Element.WithContext exposing (DeviceClass)
import File exposing (File)
import Json.Decode as JD
import List.Extra as List
import List.MyExtra as List exposing (LeftOrRight(..))
import Maybe.MyExtra as Maybe
import UI.L10N exposing (Language)
import Zipper exposing (Zipper)



-- FLAGS


type alias Flags =
    { saved : JD.Value
    , hasClipboard : Bool
    , languages : List String
    , rootUrl : String
    , googleAccessToken : String
    }



-- MODEL


type alias Model =
    { documents : Maybe (Zipper UIDocument)
    , nextId : Int
    , size : Size
    , openMenu : Bool
    , showPendingActions : Bool
    , context : Context
    , rootUrl : String
    , google : Google
    , key : Key
    }


type alias Size =
    { width : Int
    , height : Int
    }


type alias Context =
    { language : Language
    , hasClipboard : Bool
    , expandIntervals : Bool
    , rayDifferentials : Bool
    , deviceClass : DeviceClass
    }


type alias Google =
    { accessToken : Google.AccessToken
    , waitingId : List { id : Document.Id, name : String, content : String, request : Google.Request Google.FileId }
    , waitingSave : List { id : Document.Id, googleId : Google.FileId, name : String, content : String, request : Google.Request () }
    , errors : List Google.Error
    }



-- MSG


type Msg
    = DocumentNew
    | DocumentOpen
    | DocumentOpenSelected File
    | DocumentOpenContent { name : String, content : String }
    | DocumentMsg Document.Id DocumentMsg
    | Resized Size
    | ToggleMenu Bool
    | ToggleShowPendingActions Bool
    | ToggleExpandIntervals Bool
    | ToggleRayDifferentials Bool
    | Language Language
    | GoogleAuth
    | GoogleGotAccessToken String
    | Nop String


type DocumentMsg
    = DocumentSelect
    | DocumentRename String
    | DocumentClose { ask : Bool }
    | DocumentDownload
    | DocumentGoogleSave
    | DocumentGoogleSaveResult (Result Google.Error ())
    | DocumentGoogleId (Result Google.Error Google.FileId)
    | DocumentCellMsg { index : Int, msg : CellMsg }
    | DocumentPushModal Document.Modal
    | DocumentReplaceModal Document.Modal
    | DocumentPopModal
    | DocumentCalculateAll
    | DocumentClearAll


type CellMsg
    = EndEditing
    | ToCode
    | ToMarkdown
    | Clear
    | StartEditing
    | Calculate
    | Input String
    | SaveCanvas CanvasId
    | FullscreenCanvas CanvasId
    | CopyCanvas CanvasId


type CanvasId
    = CanvasId String



-- CODEC


documentsCodec : Codec (Maybe (Zipper StoredDocument))
documentsCodec =
    -- We convert to a list to prevent failures to be read as a valid empty value
    Codec.list (Zipper.codec Document.codec)
        |> Codec.map List.head
            (\c ->
                case c of
                    Nothing ->
                        []

                    Just v ->
                        [ v ]
            )
