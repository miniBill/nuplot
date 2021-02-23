module Model exposing
    ( CellMsg(..)
    , Context
    , DocumentId(..)
    , Flags
    , Output(..)
    , Row
    , RowData(..)
    , Size
    , metadataMarker
    )

import Element.WithContext exposing (DeviceClass)
import Expression exposing (Expression)
import Json.Decode as JD
import List.Extra as List
import List.MyExtra as List exposing (LeftOrRight(..))
import UI.L10N exposing (Language)


type alias Flags =
    { saved : JD.Value
    , hasClipboard : Bool
    , languages : List String
    , rootUrl : String
    , googleAccessToken : String
    }


type DocumentId
    = DocumentId Int


type alias Context =
    { language : Language
    , hasClipboard : Bool
    , expandIntervals : Bool
    , deviceClass : DeviceClass
    }


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


type Output
    = ParseError String
    | Parsed Expression


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


metadataMarker : String
metadataMarker =
    "[//]: # (NUPLOT INTERNAL DATA -- "
