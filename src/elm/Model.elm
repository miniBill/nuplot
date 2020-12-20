module Model exposing (Document, Flags, Model, Msg(..), Output(..), Row, emptyRow)

import Expression exposing (Expression)
import Json.Decode as JD
import Zipper exposing (Zipper)


type alias Flags =
    JD.Value


type alias Model =
    { documents : Maybe (Zipper Document)
    , size : Size
    }


type alias Document =
    { name : String
    , changed : Bool
    , rows : List Row
    }


type alias Size =
    { width : Int
    , height : Int
    }


type alias Row =
    { input : String
    , output : Output
    }


emptyRow : Row
emptyRow =
    { input = ""
    , output = Empty
    }


type Output
    = Empty
    | ParseError String
    | Parsed Expression


type Msg
    = Input Int String
    | Calculate Int
    | NewDocument
    | SelectDocument Int
    | CloseDocument Int
    | Resized Size
