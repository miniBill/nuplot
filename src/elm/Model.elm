module Model exposing (Flags, Model, Msg(..), Output(..), Row, emptyRow)

import Expression exposing (Expression)
import Json.Decode as JD


type alias Flags =
    JD.Value


type alias Model =
    { rows : List Row
    , size : Size
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
    | Resized Size
