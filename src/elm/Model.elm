module Model exposing (Flags, Model, Msg(..), Output(..), Row, emptyRow)

import Expression exposing (Expression)
import Json.Decode as JD


type alias Flags =
    JD.Value


type alias Model =
    { rows : List Row
    , pageWidth : Int
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
    | Typing (Maybe Expression)
    | ParseError String
    | Parsed Expression


type Msg
    = Input Int String
    | Width Int
    | Calculate Int
