module Model exposing (Flags, Model, Msg(..), Output(..), Row, emptyRow)

import Bounce exposing (Bounce)
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
    , bounce : Bounce
    }


emptyRow : Row
emptyRow =
    { input = ""
    , output = Empty
    , bounce = Bounce.init
    }


type Output
    = Empty
    | Typing (Maybe Expression)
    | ParseError String
    | Parsed Expression


type Msg
    = Input { row : Int, input : String }
    | Width Int
    | BounceMsg Int
