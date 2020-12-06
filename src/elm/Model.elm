module Model exposing (Flags, Model, Msg(..), Output(..), Row, emptyRow)

import Bounce exposing (Bounce)
import Expression exposing (Expression)


type alias Flags =
    {}


type alias Model =
    List Row


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
    | BounceMsg Int
