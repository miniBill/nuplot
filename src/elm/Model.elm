module Model exposing (Flags, Model, Msg(..), PlotStatus(..), Row, emptyRow)

import Bounce exposing (Bounce)
import Expression exposing (Expression)


type alias Flags =
    {}


type alias Model =
    List Row


type alias Row =
    { input : String
    , plotStatus : PlotStatus
    , bounce : Bounce
    }


emptyRow : Row
emptyRow =
    { input = ""
    , plotStatus = Empty
    , bounce = Bounce.init
    }


type PlotStatus
    = Empty
    | Typing (Maybe Expression)
    | Plotting Expression
    | ParseError String


type Msg
    = Input { row : Int, input : String }
    | BounceMsg Int
