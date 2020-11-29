module Model exposing (Flags, Model, Msg(..), Row, RowResult(..), emptyRow)

import Bounce exposing (Bounce)


type alias Flags =
    {}


type alias Model =
    List Row


type alias Row =
    { input : String
    , result : RowResult
    , plotting : String
    , bounce : Bounce
    }


emptyRow : Row
emptyRow =
    { input = ""
    , result = Empty
    , plotting = ""
    , bounce = Bounce.init
    }


type RowResult
    = Empty
    | Typing
    | Plotted
    | ParseError String


type Msg
    = Input { row : Int, input : String }
    | BounceMsg Int
