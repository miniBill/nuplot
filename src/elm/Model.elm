module Model exposing (ColoredLines, Flags, GraphStatus(..), Line, Model, Msg(..), PlotResult, Row)


type GraphStatus
    = Drawn (List ColoredLines)
    | Drawing


type alias PlotResult =
    { expression : String
    , lines : List ColoredLines
    }


type alias ColoredLines =
    { color : String
    , lines : List Line
    }


type alias Line =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }


type alias Flags =
    {}


type alias Model =
    List Row


type alias Row =
    { input : String
    , graph : Maybe GraphStatus
    }


type Msg
    = Input
        { row : Int
        , value : String
        }
    | Plotted PlotResult
