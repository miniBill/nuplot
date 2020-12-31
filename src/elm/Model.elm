module Model exposing (Document, Flags, Modal(..), Model, Msg(..), Output(..), Row, Size, documentsCodec, emptyRow)

import Codec exposing (Codec)
import Expression exposing (Expression)
import Json.Decode as JD
import Zipper exposing (Zipper)


type alias Flags =
    JD.Value


type alias Model =
    { documents : Maybe (Zipper Document)
    , modal : Maybe Modal
    , size : Size
    }


type Modal
    = ModalClose Int
    | ModalRename String


documentsCodec : Codec (Maybe (Zipper Document))
documentsCodec =
    -- We convert to a list to prevent failures to be read as a valid empty value
    Codec.list (Zipper.codec documentCodec)
        |> Codec.map List.head
            (\c ->
                case c of
                    Nothing ->
                        []

                    Just v ->
                        [ v ]
            )


type alias Document =
    { name : String
    , changed : Bool
    , rows : List Row
    }


documentCodec : Codec Document
documentCodec =
    Codec.object (\n rs -> { name = n, changed = False, rows = rs })
        |> Codec.field "name" .name Codec.string
        |> Codec.field "rows" .rows (Codec.list rowCodec)
        |> Codec.buildObject


type alias Size =
    { width : Int
    , height : Int
    }


type alias Row =
    { input : String
    , output : Output
    }


rowCodec : Codec Row
rowCodec =
    Codec.map (\i -> { input = i, output = Empty }) .input Codec.string


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
    | RenameDocument (Maybe String)
    | CloseDocument { ask : Bool, index : Int }
    | Resized Size
    | DismissModal
    | SetModal Modal
