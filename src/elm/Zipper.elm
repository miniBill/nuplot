module Zipper exposing
    ( Zipper
    , allRight
    , append
    , codec
    , fromList
    , get
    , left
    , map
    , removeAt
    , right
    , selected
    , setSelected
    , singleton
    )

import Codec exposing (Codec)


type Zipper a
    = Zipper
        { before : List a
        , curr : a
        , after : List a
        }


singleton : a -> Zipper a
singleton x =
    Zipper
        { before = []
        , curr = x
        , after = []
        }


selected : Zipper a -> a
selected (Zipper { curr }) =
    curr


setSelected : a -> Zipper a -> Zipper a
setSelected x (Zipper z) =
    Zipper { z | curr = x }


get : Int -> Zipper a -> Maybe a
get i (Zipper { before, curr, after }) =
    if i == 0 then
        Just curr

    else if i < 0 then
        List.head (List.drop (-i - 1) before)

    else
        List.head (List.drop (i - 1) after)


left : Int -> Zipper a -> Zipper a
left i ((Zipper { before, curr, after }) as orig) =
    if i == 0 then
        orig

    else if i < 0 then
        right -i orig

    else
        case before of
            [] ->
                orig

            h :: t ->
                left (i - 1) (Zipper { before = t, curr = h, after = curr :: after })


right : Int -> Zipper a -> Zipper a
right i ((Zipper { before, curr, after }) as orig) =
    if i == 0 then
        orig

    else if i < 0 then
        left -i orig

    else
        case after of
            [] ->
                orig

            h :: t ->
                right (i - 1) (Zipper { before = curr :: before, curr = h, after = t })


allRight : Zipper a -> Zipper a
allRight ((Zipper { before, curr, after }) as orig) =
    case List.reverse after of
        [] ->
            orig

        end :: tail ->
            Zipper { before = tail ++ curr :: before, curr = end, after = [] }


map : (Bool -> Int -> a -> b) -> Zipper a -> List b
map f (Zipper { before, curr, after }) =
    List.reverse (List.indexedMap (\i -> f False (-i - 1)) before)
        ++ f True 0 curr
        :: List.indexedMap (\i -> f False (i + 1)) after


append : a -> Zipper a -> Zipper a
append x (Zipper z) =
    Zipper { z | after = z.after ++ [ x ] }


removeAt : Int -> Zipper a -> Maybe (Zipper a)
removeAt i (Zipper z) =
    if i < 0 then
        Just <| Zipper { z | before = List.take (-i - 1) z.before ++ List.drop -i z.before }

    else if i > 0 then
        Just <| Zipper { z | after = List.take (i - 1) z.after ++ List.drop i z.after }

    else
        case ( z.before, z.after ) of
            ( [], [] ) ->
                Nothing

            ( _, h :: t ) ->
                Just <| Zipper { z | curr = h, after = t }

            ( h :: t, _ ) ->
                Just <| Zipper { z | curr = h, before = t }


fromList : List a -> Maybe (Zipper a)
fromList ls =
    case ls of
        [] ->
            Nothing

        h :: t ->
            Just <| Zipper { before = [], curr = h, after = t }


codec : Codec a -> Codec (Zipper a)
codec inner =
    Codec.object (\b c a -> { before = b, curr = c, after = a })
        |> Codec.field "before" .before (Codec.list inner)
        |> Codec.field "curr" .curr inner
        |> Codec.field "after" .after (Codec.list inner)
        |> Codec.buildObject
        |> Codec.map Zipper (\(Zipper z) -> z)
