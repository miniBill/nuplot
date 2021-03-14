module Zipper exposing
    ( Zipper
    , allRight
    , append
    , canGoLeft
    , canGoRight
    , codec
    , filter
    , find
    , focusFind
    , fromList
    , fromNonemptyList
    , get
    , getLeft
    , getRight
    , left
    , map
    , removeAt
    , right
    , selected
    , setSelected
    , singleton
    , toList
    )

import Codec exposing (Codec)
import List.Extra as List



-- Types and constructors


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



-- Information


getLeft : Zipper a -> List a
getLeft (Zipper { before }) =
    List.reverse before


getRight : Zipper a -> List a
getRight (Zipper { after }) =
    after


canGoLeft : Zipper a -> Bool
canGoLeft (Zipper { before }) =
    not <| List.isEmpty before


canGoRight : Zipper a -> Bool
canGoRight (Zipper { after }) =
    not <| List.isEmpty after


selected : Zipper a -> a
selected (Zipper { curr }) =
    curr


get : Int -> Zipper a -> Maybe a
get i (Zipper { before, curr, after }) =
    if i == 0 then
        Just curr

    else if i < 0 then
        List.head (List.drop (-i - 1) before)

    else
        List.head (List.drop (i - 1) after)


find : (a -> Bool) -> Zipper a -> Maybe a
find f zip =
    List.find f (toList zip)



-- Rotations


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


focusFind : (a -> Bool) -> Zipper a -> Zipper a
focusFind cond ((Zipper { before, curr, after }) as orig) =
    case List.findIndex cond (List.reverse before) of
        Just i ->
            left (List.length before - i) orig

        Nothing ->
            if cond curr then
                orig

            else
                case List.findIndex cond after of
                    Nothing ->
                        orig

                    Just i ->
                        right (i + 1) orig



-- Map and filter


map : (Int -> a -> b) -> Zipper a -> Zipper b
map f (Zipper { before, curr, after }) =
    Zipper
        { before = List.indexedMap (\i -> f (-i - 1)) before
        , curr = f 0 curr
        , after = List.indexedMap (\i -> f (i + 1)) after
        }


filter : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
filter f (Zipper { before, curr, after }) =
    let
        before_ =
            List.filter f before

        after_ =
            List.filter f after
    in
    if f curr then
        Just <|
            Zipper
                { before = before_
                , curr = curr
                , after = after_
                }

    else
        case ( before_, after_ ) of
            ( [], [] ) ->
                Nothing

            ( _, a :: atail ) ->
                Just <|
                    Zipper
                        { before = before_
                        , curr = a
                        , after = atail
                        }

            ( b :: btail, [] ) ->
                Just <|
                    Zipper
                        { before = btail
                        , curr = b
                        , after = []
                        }



-- Modification


setSelected : a -> Zipper a -> Zipper a
setSelected x (Zipper z) =
    Zipper { z | curr = x }


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



-- Conversion


fromList : List a -> Maybe (Zipper a)
fromList ls =
    case ls of
        [] ->
            Nothing

        h :: t ->
            Just <| fromNonemptyList h t


toList : Zipper a -> List a
toList (Zipper { before, curr, after }) =
    List.reverse before ++ curr :: after


fromNonemptyList : a -> List a -> Zipper a
fromNonemptyList h t =
    Zipper { before = [], curr = h, after = t }


codec : Codec a -> Codec (Zipper a)
codec inner =
    Codec.object (\b c a -> { before = b, curr = c, after = a })
        |> Codec.field "before" .before (Codec.list inner)
        |> Codec.field "curr" .curr inner
        |> Codec.field "after" .after (Codec.list inner)
        |> Codec.buildObject
        |> Codec.map Zipper (\(Zipper z) -> z)
