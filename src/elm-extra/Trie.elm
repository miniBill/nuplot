module Trie exposing (Trie, empty, fromList, get, getLongestPrefix, insert, union)

import Dict exposing (Dict)
import Maybe.MyExtra


type Trie a
    = Trie
        { value : Maybe a
        , children : Dict Char (Trie a)
        }


fromList : List ( String, a ) -> Trie a
fromList =
    List.foldl (\( k, v ) -> insert k v) empty


get : String -> Trie a -> Maybe a
get key =
    let
        go : List Char -> Trie b -> Maybe b
        go k (Trie { value, children }) =
            case k of
                [] ->
                    value

                h :: t ->
                    Dict.get h children
                        |> Maybe.andThen (go t)
    in
    go (String.toList key)


insert : String -> a -> Trie a -> Trie a
insert s v =
    let
        go : List Char -> Trie a -> Trie a
        go ss ((Trie trie) as orig) =
            case ss of
                [] ->
                    if trie.value == Nothing then
                        Trie { trie | value = Just v }

                    else
                        orig

                c :: cs ->
                    let
                        node =
                            Dict.get c trie.children
                                |> Maybe.withDefault empty
                                |> go cs
                    in
                    Trie { trie | children = Dict.insert c node trie.children }
    in
    go (String.toList s)


empty : Trie a
empty =
    Trie
        { value = Nothing
        , children = Dict.empty
        }


union : Trie a -> Trie a -> Trie a
union (Trie l) (Trie r) =
    Trie
        { value =
            l.value
                |> Maybe.MyExtra.withDefaultMaybe r.value
        , children =
            Dict.merge Dict.insert
                (\k lv rv -> Dict.insert k (union lv rv))
                Dict.insert
                l.children
                r.children
                Dict.empty
        }


getLongestPrefix : String -> Trie a -> Maybe ( String, a )
getLongestPrefix s =
    let
        tryClose : List Char -> Maybe a -> Maybe ( String, a )
        tryClose acc value =
            Maybe.map (\v -> ( String.fromList <| List.reverse acc, v )) value

        go : List Char -> List Char -> Trie a -> Maybe ( String, a )
        go cs acc (Trie { value, children }) =
            case cs of
                [] ->
                    tryClose acc value

                h :: t ->
                    case Dict.get h children of
                        Nothing ->
                            tryClose acc value

                        Just child ->
                            case go t (h :: acc) child of
                                Just r ->
                                    Just r

                                Nothing ->
                                    tryClose acc value
    in
    go (String.toList s) []
