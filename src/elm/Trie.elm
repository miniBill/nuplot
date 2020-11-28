module Trie exposing (Trie, empty, fromList, get, getLongestPrefix, insert, isPrefix, union)

import Dict exposing (Dict)


type Trie a
    = Trie
        { value : Maybe a
        , children : Dict Char (Trie a)
        }


fromList : List ( String, a ) -> Trie a
fromList =
    List.foldl (\( k, v ) -> insert k v) empty


insert : String -> a -> Trie a -> Trie a
insert s v =
    let
        go ss (Trie trie) =
            case ss of
                [] ->
                    Trie { trie | value = Just v }

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
    let
        go x y =
            case x of
                [] ->
                    y

                (( xc, xn ) as xh) :: xt ->
                    case y of
                        [] ->
                            x

                        (( yc, yn ) as yh) :: yt ->
                            if xc == yc then
                                ( xc, union xn yn ) :: go xt yt

                            else if xc < yc then
                                xh :: go xt y

                            else
                                yh :: go x yt

        value =
            case l.value of
                Nothing ->
                    r.value

                Just _ ->
                    l.value
    in
    Trie
        { value = value
        , children = Dict.fromList <| go (Dict.toList l.children) (Dict.toList r.children)
        }


get : List Char -> Trie a -> Maybe a
get cs (Trie { value, children }) =
    case cs of
        [] ->
            value

        s :: ss ->
            case Dict.get s children of
                Nothing ->
                    Nothing

                Just child ->
                    get ss child


getLongestPrefix : String -> Trie a -> Maybe ( String, a )
getLongestPrefix s =
    let
        tryClose acc value =
            Maybe.map (\v -> ( String.fromList <| List.reverse acc, v )) value

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


isPrefix : List Char -> Trie a -> Bool
isPrefix cs (Trie { children }) =
    case cs of
        [] ->
            True

        s :: ss ->
            case Dict.get s children of
                Nothing ->
                    False

                Just child ->
                    isPrefix ss child
