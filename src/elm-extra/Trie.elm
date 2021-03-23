module Trie exposing (Trie, empty, fromList, get, getLongestPrefix, insert, isEmpty, member, size, union)

import Dict exposing (Dict)


type Trie a
    = Trie
        { value : Maybe a
        , children : Dict Char (Trie a)
        }


fromList : List ( String, a ) -> Trie a
fromList =
    List.foldl (\( k, v ) -> insert k v) empty


size : Trie a -> Int
size (Trie { value, children }) =
    let
        childrenSize =
            children
                |> Dict.values
                |> List.map size
                |> List.sum
    in
    if value == Nothing then
        childrenSize

    else
        1 + childrenSize


member : String -> Trie a -> Bool
member key trie =
    get key trie /= Nothing


get : String -> Trie a -> Maybe a
get key =
    let
        go k (Trie { value, children }) =
            case k of
                [] ->
                    value

                h :: t ->
                    Dict.get h children
                        |> Maybe.andThen (go t)
    in
    go (String.toList key)


isEmpty : Trie a -> Bool
isEmpty (Trie { value, children }) =
    value == Nothing && Dict.isEmpty children


insert : String -> a -> Trie a -> Trie a
insert s v =
    let
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
