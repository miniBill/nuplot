module SortedSet exposing (SortedSet, insert, member, toList)

import Set exposing (Set)


type SortedSet a
    = SortedSet
        { set : Set a
        , list : List a
        }


member : comparable -> SortedSet comparable -> Bool
member x (SortedSet { set }) =
    Set.member x set


insert : comparable -> SortedSet comparable -> SortedSet comparable
insert x ((SortedSet { set, list }) as o) =
    if Set.member x set then
        o

    else
        SortedSet
            { set = Set.insert x set
            , list = x :: list
            }


toList : SortedSet a -> List a
toList (SortedSet { list }) =
    List.reverse list
