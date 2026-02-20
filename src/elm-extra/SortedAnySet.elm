module SortedAnySet exposing (SortedAnySet, empty, insert, member, toList)


type SortedAnySet a
    = SortedAnySet (List a)


member : a -> SortedAnySet a -> Bool
member x (SortedAnySet list) =
    List.member x list


insert : a -> SortedAnySet a -> SortedAnySet a
insert x ((SortedAnySet list) as o) =
    if List.member x list then
        o

    else
        SortedAnySet <| x :: list


toList : SortedAnySet a -> List a
toList (SortedAnySet list) =
    List.reverse list


empty : SortedAnySet a
empty =
    SortedAnySet []
