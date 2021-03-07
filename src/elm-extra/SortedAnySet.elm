module SortedAnySet exposing (SortedAnySet, empty, fromList, insert, isEmpty, member, singleton, toList)


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


fromList : List a -> SortedAnySet a
fromList =
    List.foldr insert empty


toList : SortedAnySet a -> List a
toList (SortedAnySet list) =
    List.reverse list


singleton : a -> SortedAnySet a
singleton x =
    SortedAnySet [ x ]


empty : SortedAnySet a
empty =
    SortedAnySet []


isEmpty : SortedAnySet a -> Bool
isEmpty (SortedAnySet l) =
    List.isEmpty l
