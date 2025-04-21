module SortedSet exposing (SortedSet, empty, fromList, insert, insertAll, singleton, toList)

import Set exposing (Set)


type SortedSet a
    = SortedSet
        { reverseOrder : List a
        , set : Set a
        }


empty : SortedSet a
empty =
    SortedSet
        { reverseOrder = []
        , set = Set.empty
        }


singleton : comparable -> SortedSet comparable
singleton x =
    SortedSet
        { reverseOrder = [ x ]
        , set = Set.singleton x
        }


fromList : List comparable -> SortedSet comparable
fromList order =
    List.foldl insert empty order


toList : SortedSet a -> List a
toList (SortedSet { reverseOrder }) =
    List.reverse reverseOrder


insert : comparable -> SortedSet comparable -> SortedSet comparable
insert elem ((SortedSet { set, reverseOrder }) as s) =
    if Set.member elem set then
        s

    else
        SortedSet
            { set = Set.insert elem set
            , reverseOrder = elem :: reverseOrder
            }


{-| Insert all the elements from the first argument into the set given by the second argument.
-}
insertAll : SortedSet comparable -> SortedSet comparable -> SortedSet comparable
insertAll (SortedSet l) ((SortedSet _) as r) =
    List.foldr insert r l.reverseOrder
