module UI.Glsl.Poly exposing (Poly, asPoly)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), RelationOperation(..), UnaryOperation(..))
import List
import List.Extra as List
import Maybe.Extra as Maybe


type alias Poly =
    Dict (List String) Float


asPoly : Expression -> Maybe Poly
asPoly e =
    let
        res =
            case e of
                Integer i ->
                    Just <| Dict.singleton [] (toFloat i)

                Float f ->
                    Just <| Dict.singleton [] f

                Variable v ->
                    Just <| Dict.singleton [ v ] (toFloat 1)

                UnaryOperation Negate c ->
                    c
                        |> asPoly
                        |> Maybe.map negate_

                AssociativeOperation Addition l m r ->
                    (l :: m :: r)
                        |> Maybe.traverse asPoly
                        |> Maybe.map (List.foldr sum Dict.empty)

                AssociativeOperation Multiplication l m r ->
                    (l :: m :: r)
                        |> Maybe.traverse asPoly
                        |> Maybe.map (List.foldr by (Dict.singleton [] 1))

                BinaryOperation Power b (Integer i) ->
                    Maybe.andThen (pow i) (asPoly b)

                RelationOperation Equals l r ->
                    Maybe.map2 (\lp rp -> sum lp <| negate_ rp)
                        (asPoly l)
                        (asPoly r)

                _ ->
                    Nothing
    in
    res


negate_ : Poly -> Poly
negate_ =
    Dict.map <| always negate


sum : Poly -> Poly -> Poly
sum l r =
    let
        go ll rl acc =
            case ( ll, rl ) of
                ( [], _ ) ->
                    rl ++ acc

                ( _, [] ) ->
                    ll ++ acc

                ( (( ln, lc ) as lh) :: lt, (( rn, rc ) as rh) :: rt ) ->
                    if ln == rn then
                        go lt rl (( ln, lc + rc ) :: acc)

                    else if ln < rn then
                        go lt rl (lh :: acc)

                    else
                        go ll rt (rh :: acc)
    in
    Dict.fromList <| go (Dict.toList l) (Dict.toList r) []


by : Poly -> Poly -> Poly
by l r =
    List.cartesianProduct [ Dict.toList l, Dict.toList r ]
        |> List.map
            (\ls ->
                ( List.sort <| List.concat <| List.map Tuple.first ls
                , List.product <| List.map Tuple.second ls
                )
            )
        |> List.foldr (\( k, v ) -> Dict.update k (Maybe.withDefault 0 >> (+) v >> Just)) Dict.empty


pow : Int -> Poly -> Maybe Poly
pow e p =
    if e < 0 then
        Nothing

    else if e == 0 then
        Just <| Dict.singleton [] 1

    else if e == 1 then
        Just p

    else
        Maybe.map (by p) (pow (e - 1) p)
