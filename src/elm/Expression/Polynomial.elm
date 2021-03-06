module Expression.Polynomial exposing (Exponents, Polynomial, asPolynomial)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), UnaryOperation(..))
import Expression.Utils exposing (byShort, negateShort, one, plusShort)
import List
import List.Extra as List
import List.MyExtra as List
import Maybe.Extra as Maybe


type alias Exponents =
    List ( String, Int )


type alias Polynomial =
    Dict Exponents Expression


asPolynomial : List String -> Expression -> Maybe Polynomial
asPolynomial vars e =
    let
        res =
            case e of
                Integer _ ->
                    Just <| Dict.singleton [] e

                Float _ ->
                    Just <| Dict.singleton [] e

                Variable v ->
                    if List.member v vars then
                        Just <| Dict.singleton [ ( v, 1 ) ] one

                    else
                        Just <| Dict.singleton [] e

                UnaryOperation Negate c ->
                    asPolynomial vars c
                        |> Maybe.map negate_

                AssociativeOperation Addition l m r ->
                    (l :: m :: r)
                        |> Maybe.traverse (asPolynomial vars)
                        |> Maybe.map (List.foldr sum Dict.empty)

                AssociativeOperation Multiplication l m r ->
                    (l :: m :: r)
                        |> Maybe.traverse (asPolynomial vars)
                        |> Maybe.map (List.foldr by (Dict.singleton [] (Integer 1)))

                BinaryOperation Power b (Integer i) ->
                    Maybe.andThen (pow i) (asPolynomial vars b)

                _ ->
                    Nothing
    in
    res


negate_ : Polynomial -> Polynomial
negate_ =
    Dict.map <| always negateShort


sum : Polynomial -> Polynomial -> Polynomial
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
                        go lt rl (( ln, plusShort [ lc, rc ] ) :: acc)

                    else if ln < rn then
                        go lt rl (lh :: acc)

                    else
                        go ll rt (rh :: acc)
    in
    Dict.fromList <| go (Dict.toList l) (Dict.toList r) []


by : Polynomial -> Polynomial -> Polynomial
by l r =
    List.cartesianProduct [ Dict.toList l, Dict.toList r ]
        |> List.filterMap
            (\p ->
                case p of
                    [ v, w ] ->
                        Just ( v, w )

                    _ ->
                        Nothing
            )
        |> List.map
            (\( ( les, lc ), ( res, rc ) ) ->
                ( (les ++ res)
                    |> List.sort
                    |> List.groupOneWith
                        (\( ln, le ) ( rn, re ) ->
                            if ln == rn then
                                Just ( ln, le + re )

                            else
                                Nothing
                        )
                , byShort [ lc, rc ]
                )
            )
        |> List.foldr
            (\( k, v ) ->
                Dict.update k
                    (Maybe.withDefault (Integer 0)
                        >> (\w -> plusShort [ v, w ])
                        >> Just
                    )
            )
            Dict.empty


pow : Int -> Polynomial -> Maybe Polynomial
pow e p =
    if e < 0 then
        Nothing

    else if e == 0 then
        Just <| Dict.singleton [] (Integer 1)

    else if e == 1 then
        Just p

    else
        Maybe.map (by p) (pow (e - 1) p)
