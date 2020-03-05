module State exposing (State, andThen, get, map, set, while)


type alias State s a =
    s -> ( s, a )


get : State s s
get s =
    ( s, s )


set : s -> State s ()
set s _ =
    ( s, () )


map : (a -> b) -> State s a -> State s b
map f r s =
    let
        ( s_, x ) =
            r s
    in
    ( s_, f x )


map2 : (a -> b -> c) -> State s a -> State s b -> State s c
map2 f l r s =
    let
        ( s_, x ) =
            l s

        ( s__, y ) =
            r s_
    in
    ( s__, f x y )


andThen : (a -> State s b) -> State s a -> State s b
andThen f r s =
    let
        ( s_, x ) =
            r s
    in
    f x s_


return : a -> State s a
return x s =
    ( s, x )


while : State s Bool -> State s a -> State s (List a)
while cond step =
    cond
        |> andThen
            (\c ->
                if c then
                    map2 (::) step (while cond step)

                else
                    return []
            )
