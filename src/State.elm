module State exposing (State, andThen, get, map, set)


type State s a
    = State (s -> ( s, a ))


get : State s s
get =
    State <| \s -> ( s, s )


set : s -> State s ()
set s =
    State <| \_ -> ( s, () )


runState : State s a -> s -> ( s, a )
runState (State r) =
    r


map : (a -> b) -> State s a -> State s b
map f r =
    State <| \s -> Tuple.mapSecond f <| runState r s


andThen : (a -> State s b) -> State s a -> State s b
andThen f r =
    State <|
        \s ->
            let
                ( s_, x ) =
                    runState r s
            in
            runState (f x) s_
