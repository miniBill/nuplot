module Maybe.MyExtra exposing (withDefaultMaybe)


withDefaultMaybe : Maybe a -> Maybe a -> Maybe a
withDefaultMaybe r l =
    case l of
        Just _ ->
            l

        Nothing ->
            r
