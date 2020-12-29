module Expression.Cleaner exposing (cleanInput)


cuts : List ( String, String )
cuts =
    [ ( "≥", ">=" )
    , ( "⇒", "=>" )
    , ( "≤", "<=" )
    , ( "≠", "<>" )
    , ( "++", "+" )
    , ( "+-", "-" )
    , ( "-+", "-" )
    , ( "--", "+" )
    , ( "**", "^" )
    , ( "^-1", "^(-1)" )
    , ( "²", "^(2)" )
    , ( "³", "^(3)" )
    , ( "'", "" )
    ]


cleanInput : String -> String
cleanInput string =
    let
        cut f t s =
            let
                replaced =
                    String.replace f t s
            in
            if s == replaced then
                s

            else
                cut f t replaced
    in
    List.foldl (\( ef, et ) -> cut ef et) string cuts
