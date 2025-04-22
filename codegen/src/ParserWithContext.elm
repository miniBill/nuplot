module ParserWithContext exposing (Parser, andThen, backtrackable, chompIf, chompWhile, commit, end, float, getChompedString, inContext, int, keyword, lazy, loop, many, map, oneOf, problem, sequence, spaces, succeed, symbol, token)

import Parser exposing (Problem(..))
import Parser.Advanced exposing ((|.), (|=), Step, Token(..))
import Parser.Advanced.Workaround


type alias Parser c a =
    Parser.Advanced.Parser c Problem a


inContext : c -> Parser c a -> Parser c a
inContext ctx parser =
    Parser.Advanced.inContext ctx parser


succeed : a -> Parser c a
succeed x =
    Parser.Advanced.succeed x


getChompedString : Parser c a -> Parser c String
getChompedString parser =
    Parser.Advanced.getChompedString parser


symbol : String -> Parser c ()
symbol k =
    Parser.Advanced.symbol (token k)
        |. spaces


keyword : String -> Parser c ()
keyword k =
    Parser.Advanced.keyword (token k)
        |. spaces


token : String -> Token Problem
token k =
    if String.isEmpty k then
        Token k UnexpectedChar

    else
        Token k (Expecting k)


end : Parser c ()
end =
    Parser.Advanced.end ExpectingEnd


many : Parser c a -> Parser c (List a)
many item =
    sequence
        { start = ""
        , separator = ""
        , item = item
        , end = ""
        , trailing = Parser.Advanced.Optional
        }


chompWhile : (Char -> Bool) -> Parser c ()
chompWhile cond =
    Parser.Advanced.chompWhile cond


chompIf : (Char -> Bool) -> Problem -> Parser c ()
chompIf cond k =
    Parser.Advanced.chompIf cond k


oneOf : List (Parser c a) -> Parser c a
oneOf options =
    Parser.Advanced.oneOf options


spaces : Parser c ()
spaces =
    let
        inner : Parser c ()
        inner =
            chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')

        comment : Parser c ()
        comment =
            oneOf
                [ Parser.Advanced.Workaround.lineCommentAfter (token "//")
                , Parser.Advanced.Workaround.multiCommentAfter (token "/*") (token "*/") Parser.Advanced.NotNestable
                ]
    in
    Parser.Advanced.sequence
        { start = token ""
        , end = token ""
        , trailing = Parser.Advanced.Optional
        , spaces = inner
        , separator = token ""
        , item = comment
        }
        |> Parser.Advanced.map (\_ -> ())


sequence :
    { start : String
    , end : String
    , trailing : Parser.Advanced.Trailing
    , separator : String
    , item : Parser c a
    }
    -> Parser c (List a)
sequence config =
    Parser.Advanced.sequence
        { start = token config.start
        , end = token config.end
        , trailing = config.trailing
        , spaces = spaces
        , separator = token config.separator
        , item = config.item
        }
        |. spaces


lazy : (() -> Parser c a) -> Parser c a
lazy k =
    Parser.Advanced.lazy k


map : (a -> b) -> Parser c a -> Parser c b
map f parser =
    Parser.Advanced.map f parser


loop : a -> (a -> Parser c (Step a b)) -> Parser c b
loop acc step =
    Parser.Advanced.loop acc step


backtrackable : Parser c x -> Parser c x
backtrackable parser =
    Parser.Advanced.backtrackable parser


problem : String -> Parser c a
problem msg =
    Parser.Advanced.problem (Problem msg)


int : Parser c Int
int =
    (succeed ()
        |. chompIf Char.isDigit ExpectingInt
        |. chompWhile Char.isDigit
        |> getChompedString
        |> andThen
            (\s ->
                case String.toInt s of
                    Just i ->
                        succeed i

                    Nothing ->
                        Parser.Advanced.problem ExpectingInt
            )
    )
        |. spaces


float : Parser c Float
float =
    let
        mantissaWithDot : Parser c Float
        mantissaWithDot =
            oneOf
                [ succeed ()
                    |. symbol "."
                    |. int
                , succeed ()
                    |. backtrackable int
                    |. symbol "."
                    |. commit ()
                    |. oneOf
                        [ int
                        , succeed 0
                        ]
                ]
                |> getChompedString
                |> andThen
                    (\s ->
                        let
                            withZero : String
                            withZero =
                                if String.endsWith "." s then
                                    s ++ "0"

                                else
                                    s
                        in
                        case String.toFloat (String.trimRight withZero) of
                            Just f ->
                                succeed f

                            Nothing ->
                                problem ("Cannot parse \"" ++ s ++ "\" as float")
                    )

        exponent : Parser c Int
        exponent =
            succeed identity
                |. oneOf [ symbol "e", symbol "E" ]
                |. commit ()
                |= oneOf
                    [ succeed identity
                        |. symbol "+"
                        |= int
                    , succeed negate
                        |. symbol "-"
                        |= int
                    , int
                    ]
    in
    oneOf
        [ succeed (\c e -> c * 10 ^ toFloat e)
            |= mantissaWithDot
            |= oneOf
                [ exponent
                , succeed 0
                ]
        , succeed (\c e -> toFloat c * 10 ^ toFloat e)
            |= backtrackable int
            |= exponent
        ]


andThen : (a -> Parser c b) -> Parser c a -> Parser c b
andThen f parser =
    Parser.Advanced.andThen f parser


commit : a -> Parser c a
commit x =
    Parser.Advanced.commit x
