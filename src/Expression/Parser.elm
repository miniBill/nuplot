module Expression.Parser exposing (Context(..), Problem(..), parse)

import Dict
import Expression exposing (Expression(..), by, div, minus, plus, pow)
import Parser.Advanced as Parser exposing ((|.), (|=), Parser, Step(..), Token(..))


type alias ExpressionParser a =
    Parser Context Problem a


type Context
    = Expression
    | List
    | Replacement
    | ReplacementList


type Problem
    = Expected String
    | Unexpected


parse : String -> Result (List (Parser.DeadEnd Context Problem)) Expression
parse input =
    input
        |> String.trim
        |> Parser.run
            (Parser.succeed identity
                |= mainParser
                |. Parser.end Unexpected
            )
        |> Result.map
            (\r ->
                r
                    |> explode defaultContext
                    |> Expression.squash
            )


defaultContext : List String
defaultContext =
    [ "sqrt", "sin", "cos", "sinh", "cosh", "gra" ]
        |> List.sortBy (\s -> -(String.length s))


explode : List String -> Expression -> Expression
explode context expr =
    let
        findPrefixes w =
            case String.uncons w of
                Just ( h, t ) ->
                    context
                        |> List.foldl
                            (\e acc ->
                                case acc of
                                    Just found ->
                                        Just found

                                    Nothing ->
                                        if e == w then
                                            Just [ e ]

                                        else if String.startsWith e w then
                                            Just <| e :: findPrefixes (String.dropLeft (String.length e) w)

                                        else
                                            Nothing
                            )
                            Nothing
                        |> Maybe.withDefault (String.fromChar h :: findPrefixes t)

                Nothing ->
                    []
    in
    case expr of
        Variable v ->
            if String.length v < 2 then
                expr

            else
                by <| List.map Variable <| findPrefixes v

        Replace vars e ->
            Replace (Dict.map (\_ -> explode context) vars) <|
                explode
                    ((context ++ Dict.keys vars)
                        |> List.sortBy (\s -> -(String.length s))
                    )
                    e

        UnaryOperation uop e ->
            UnaryOperation uop <| explode context e

        BinaryOperation bop l r ->
            BinaryOperation bop (explode context l) (explode context r)

        AssociativeOperation aop l r o ->
            AssociativeOperation aop (explode context l) (explode context r) (List.map (explode context) o)

        Expression.List es ->
            Expression.List <| List.map (explode context) es

        _ ->
            expr


mainParser : ExpressionParser Expression
mainParser =
    addsubtractionParser


token : String -> Token Problem
token x =
    Token x (Expected <| "a '" ++ x ++ "'")


replacementParser : ExpressionParser Expression
replacementParser =
    Parser.inContext Replacement <|
        Parser.succeed Replace
            |= Parser.inContext ReplacementList
                (Parser.map Dict.fromList <|
                    Parser.sequence
                        { start = token "["
                        , separator = token ";"
                        , end = token "]"
                        , spaces = whitespace
                        , item =
                            Parser.succeed (\h ( t, v ) -> ( h ++ t, v ))
                                |= Parser.getChompedString (Parser.chompIf Char.isAlpha (Expected "a letter"))
                                |= Parser.oneOf
                                    [ Parser.backtrackable
                                        (Parser.succeed Tuple.pair
                                            |= (Parser.chompWhile Char.isAlpha
                                                    |> Parser.getChompedString
                                                    |> Parser.andThen
                                                        (\s ->
                                                            if String.isEmpty s then
                                                                Parser.problem <| Expected "a letter"

                                                            else
                                                                Parser.succeed s
                                                        )
                                               )
                                            |. whitespace
                                            |. Parser.symbol (token "=")
                                            |. whitespace
                                            |= Parser.lazy (\_ -> mainParser)
                                        )
                                    , Parser.succeed (\e -> ( "", e ))
                                        |. whitespace
                                        |. (Parser.chompWhile ((==) '=')
                                                |> Parser.getChompedString
                                                |> Parser.andThen
                                                    (\s ->
                                                        if String.length s > 1 then
                                                            Parser.problem <| Expected "0 or 1 '='"

                                                        else
                                                            Parser.succeed ()
                                                    )
                                           )
                                        |. whitespace
                                        |= Parser.lazy (\_ -> mainParser)
                                    ]
                        , trailing = Parser.Optional
                        }
                )
            |= Parser.lazy (\_ -> mainParser)


whitespace : ExpressionParser ()
whitespace =
    Parser.chompWhile (\c -> c == ' ' || c == '\n')


listParser : ExpressionParser Expression
listParser =
    Parser.inContext List <|
        Parser.map Expression.List <|
            Parser.sequence
                { start = token "{"
                , separator = token ","
                , end = token "}"
                , spaces = whitespace
                , item = Parser.lazy (\_ -> mainParser)
                , trailing = Parser.Optional
                }


addsubtractionParser : ExpressionParser Expression
addsubtractionParser =
    Parser.inContext Expression <|
        multiSequence
            { separators =
                [ ( \l r -> plus [ l, r ], Parser.symbol <| token "+" )
                , ( \l r -> minus l r, Parser.symbol <| token "-" )
                ]
            , item = multidivisionParser
            }


multidivisionParser : ExpressionParser Expression
multidivisionParser =
    multiSequence
        { separators =
            [ ( \l r -> by [ l, r ], Parser.symbol <| token "*" )
            , ( \l r -> div l r, Parser.symbol <| token "/" )
            , ( \l r -> by [ l, r ], Parser.succeed () )
            ]
        , item = powerParser
        }


powerParser : ExpressionParser Expression
powerParser =
    Parser.succeed (\a f -> f a)
        |= atomParser
        |. whitespace
        |= Parser.oneOf
            [ Parser.succeed (\e b -> pow b e)
                |. Parser.symbol (token "^")
                |. whitespace
                |= Parser.lazy (\_ -> powerParser)
            , Parser.succeed identity
            ]


type alias SequenceData =
    { separators : List ( Expression -> Expression -> Expression, ExpressionParser () )
    , item : ExpressionParser Expression
    }


multiSequence : SequenceData -> ExpressionParser Expression
multiSequence data =
    data.item
        |> Parser.andThen
            (\first ->
                Parser.loop first (multiSequenceHelp data)
            )


multiSequenceHelp :
    SequenceData
    -> Expression
    -> ExpressionParser (Step Expression Expression)
multiSequenceHelp { separators, item } acc =
    let
        separated =
            separators
                |> List.map
                    (\( f, parser ) ->
                        Parser.succeed (\e -> Loop <| f acc e)
                            |. parser
                            |. whitespace
                            |= item
                    )
                |> Parser.oneOf
                |> (\p ->
                        Parser.succeed identity
                            |. whitespace
                            |= p
                   )
    in
    Parser.oneOf
        [ separated
        , Parser.succeed (Done acc)
        ]


atomParser : ExpressionParser Expression
atomParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol (token "(")
            |. whitespace
            |= Parser.lazy (\_ -> mainParser)
            |. whitespace
            |. Parser.symbol (token ")")
        , replacementParser
        , listParser
        , Parser.chompWhile Char.isLower
            |> Parser.getChompedString
            |> Parser.andThen
                (\s ->
                    if String.isEmpty s then
                        Parser.problem (Expected "a lowercase char")

                    else
                        Parser.succeed (Variable s)
                )
        , Parser.number
            { binary = Ok Integer
            , float = Ok Float
            , hex = Ok Integer
            , int = Ok Integer
            , octal = Ok Integer
            , invalid = Expected "a valid number"
            , expecting = Expected "a number"
            }
        ]
