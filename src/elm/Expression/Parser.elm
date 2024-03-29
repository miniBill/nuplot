module Expression.Parser exposing (ParserContext(..), Problem(..), errorsToString, parse)

import Dict exposing (Dict)
import Expression exposing (Context, Expression(..), RelationOperation(..), VariableStatus(..), defaultContext)
import Expression.Cleaner as Cleaner
import Expression.Utils exposing (by, div, minus, negate_, plus, pow, vector)
import List.Extra as List
import Parser.Advanced as Parser exposing ((|.), (|=), Parser, Step(..), Token(..))
import Trie exposing (Trie)
import UI.L10N exposing (L10N, invariant)
import Unicode


type alias ExpressionParser a =
    Context -> Parser ParserContext Problem a


type ParserContext
    = AddSubtractionContext
    | AtomContext
    | ExpressionContext
    | ListContext
    | MultiDivisionContext
    | NumberContext
    | PowerContext
    | ReplacementContext
    | ReplacementListContext
    | VariableContext
    | VectorContext


contextToString : ParserContext -> L10N String
contextToString ctx =
    case ctx of
        AddSubtractionContext ->
            { en = "an addition or subtraction"
            , it = "un addizione o una sottrazione"
            }

        AtomContext ->
            { en = "an atomic expression"
            , it = "un espressione atomica"
            }

        ExpressionContext ->
            { en = "an expression"
            , it = "un espressione"
            }

        ListContext ->
            { en = "a list"
            , it = "una lista"
            }

        MultiDivisionContext ->
            { en = "a multiplication or a division"
            , it = "una moltiplicazione o una divisione"
            }

        NumberContext ->
            { en = "a number"
            , it = "un numero"
            }

        PowerContext ->
            { en = "an exponentiation"
            , it = "un elevamento a potenza"
            }

        ReplacementContext ->
            { en = "a replacement"
            , it = "una sostituzione"
            }

        ReplacementListContext ->
            { en = "a replacement list"
            , it = "una lista di sostituzioni"
            }

        VariableContext ->
            { en = "a variable"
            , it = "una variabile"
            }

        VectorContext ->
            { en = "a vector"
            , it = "un vettore"
            }


type Problem
    = Expected (L10N String)
    | Unexpected


errorsToString : String -> List (Parser.DeadEnd ParserContext Problem) -> L10N String
errorsToString input err =
    UI.L10N.map (String.join "\n") <|
        UI.L10N.sequence <|
            [ { en = "Error parsing expression:", it = "Errore nel parsing dell'espressione:" }
            , invariant <| "  " ++ input
            ]
                ++ (if List.isEmpty err then
                        [ { en = "No valid parse?"
                          , it = "Nessuna interpretazione possibile?"
                          }
                        ]

                    else
                        err
                            |> List.gatherEqualsBy (\{ col, contextStack } -> { col = col, contextStack = contextStack })
                            |> List.map problemGroupToString
                   )


problemGroupToString :
    ( Parser.DeadEnd ParserContext Problem
    , List (Parser.DeadEnd ParserContext Problem)
    )
    -> L10N String
problemGroupToString ( { col, contextStack, problem }, problems ) =
    let
        allProblems =
            problem :: List.map .problem problems

        prefix =
            case contextStack of
                [] ->
                    invariant <| String.repeat (col + 1) " " ++ "^ "

                frame :: _ ->
                    let
                        pprefix =
                            if col == frame.col then
                                String.repeat (col + 1) " "

                            else
                                String.repeat (frame.col + 1) " "
                                    ++ "|"
                                    ++ String.repeat (col - frame.col - 1) "-"

                        ctxString =
                            contextToString frame.context
                    in
                    { en = pprefix ++ "^ While trying to parse " ++ ctxString.en ++ ". "
                    , it = pprefix ++ "^ Durante il parsing di " ++ ctxString.it ++ ". "
                    }
    in
    UI.L10N.concat
        [ prefix
        , if List.any ((==) Unexpected) allProblems then
            if List.all ((==) Unexpected) allProblems then
                { en = "Unexpected"
                , it = "Inatteso"
                }

            else
                { en = "Unexpected, expected "
                , it = "Inatteso, atteso "
                }

          else
            { en = "Expected "
            , it = "Atteso "
            }
        , allProblems
            |> List.filterMap
                (\p ->
                    case p of
                        Unexpected ->
                            Nothing

                        Expected e ->
                            Just e
                )
            |> (\expecteds ->
                    case List.reverse expecteds of
                        [] ->
                            invariant ""

                        [ single ] ->
                            single

                        last :: init ->
                            UI.L10N.concat
                                [ UI.L10N.map (String.join ", ") <| UI.L10N.sequence (List.reverse init)
                                , { en = " or ", it = " o " }
                                , last
                                ]
               )
        ]


parse : String -> Result (List (Parser.DeadEnd ParserContext Problem)) Expression
parse input =
    input
        |> String.trim
        |> Cleaner.cleanInput
        |> Parser.run
            (Parser.succeed identity
                |= mainParser defaultContext
                |. Parser.end Unexpected
            )
        |> Result.map Expression.Utils.squash


toContext : Dict String (Maybe Expression) -> Context
toContext d =
    { variables = Trie.fromList <| List.map (\v -> ( v, Defined )) <| Dict.keys d
    , functions = Trie.empty
    }


combineContext : Context -> Context -> Context
combineContext old new =
    { variables = Trie.union old.variables new.variables
    , functions = Trie.union old.functions new.functions
    }


mainParser : ExpressionParser Expression
mainParser =
    relationParser


token : String -> Token Problem
token x =
    Token x
        (Expected
            { en = "a '" ++ x ++ "'"
            , it = "un '" ++ x ++ "'"
            }
        )


replacementParser : ExpressionParser Expression
replacementParser context =
    replacementListParser context
        |> Parser.andThen
            (\list ->
                Parser.succeed (Replace list)
                    |. whitespace
                    |= mainParser (combineContext context (toContext list))
            )
        |> Parser.inContext ReplacementContext


expectedALetter : Problem
expectedALetter =
    Expected
        { en = "a letter"
        , it = "una lettera"
        }


replacementListParser : ExpressionParser (Dict String (Maybe Expression))
replacementListParser context =
    let
        longVariable =
            Parser.backtrackable
                (Parser.succeed (\v e -> ( v, Just e ))
                    |= (Parser.chompWhile isVariableLetter
                            |> Parser.getChompedString
                            |> Parser.andThen
                                (\s ->
                                    if String.isEmpty s then
                                        Parser.problem expectedALetter

                                    else
                                        Parser.succeed s
                                )
                       )
                    |. whitespace
                    |. Parser.symbol (token "=")
                    |. whitespace
                    |= Parser.lazy (\_ -> mainParser context)
                )

        shortVariable =
            Parser.succeed (\p e -> ( p, Just e ))
                |= Parser.getChompedString (Parser.chompIf isVariableLetter expectedALetter)
                |. whitespace
                |. (Parser.chompWhile ((==) '=')
                        |> Parser.getChompedString
                        |> Parser.andThen
                            (\s ->
                                if String.length s > 1 then
                                    Parser.problem <| Expected { en = "0 or 1 '='", it = "0 o 1 '='" }

                                else
                                    Parser.succeed ()
                            )
                   )
                |. whitespace
                |= Parser.lazy (\_ -> mainParser context)

        declaration =
            Parser.succeed (\v -> ( v, Nothing ))
                |. Parser.symbol (token "!")
                |= (Parser.chompWhile isVariableLetter
                        |> Parser.getChompedString
                        |> Parser.andThen
                            (\s ->
                                if String.isEmpty s then
                                    Parser.problem expectedALetter

                                else
                                    Parser.succeed s
                            )
                   )
    in
    Parser.inContext ReplacementListContext <|
        (Parser.map Dict.fromList <|
            Parser.sequence
                { start = token "["
                , separator = token ";"
                , end = token "]"
                , spaces = whitespace
                , item =
                    Parser.oneOf
                        [ declaration
                        , longVariable
                        , shortVariable
                        ]
                , trailing = Parser.Optional
                }
        )


whitespace : Parser ParserContext Problem ()
whitespace =
    Parser.chompWhile (\c -> c == ' ' || c == '\n')


listParser : ExpressionParser Expression
listParser context =
    let
        restParser =
            Parser.map List.reverse <|
                Parser.loop [] step

        step acc =
            Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.symbol (token ",")
                    |. whitespace
                    |= Parser.oneOf
                        [ Parser.map (\e -> Parser.Loop <| e :: acc) <|
                            Parser.lazy (\_ -> mainParser context)
                        , Parser.succeed <| Parser.Done acc
                        ]
                , Parser.succeed <| Parser.Done acc
                ]
    in
    Parser.inContext ListContext <|
        Parser.succeed (\h t -> Expression.List (h :: t))
            |. Parser.symbol (token "{")
            |. whitespace
            |= Parser.lazy (\_ -> mainParser context)
            |. whitespace
            |= restParser
            |. Parser.oneOf
                [ Parser.symbol (token "}")
                , Parser.succeed ()
                ]


relationParser : ExpressionParser Expression
relationParser context =
    let
        inner =
            Parser.succeed (\a f -> f a)
                |= addsubtractionParser context
                |. whitespace
                |= Parser.oneOf
                    [ Parser.succeed
                        (\r l ->
                            case l of
                                Variable v ->
                                    Parser.succeed <| Lambda v r

                                _ ->
                                    Parser.problem <|
                                        Expected
                                            { en = "left side of lambda"
                                            , it = "la parte sinistra di una lambda"
                                            }
                        )
                        |. Parser.symbol (token "=>")
                        |. whitespace
                        |= addsubtractionParser context
                    , Parser.succeed (\o r l -> Parser.succeed <| RelationOperation o l r)
                        |= Parser.oneOf
                            [ Parser.succeed LessThanOrEquals |. Parser.symbol (token "<=")
                            , Parser.succeed GreaterThanOrEquals |. Parser.symbol (token ">=")
                            , Parser.succeed LessThan |. Parser.symbol (token "<")
                            , Parser.succeed Equals |. Parser.symbol (token "=")
                            , Parser.succeed GreaterThan |. Parser.symbol (token ">")
                            ]
                        |. whitespace
                        |= addsubtractionParser context
                    , Parser.succeed Parser.succeed
                    ]
    in
    Parser.inContext ExpressionContext <|
        Parser.andThen identity <|
            inner


addsubtractionParser : ExpressionParser Expression
addsubtractionParser context =
    Parser.inContext AddSubtractionContext <|
        multiSequence
            { separators =
                [ ( \l r -> plus [ l, r ], \_ -> Parser.symbol <| token "+" )
                , ( \l r -> minus l r, \_ -> Parser.symbol <| token "-" )
                ]
            , item = multidivisionParser
            , allowNegation = False
            }
            context


multidivisionParser : ExpressionParser Expression
multidivisionParser context =
    let
        signLoopStep b =
            Parser.oneOf
                [ Parser.succeed (Loop <| not b)
                    |. Parser.symbol (token "-")
                , Parser.succeed (Loop b)
                    |. Parser.symbol (token "+")
                , Parser.succeed
                    (Done <|
                        if b then
                            negate_

                        else
                            identity
                    )
                ]
    in
    Parser.inContext MultiDivisionContext <|
        Parser.succeed (\maybeNegate md -> maybeNegate md)
            |= Parser.loop False signLoopStep
            |. whitespace
            |= multiSequence
                { separators =
                    [ ( \l r -> by [ l, r ], \_ -> Parser.symbol <| token "*" )
                    , ( \l r -> div l r, \_ -> Parser.symbol <| token "/" )
                    , ( \l r -> by [ l, r ], \_ -> Parser.succeed () )
                    ]
                , item = powerParser
                , allowNegation = True
                }
                context


powerParser : ExpressionParser Expression
powerParser context =
    Parser.inContext PowerContext <|
        Parser.succeed (\atom maybePow -> maybePow atom)
            |= atomParser context
            |. whitespace
            |= Parser.oneOf
                [ Parser.succeed (\e b -> pow b e)
                    |. Parser.symbol (token "^")
                    |. whitespace
                    |= Parser.lazy (\_ -> powerParser context)
                , Parser.succeed identity
                ]


type alias SequenceData =
    { separators : List ( Expression -> Expression -> Expression, ExpressionParser () )
    , item : ExpressionParser Expression
    , allowNegation : Bool
    }


multiSequence : SequenceData -> ExpressionParser Expression
multiSequence data context =
    Parser.succeed identity
        |= data.item context
        |> Parser.andThen
            (\first ->
                Parser.loop first (\expr -> multiSequenceHelp data expr context)
            )


multiSequenceHelp :
    SequenceData
    -> Expression
    -> ExpressionParser (Step Expression Expression)
multiSequenceHelp { allowNegation, separators, item } acc context =
    let
        separated =
            separators
                |> List.map
                    (\( f, parser ) ->
                        Parser.getChompedString (parser context)
                            |> Parser.andThen
                                (\chomped ->
                                    Parser.succeed (\mn e -> Loop <| f acc <| mn e)
                                        |= (if allowNegation && not (String.isEmpty <| String.trim chomped) then
                                                Parser.oneOf
                                                    [ Parser.succeed negate_
                                                        |. Parser.symbol (token "-")
                                                    , Parser.succeed identity
                                                    ]

                                            else
                                                Parser.succeed identity
                                           )
                                        |. whitespace
                                        |= item context
                                )
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
atomParser context =
    Parser.inContext AtomContext <|
        Parser.oneOf
            [ Parser.inContext VectorContext <|
                Parser.succeed
                    (\es ->
                        case es of
                            [ x ] ->
                                x

                            _ ->
                                vector es
                    )
                    |= Parser.sequence
                        { start = token "("
                        , separator = token ","
                        , end = token ""
                        , spaces = whitespace
                        , item = Parser.lazy (\_ -> mainParser context)
                        , trailing = Parser.Forbidden
                        }
                    |. whitespace
                    |. optional (Parser.symbol (token ")"))
            , replacementParser context
            , listParser context
            , Parser.inContext VariableContext <| variableParser context
            , Parser.inContext NumberContext <|
                Parser.andThen tryParseNumber <|
                    Parser.getChompedString <|
                        Parser.succeed ()
                            |. Parser.chompIf (\c -> c == '.' || Char.isDigit c)
                                (Expected
                                    { en = "a digit, or a dot"
                                    , it = "una cifra, o un punto"
                                    }
                                )
                            |. Parser.chompWhile (\c -> c == '.' || Char.isDigit c)
            ]


tryParseNumber : String -> Parser ParserContext Problem Expression
tryParseNumber n =
    case String.toInt n of
        Just i ->
            Parser.succeed <| Integer i

        Nothing ->
            case String.toFloat n of
                Just f ->
                    Parser.succeed <| Float f

                Nothing ->
                    Parser.problem
                        (Expected
                            { en = "a number"
                            , it = "un numero"
                            }
                        )


optional : Parser c x () -> Parser c x ()
optional p =
    Parser.oneOf
        [ p
        , Parser.succeed ()
        ]


variableParser : ExpressionParser Expression
variableParser context =
    let
        chomp i =
            if i <= 0 then
                Parser.succeed ()

            else
                Parser.succeed identity
                    |. Parser.chompIf (\_ -> True) Unexpected
                    |= chomp (i - 1)
    in
    (Parser.succeed String.dropLeft
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.andThen
            (\rest ->
                case Trie.getLongestPrefix rest context.functions of
                    Just ( nameString, ( name, arity ) ) ->
                        Parser.succeed
                            (Maybe.map (Apply name)
                                >> Maybe.withDefault
                                    (let
                                        free =
                                            getFirstFree context.variables
                                     in
                                     Lambda free <| Apply name [ Variable free ]
                                    )
                            )
                            |. chomp (String.length nameString)
                            |. whitespace
                            |= Parser.oneOf
                                [ Parser.succeed Just
                                    |. Parser.symbol (token "(")
                                    |. whitespace
                                    |= parseArgs Nothing True context
                                    |. Parser.oneOf [ Parser.symbol (token ")"), Parser.succeed () ]
                                    |. whitespace
                                , Parser.map Just <| parseArgs arity False context
                                , Parser.succeed Nothing
                                    |. whitespace
                                ]

                    Nothing ->
                        case Trie.getLongestPrefix rest context.variables of
                            Just ( name, _ ) ->
                                Parser.succeed (Variable name)
                                    |. chomp (String.length name)

                            Nothing ->
                                let
                                    l =
                                        String.left 1 rest
                                in
                                case String.toList l of
                                    [ v ] ->
                                        if isVariableLetter v then
                                            Parser.succeed (Variable l)
                                                |. chomp 1

                                        else
                                            Parser.problem expectedALetter

                                    _ ->
                                        Parser.problem expectedALetter
            )


getFirstFree : Trie VariableStatus -> String
getFirstFree trie =
    let
        alphabeth =
            List.map (String.fromChar << Char.fromCode) <| List.range (Char.toCode 'a') (Char.toCode 'z')

        go : List String -> String
        go lst =
            case List.find (\p -> Trie.get p trie /= Just Defined) lst of
                Just p ->
                    p

                Nothing ->
                    go (List.concatMap (\l -> List.map (\r -> l ++ r) alphabeth) lst)
    in
    go alphabeth


parseArgs : Maybe Int -> Bool -> ExpressionParser (List Expression)
parseArgs count greedy context =
    case count of
        Just 0 ->
            Parser.succeed []

        Just 1 ->
            Parser.succeed (\e -> [ e ])
                |= Parser.lazy
                    (\_ ->
                        if greedy then
                            mainParser context

                        else
                            powerParser context
                    )
                |. whitespace

        Just c ->
            Parser.succeed (::)
                |= Parser.lazy (\_ -> mainParser context)
                |. whitespace
                |. Parser.symbol (token ",")
                |. whitespace
                |= parseArgs (Just (c - 1)) greedy context
                |. whitespace

        Nothing ->
            Parser.oneOf
                [ Parser.succeed (::)
                    |= Parser.backtrackable (Parser.lazy (\_ -> mainParser context))
                    |. whitespace
                    |. Parser.symbol (token ",")
                    |. Parser.commit ()
                    |. whitespace
                    |= Parser.lazy (\_ -> parseArgs Nothing greedy context)
                    |. whitespace
                , Parser.succeed (\e -> [ e ])
                    |= Parser.lazy
                        (\_ ->
                            if greedy then
                                mainParser context

                            else
                                powerParser context
                        )
                    |. whitespace
                ]


isVariableLetter : Char -> Bool
isVariableLetter =
    Unicode.isAlpha
