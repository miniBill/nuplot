module Expression.Parser exposing (ParserContext(..), Problem(..), errorsToString, expressionToGraph, parse)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Context, Expression(..), Graph(..), RelationOperation(..), VariableStatus(..), defaultContext, getFreeVariables)
import Expression.Cleaner as Cleaner
import Expression.Utils exposing (by, div, minus, negate_, plus, pow, vector)
import List
import List.Extra as List
import Parser.Advanced as Parser exposing ((|.), (|=), Parser, Step(..), Token(..), getChompedString)
import Set
import Trie exposing (Trie)


type alias ExpressionParser a =
    Context -> Parser ParserContext Problem a


type ParserContext
    = ExpressionContext
    | ListContext
    | ReplacementContext
    | ReplacementListContext
    | NumberContext


type Problem
    = Expected String
    | Unexpected


errorsToString : String -> List (Parser.DeadEnd ParserContext Problem) -> String
errorsToString input err =
    String.join "\n" <|
        [ "Error parsing expression:"
        , "  " ++ input
        ]
            ++ (if List.isEmpty err then
                    [ "No valid parse?" ]

                else
                    List.map
                        (\( { col, problem }, problems ) ->
                            let
                                allProblems =
                                    problem :: List.map .problem problems
                            in
                            String.concat
                                [ String.repeat (col + 1) " "
                                , "^--"
                                , if List.any ((==) Unexpected) allProblems then
                                    if List.all ((==) Unexpected) allProblems then
                                        "Unexpected"

                                    else
                                        "Unexpected, expected: "

                                  else
                                    "Expected: "
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
                                                    ""

                                                [ single ] ->
                                                    single

                                                last :: init ->
                                                    String.join ", " (List.reverse init) ++ " or " ++ last
                                       )
                                ]
                        )
                    <|
                        List.gatherEqualsBy .col err
               )


parse : String -> Result (List (Parser.DeadEnd ParserContext Problem)) Expression
parse input =
    let
        log _ =
            {- if String.contains "DEBUG" input then
                   Debug.log msg

               else
            -}
            identity
    in
    input
        |> String.trim
        |> Cleaner.cleanInput
        |> Parser.run
            (Parser.succeed identity
                |= mainParser defaultContext
                |. Parser.end Unexpected
            )
        |> Result.map
            (\r ->
                r
                    |> log "raw"
                    |> Expression.Utils.squash
                    |> log "after squash"
            )


expressionToGraph : Expression -> Graph
expressionToGraph expr =
    case expr of
        Replace ctx (RelationOperation rop l r) ->
            expressionToGraph (RelationOperation rop (Replace ctx l) (Replace ctx r))

        RelationOperation rop l r ->
            case ( l, rop, Set.member "y" <| getFreeVariables r ) of
                ( Variable "y", Equals, False ) ->
                    if Set.member "z" <| getFreeVariables expr then
                        Implicit3D expr

                    else
                        Explicit2D r

                ( _, Equals, _ ) ->
                    if Set.member "z" <| getFreeVariables expr then
                        Implicit3D expr

                    else
                        Implicit2D l r

                _ ->
                    Relation2D expr

        List ls ->
            GraphList <| List.map expressionToGraph ls

        _ ->
            let
                free =
                    getFreeVariables expr
            in
            if Set.member "z" free then
                Implicit3D expr

            else if Set.member "y" free then
                Contour expr

            else
                Explicit2D expr


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
    Token x (Expected <| "a '" ++ x ++ "'")


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
                                        Parser.problem <| Expected "a letter"

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
                |= Parser.getChompedString (Parser.chompIf isVariableLetter (Expected "a letter"))
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
                |= Parser.lazy (\_ -> mainParser context)

        declaration =
            Parser.succeed (\v -> ( v, Nothing ))
                |. Parser.symbol (token "!")
                |= (Parser.chompWhile isVariableLetter
                        |> Parser.getChompedString
                        |> Parser.andThen
                            (\s ->
                                if String.isEmpty s then
                                    Parser.problem <| Expected "a letter"

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
    Parser.inContext ExpressionContext <|
        Parser.succeed (\a f -> f a)
            |= addsubtractionParser context
            |. whitespace
            |= Parser.oneOf
                [ Parser.succeed (\o r l -> RelationOperation o l r)
                    |= Parser.oneOf
                        [ Parser.succeed LessThanOrEquals |. Parser.symbol (token "<=")
                        , Parser.succeed GreaterThanOrEquals |. Parser.symbol (token ">=")
                        , Parser.succeed LessThan |. Parser.symbol (token "<")
                        , Parser.succeed Equals |. Parser.symbol (token "=")
                        , Parser.succeed GreaterThan |. Parser.symbol (token ">")
                        ]
                    |. whitespace
                    |= addsubtractionParser context
                , Parser.succeed identity
                ]


addsubtractionParser : ExpressionParser Expression
addsubtractionParser context =
    multiSequence
        { separators =
            [ ( \l r -> plus [ l, r ], \_ -> Parser.symbol <| token "+" )
            , ( \l r -> minus l r, \_ -> Parser.symbol <| token "-" )
            ]
        , item = multidivisionParser
        }
        context


multidivisionParser : ExpressionParser Expression
multidivisionParser context =
    multiSequence
        { separators =
            [ ( \l r -> by [ l, r ], \_ -> Parser.symbol <| token "*" )
            , ( \l r -> div l r, \_ -> Parser.symbol <| token "/" )
            , ( \l r -> by [ l, r ], \_ -> Parser.succeed () )
            ]
        , item = powerParser
        }
        context


powerParser : ExpressionParser Expression
powerParser context =
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
    }


multiSequence : SequenceData -> ExpressionParser Expression
multiSequence data context =
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.succeed negate_
                |. Parser.symbol (token "-")
            , Parser.succeed identity
            ]
        |= data.item context
        |> Parser.andThen
            (\first ->
                Parser.loop first (\expr -> multiSequenceHelp data expr context)
            )


multiSequenceHelp :
    SequenceData
    -> Expression
    -> ExpressionParser (Step Expression Expression)
multiSequenceHelp { separators, item } acc context =
    let
        separated =
            separators
                |> List.map
                    (\( f, parser ) ->
                        Parser.succeed (\e -> Loop <| f acc e)
                            |. parser context
                            |. whitespace
                            |= item context
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
    Parser.oneOf
        [ Parser.succeed
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
        , variableParser context
        , Parser.andThen tryParseNumber <|
            getChompedString <|
                Parser.succeed ()
                    |. Parser.chompIf (\c -> c == '.' || Char.isDigit c) (Expected "a digit, or a dot")
                    |. Parser.chompWhile (\c -> c == '.' || Char.isDigit c)
        ]


tryParseNumber : String -> Parser ParserContext Problem Expression
tryParseNumber n =
    Parser.inContext NumberContext <|
        case String.toInt n of
            Just i ->
                Parser.succeed <| Integer i

            Nothing ->
                case String.toFloat n of
                    Just f ->
                        Parser.succeed <| Float f

                    Nothing ->
                        Parser.problem (Expected "a number")


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
                                    |= parseArgs arity True context
                                    |. Parser.oneOf [ Parser.symbol (token ")"), Parser.succeed () ]
                                , Parser.map Just <| parseArgs arity False context
                                , Parser.succeed Nothing
                                ]

                    Nothing ->
                        case Trie.getLongestPrefix rest context.variables of
                            Just ( name, _ ) ->
                                Parser.succeed (Variable name)
                                    |. chomp (String.length name)

                            Nothing ->
                                Parser.problem (Expected "a letter")
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


parseArgs : Int -> Bool -> ExpressionParser (List Expression)
parseArgs count greedy context =
    case count of
        0 ->
            Parser.succeed []

        1 ->
            if greedy then
                Parser.succeed (\e -> [ e ])
                    |= Parser.lazy (\_ -> mainParser context)
                    |. whitespace

            else
                Parser.succeed (\e -> [ e ])
                    |= Parser.lazy (\_ -> powerParser context)
                    |. whitespace

        _ ->
            Parser.succeed (::)
                |= Parser.lazy (\_ -> mainParser context)
                |. whitespace
                |. Parser.symbol (token ",")
                |. whitespace
                |= parseArgs (count - 1) greedy context


isVariableLetter : Char -> Bool
isVariableLetter c =
    Char.isAlpha c || (Char.toCode c >= Char.toCode 'Ά' && Char.toCode c <= Char.toCode 'Ͽ')
