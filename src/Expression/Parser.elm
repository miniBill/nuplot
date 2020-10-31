module Expression.Parser exposing (Context(..), Problem(..), errorsToString, parse, parseGraph)

import Dict
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), Graph(..), RelationOperation(..), defaultContext, getFreeVariables, isFunction, visit)
import Expression.Utils exposing (by, div, minus, negate_, plus, pow, unaryFunc)
import List
import List.Extra as List
import Parser.Advanced as Parser exposing ((|.), (|=), Parser, Step(..), Token(..))
import Set


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


longerFirst : List String -> List String
longerFirst =
    List.sortBy (\s -> -(String.length s))


errorsToString : String -> List (Parser.DeadEnd Context Problem) -> String
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

                                                last :: init ->
                                                    String.join ", " (List.reverse init) ++ " or " ++ last
                                       )
                                ]
                        )
                    <|
                        List.gatherEqualsBy .col err
               )


parse : String -> Result (List (Parser.DeadEnd Context Problem)) Expression
parse input =
    let
        log msg =
            if String.contains "DEBUG" input then
                Debug.log msg

            else
                identity
    in
    input
        |> String.trim
        |> prepare
        |> Parser.run
            (Parser.succeed identity
                |= mainParser
                |. Parser.end Unexpected
            )
        |> Result.map
            (\r ->
                r
                    |> log "raw"
                    |> explode (longerFirst defaultContext)
                    |> log "after explode"
                    |> Expression.Utils.squash
                    |> log "after squash"
                    |> activateFunctions (longerFirst defaultContext)
                    |> log "after function activation"
                    |> Expression.Utils.squashHarder
            )


parseGraph : Expression -> Graph
parseGraph expr =
    case expr of
        RelationOperation rop l r ->
            case l of
                Variable "y" ->
                    case Set.toList <| getFreeVariables r of
                        [ "x" ] ->
                            Explicit2D r

                        _ ->
                            Implicit2D l rop r

                _ ->
                    Implicit2D l rop r

        _ ->
            Explicit2D expr


prepare : String -> String
prepare =
    String.replace "DEBUG" ""
        >> String.replace "²" "^2"
        >> String.replace "³" "^3"


activateFunctions : List String -> Expression -> Expression
activateFunctions context =
    visit <|
        \expr ->
            case expr of
                Replace vars e ->
                    Just <|
                        Replace (Dict.map (\_ -> activateFunctions context) vars) <|
                            activateFunctions
                                (longerFirst (context ++ Dict.keys (Dict.filter isFunction vars)))
                                e

                AssociativeOperation aop l r o ->
                    Just <|
                        let
                            al =
                                activateFunctions context l

                            ar =
                                activateFunctions context r

                            ao =
                                List.map (activateFunctions context) o
                        in
                        if aop == Multiplication then
                            let
                                ( last, net ) =
                                    case List.reverse (al :: ar :: ao) of
                                        [] ->
                                            -- Impossible
                                            ( al, [] )

                                        l_ :: init ->
                                            ( l_, init )

                                raw =
                                    (\( h, t ) -> h :: t) <|
                                        List.foldl
                                            (\e ( pending, rest ) ->
                                                case e of
                                                    Variable v ->
                                                        if List.member v context then
                                                            ( unaryFunc v pending, rest )

                                                        else
                                                            ( e, pending :: rest )

                                                    _ ->
                                                        ( e, pending :: rest )
                                            )
                                            ( last, [] )
                                            net
                            in
                            if 1 < List.length raw && List.length raw < 2 + List.length ao then
                                activateFunctions context <| by raw

                            else
                                by raw

                        else
                            AssociativeOperation aop al ar ao

                _ ->
                    Nothing


explode : List String -> Expression -> Expression
explode context =
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
    visit <|
        \expr ->
            case expr of
                Variable v ->
                    Just <|
                        if String.length v < 2 then
                            expr

                        else
                            by <| List.map Variable <| findPrefixes v

                Replace vars e ->
                    Just <|
                        Replace (Dict.map (\_ -> explode context) vars) <|
                            explode
                                ((context ++ Dict.keys vars)
                                    |> List.sortBy (\s -> -(String.length s))
                                )
                                e

                _ ->
                    Nothing


mainParser : ExpressionParser Expression
mainParser =
    relationParser


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


relationParser : ExpressionParser Expression
relationParser =
    Parser.inContext Expression <|
        Parser.succeed (\a f -> f a)
            |= addsubtractionParser
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
                    |= addsubtractionParser
                , Parser.succeed identity
                ]


addsubtractionParser : ExpressionParser Expression
addsubtractionParser =
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
    Parser.succeed (\atom maybePow -> maybePow atom)
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
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.succeed negate_
                |. Parser.symbol (token "-")
            , Parser.succeed identity
            ]
        |= data.item
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
            |. Parser.oneOf
                -- closed parens are optional
                [ Parser.symbol (token ")")
                , Parser.succeed ()
                ]
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
