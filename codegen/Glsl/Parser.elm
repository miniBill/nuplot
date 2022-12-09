module Glsl.Parser exposing (file, function, statement)

import Glsl.Types exposing (BinaryOperation(..), BooleanOperation(..), Expression(..), ForDirection(..), Function, RelationOperation(..), Statement(..), Type(..), UnaryOperation(..))
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), chompIf, chompWhile, getChompedString, loop, oneOf, problem, sequence, spaces, succeed, symbol)
import Parser.Workaround


function : Parser Function
function =
    succeed
        (\glsl begin returnType { name, hasSuffix } args stat end ->
            { returnType = returnType
            , name = name
            , hasSuffix = hasSuffix
            , args = args
            , stat = stat
            , body = String.slice begin end glsl
            }
        )
        |= Parser.getSource
        |= Parser.getOffset
        |= typeParser
        |. spaces
        |= identifierWithSuffixParser
        |. spaces
        |= sequence
            { start = "("
            , end = ")"
            , spaces = spaces
            , separator = ","
            , item = argParser
            , trailing = Forbidden
            }
        |. spaces
        |= statement
        |= Parser.getOffset


argParser : Parser ( Type, String )
argParser =
    succeed Tuple.pair
        |= typeParser
        |. spaces
        |= identifierParser


identifierWithSuffixParser : Parser { name : String, hasSuffix : Bool }
identifierWithSuffixParser =
    succeed (\name hasSuffix -> { name = name, hasSuffix = hasSuffix })
        |= identifierParser
        |= oneOf
            [ succeed True |. symbol "\"\"\" ++ suffix ++ \"\"\""
            , succeed False
            ]


identifierParser : Parser String
identifierParser =
    getChompedString
        (succeed ()
            |. chompIf (\c -> Char.isAlpha c || c == '_')
            |. chompWhile (\c -> Char.isAlphaNum c || c == '_')
        )


typeParser : Parser Type
typeParser =
    [ ( "void", TVoid )
    , ( "bool", TBool )
    , ( "float", TFloat )
    , ( "int", TInt )
    , ( "vec2", TVec2 )
    , ( "ivec2", TIVec2 )
    , ( "vec3", TVec3 )
    , ( "vec4", TVec4 )
    , ( "mat3", TMat3 )
    ]
        |> List.map (\( s, t ) -> succeed t |. symbol s)
        |> oneOf


statement : Parser Statement
statement =
    Parser.lazy <|
        \_ ->
            oneOf
                [ commentParser
                , blockParser
                , returnParser
                , ifParser
                , forParser
                , defParser
                , expressionStatementParser
                ]


expressionStatementParser : Parser Statement
expressionStatementParser =
    Parser.succeed Expression
        |= expressionParser
        |. spaces
        |. symbol ";"
        |. spaces
        |= maybeStatementParser


ifParser : Parser Statement
ifParser =
    Parser.succeed If
        |. symbol "if"
        |. spaces
        |. symbol "("
        |. spaces
        |= expressionParser
        |. spaces
        |. symbol ")"
        |. spaces
        |= statement
        |. spaces
        |= maybeStatementParser


maybeStatementParser : Parser Statement
maybeStatementParser =
    oneOf [ statement, succeed Nop ]


forParser : Parser Statement
forParser =
    Parser.succeed
        (\var from op to direction step ->
            For
                { var = var
                , from = from
                , op = op
                , to = to
                , direction = direction
                , step = step
                }
        )
        |. symbol "for"
        |. spaces
        |. symbol "("
        |. spaces
        |. symbol "int"
        |. spaces
        |= identifierParser
        |. spaces
        |. symbol "="
        |. spaces
        |= expressionParser
        |. spaces
        |. symbol ";"
        |. spaces
        |. identifierParser
        |. spaces
        |= relationOperationParser
        |. spaces
        |= expressionParser
        |. spaces
        |. symbol ";"
        |. spaces
        |. identifierParser
        |. spaces
        |= oneOf
            [ succeed PlusPlus |. symbol "++"
            , succeed MinusMinus |. symbol "--"
            ]
        |. spaces
        |. symbol ")"
        |. spaces
        |= statement
        |. spaces
        |= maybeStatementParser


returnParser : Parser Statement
returnParser =
    Parser.succeed Return
        |. symbol "return"
        |. spaces
        |= expressionParser
        |. spaces
        |. symbol ";"


commentParser : Parser Statement
commentParser =
    Parser.succeed identity
        |. Parser.Workaround.lineCommentAfter "//"
        |. spaces
        |= maybeStatementParser


blockParser : Parser Statement
blockParser =
    Parser.succeed identity
        |. symbol "{"
        |. spaces
        |= maybeStatementParser
        |. spaces
        |. symbol "}"


defParser : Parser Statement
defParser =
    succeed
        (\type_ var val ->
            Def
                { type_ = type_
                , var = var
                , val = val
                }
        )
        |= typeParser
        |. spaces
        |= identifierParser
        |. spaces
        |. symbol "="
        |. spaces
        |= expressionParser
        |. spaces
        |. symbol ";"
        |. spaces
        |= maybeStatementParser


expressionParser : Parser Expression
expressionParser =
    ternaryParser


ternaryParser : Parser Expression
ternaryParser =
    Parser.succeed (\c f -> f c)
        |= booleanParser
        |. spaces
        |= oneOf
            [ succeed (\t f c -> Ternary c t f)
                |. symbol "?"
                |. spaces
                |= booleanParser
                |. spaces
                |. symbol ":"
                |. spaces
                |= Parser.lazy (\_ -> ternaryParser)
            , succeed identity
            ]


booleanParser : Parser Expression
booleanParser =
    multiSequence
        { separators =
            [ ( \l r -> BooleanOperation Or [ l, r ], symbol "||" )
            , ( \l r -> BooleanOperation And [ l, r ], symbol "&&" )
            ]
        , item = relationParser
        , allowNegation = False
        }


relationParser : Parser Expression
relationParser =
    let
        inner =
            succeed (\a f -> f a)
                |= addsubtractionParser
                |. spaces
                |= oneOf
                    [ succeed (\o r l -> succeed <| RelationOperation o l r)
                        |= relationOperationParser
                        |. spaces
                        |= addsubtractionParser
                    , succeed succeed
                    ]
    in
    Parser.andThen identity inner


relationOperationParser : Parser RelationOperation
relationOperationParser =
    oneOf
        [ succeed LessThanOrEquals |. symbol "<="
        , succeed GreaterThanOrEquals |. symbol ">="
        , succeed LessThan |. symbol "<"
        , succeed GreaterThan |. symbol ">"
        , succeed Equals |. symbol "=="
        , succeed NotEquals |. symbol "!="
        , succeed Assign |. symbol "="
        ]


addsubtractionParser : Parser Expression
addsubtractionParser =
    multiSequence
        { separators =
            [ ( BinaryOperation Add, symbol "+" )
            , ( BinaryOperation Subtract, symbol "-" )
            ]
        , item = multidivisionParser
        , allowNegation = False
        }


type alias SequenceData =
    { separators : List ( Expression -> Expression -> Expression, Parser () )
    , item : Parser Expression
    , allowNegation : Bool
    }


multiSequence : SequenceData -> Parser Expression
multiSequence data =
    succeed identity
        |= data.item
        |. spaces
        |> Parser.andThen
            (\first ->
                loop first (\expr -> multiSequenceHelp data expr)
            )


multiSequenceHelp :
    SequenceData
    -> Expression
    -> Parser (Step Expression Expression)
multiSequenceHelp { allowNegation, separators, item } acc =
    let
        separated =
            separators
                |> List.map
                    (\( f, parser ) ->
                        Parser.succeed (\mn e -> Loop <| f acc <| mn e)
                            |= (if allowNegation then
                                    Parser.oneOf
                                        [ Parser.succeed (UnaryOperation Negate)
                                            |. Parser.symbol "-"
                                        , Parser.succeed identity
                                        ]

                                else
                                    Parser.succeed identity
                               )
                            |. parser
                            |. spaces
                            |= item
                            |. spaces
                    )
                |> Parser.oneOf
    in
    Parser.oneOf
        [ separated
        , Parser.succeed (Done acc)
        ]


multidivisionParser : Parser Expression
multidivisionParser =
    multiSequence
        { separators =
            [ ( BinaryOperation By, symbol "*" )
            , ( BinaryOperation Div, symbol "/" )
            ]
        , item = unaryParser
        , allowNegation = False
        }


unaryParser : Parser Expression
unaryParser =
    Parser.oneOf
        [ succeed (UnaryOperation Negate)
            |. symbol "-"
            |= atomParser
        , atomParser
        ]


atomParser : Parser Expression
atomParser =
    succeed (\a f -> f a)
        |= oneOf
            [ succeed (\a f -> f a)
                |. symbol "("
                |. spaces
                |= Parser.lazy (\_ -> expressionParser)
                |. spaces
                |. symbol ")"
                |= maybeDot
            , succeed (Bool True) |. symbol "true"
            , succeed (Bool False) |. symbol "false"
            , succeed (\a f md -> md <| f a)
                |= Parser.map .name identifierWithSuffixParser
                |. spaces
                |= oneOf
                    [ succeed (\args v -> Call v args)
                        |= sequence
                            { start = "("
                            , separator = ","
                            , item = Parser.lazy <| \_ -> expressionParser
                            , end = ")"
                            , trailing = Forbidden
                            , spaces = spaces
                            }
                    , succeed (\arg v -> Arr (toVariableOrConstant v) arg)
                        |. symbol "["
                        |. spaces
                        |= Parser.lazy (\_ -> expressionParser)
                        |. spaces
                        |. symbol "]"
                    , succeed toVariableOrConstant
                    ]
                |= maybeDot
            , Parser.andThen tryParseNumber <|
                getChompedString <|
                    succeed ()
                        |. chompIf (\c -> c == '.' || Char.isDigit c)
                        |. chompWhile (\c -> c == '.' || Char.isDigit c)
            ]
        |. spaces
        |= oneOf
            [ succeed PostfixIncrement
                |. symbol "++"
            , succeed PostfixDecrement
                |. symbol "--"
            , succeed identity
            ]


toVariableOrConstant : String -> Expression
toVariableOrConstant s =
    if String.toUpper s == s then
        Constant s

    else
        Variable s


maybeDot : Parser (Expression -> Expression)
maybeDot =
    oneOf
        [ succeed (\p v -> Dot v p)
            |. symbol "."
            |= identifierParser
        , succeed identity
        ]


tryParseNumber : String -> Parser Expression
tryParseNumber n =
    case String.toInt n of
        Just i ->
            succeed <| Int i

        Nothing ->
            case String.toFloat n of
                Just f ->
                    succeed <| Float f

                Nothing ->
                    problem "Expected a number"


file : Parser (List Function)
file =
    Parser.succeed (List.filterMap identity)
        |= Parser.sequence
            { start = ""
            , separator = ""
            , item =
                Parser.oneOf
                    [ Parser.succeed Nothing
                        |. Parser.Workaround.lineCommentAfter "//"
                    , Parser.succeed Nothing
                        |. Parser.Workaround.lineCommentAfter "#define"
                    , Parser.succeed Nothing
                        |. Parser.Workaround.lineCommentAfter "precision highp"
                    , Parser.succeed Nothing
                        |. Parser.Workaround.lineCommentAfter "uniform"
                    , Parser.succeed Just
                        |= function
                    ]
            , end = ""
            , trailing = Parser.Optional
            , spaces = Parser.spaces
            }
        |. Parser.end
