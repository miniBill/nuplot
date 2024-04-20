module Glsl.Parser exposing (file, function, statement)

import Glsl exposing (BinaryOperation(..), Declaration(..), Expr(..), Expression(..), ForDirection(..), Function, RelationOperation(..), Stat(..), Statement(..), Type(..), UnaryOperation(..))
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), chompIf, chompWhile, getChompedString, keyword, loop, oneOf, problem, sequence, spaces, succeed, symbol)
import Parser.Workaround


function : Parser Declaration
function =
    succeed
        (\glsl begin returnType { name, hasSuffix } args stat end ->
            let
                res : Function
                res =
                    { returnType = returnType
                    , name = name
                    , hasSuffix = hasSuffix
                    , args = args
                    , stat = stat
                    , body = String.slice begin end glsl
                    }
            in
            FunctionDeclaration res
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


uniform : Parser Declaration
uniform =
    succeed
        (\tipe name ->
            UniformDeclaration
                { tipe = tipe
                , name = name
                }
        )
        |. symbol "uniform"
        |. spaces
        |= typeParser
        |. spaces
        |= identifierParser
        |. spaces
        |. symbol ";"


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
    let
        baseParser =
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
                |> List.map (\( s, t ) -> succeed t |. keyword s)
                |> oneOf
    in
    oneOf
        [ succeed TOut
            |. keyword "out"
            |. spaces
            |= baseParser
        , succeed TIn
            |. keyword "in"
            |. spaces
            |= baseParser
        , baseParser
        ]


statement : Parser Stat
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


expressionStatementParser : Parser Stat
expressionStatementParser =
    Parser.succeed ExpressionStatement
        |= expressionParser
        |. spaces
        |. symbol ";"
        |. spaces
        |= maybeStatementParser


ifParser : Parser Stat
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


maybeStatementParser : Parser Stat
maybeStatementParser =
    oneOf [ statement, succeed Nop ]


forParser : Parser Stat
forParser =
    Parser.succeed For
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


returnParser : Parser Stat
returnParser =
    Parser.succeed Return
        |. symbol "return"
        |. spaces
        |= expressionParser
        |. spaces
        |. symbol ";"


commentParser : Parser Stat
commentParser =
    Parser.succeed identity
        |. Parser.Workaround.lineCommentAfter "//"
        |. spaces
        |= maybeStatementParser


blockParser : Parser Stat
blockParser =
    Parser.succeed identity
        |. symbol "{"
        |. spaces
        |= maybeStatementParser
        |. spaces
        |. symbol "}"


defParser : Parser Stat
defParser =
    succeed
        (\type_ var val ->
            Decl type_ var (Just val)
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


expressionParser : Parser Expr
expressionParser =
    ternaryParser


ternaryParser : Parser Expr
ternaryParser =
    Parser.succeed (\k f -> f k)
        |= booleanParser
        |. spaces
        |= oneOf
            [ succeed (\t f c -> Ternary c t f)
                -- c is passed in last in the lambda because it's passed
                -- from above
                |. symbol "?"
                |. spaces
                |= booleanParser
                |. spaces
                |. symbol ":"
                |. spaces
                |= Parser.lazy (\_ -> ternaryParser)
            , succeed identity
            ]


booleanParser : Parser Expr
booleanParser =
    multiSequence
        { separators =
            [ ( BinaryOperation Or, symbol "||" )
            , ( BinaryOperation And, symbol "&&" )
            ]
        , item = relationParser
        , allowNegation = False
        }


relationParser : Parser Expr
relationParser =
    let
        inner =
            succeed (\a f -> f a)
                |= addsubtractionParser
                |. spaces
                |= oneOf
                    [ succeed (\o r l -> succeed <| Comparison o l r)
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


addsubtractionParser : Parser Expr
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
    { separators : List ( Expr -> Expr -> Expr, Parser () )
    , item : Parser Expr
    , allowNegation : Bool
    }


multiSequence : SequenceData -> Parser Expr
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
    -> Expr
    -> Parser (Step Expr Expr)
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


multidivisionParser : Parser Expr
multidivisionParser =
    multiSequence
        { separators =
            [ ( BinaryOperation By, symbol "*" )
            , ( BinaryOperation Div, symbol "/" )
            ]
        , item = unaryParser
        , allowNegation = False
        }


unaryParser : Parser Expr
unaryParser =
    Parser.oneOf
        [ succeed (UnaryOperation Negate)
            |. symbol "-"
            |= atomParser
        , atomParser
        ]


atomParser : Parser Expr
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
                    , succeed (\arg v -> Array (Variable v) arg)
                        |. symbol "["
                        |. spaces
                        |= Parser.lazy (\_ -> expressionParser)
                        |. spaces
                        |. symbol "]"
                    , succeed Variable
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


maybeDot : Parser (Expr -> Expr)
maybeDot =
    oneOf
        [ succeed (\p v -> Dot v p)
            |. symbol "."
            |= identifierParser
        , succeed identity
        ]


tryParseNumber : String -> Parser Expr
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


file : Parser (List Declaration)
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
                    , Parser.succeed Just
                        |= uniform
                    , Parser.succeed Just
                        |= function
                    ]
            , end = ""
            , trailing = Parser.Optional
            , spaces = Parser.spaces
            }
        |. Parser.end
