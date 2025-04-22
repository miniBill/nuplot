module Glsl.Parser exposing (Context(..), DeadEnd, Parser, expression, file, function, preprocess, statement)

import Glsl exposing (BinaryOperation(..), Declaration(..), Expr(..), RelationOperation(..), Stat(..), Type(..), UnaryOperation(..))
import Parser exposing (Problem(..))
import Parser.Advanced exposing ((|.), (|=), Step(..), Token(..), Trailing(..))
import ParserWithContext exposing (chompIf, chompWhile, end, float, getChompedString, inContext, int, keyword, loop, many, oneOf, sequence, spaces, succeed, symbol)


type Context
    = ParsingFile
    | ParsingFunction
    | ParsingStatement
    | ParsingExpression


type alias Parser a =
    ParserWithContext.Parser Context a


type alias DeadEnd =
    Parser.Advanced.DeadEnd Context Parser.Problem


file : Parser ( Maybe { version : Int }, List Declaration )
file =
    succeed Tuple.pair
        |. spaces
        |= oneOf
            [ succeed (\version -> Just { version = version })
                |. symbol "#version"
                |= int
            , succeed Nothing
            ]
        |= many
            (oneOf
                [ const
                , uniform
                , function
                ]
            )
        |. end
        |> inContext ParsingFile


function : Parser Declaration
function =
    succeed
        (\returnType name args stat ->
            FunctionDeclaration
                { returnType = returnType
                , name = name
                , args = args
                , stat = stat
                }
        )
        |= typeParser
        |= identifierParser
        |= sequence
            { start = "("
            , end = ")"
            , separator = ","
            , item = argParser
            , trailing = Forbidden
            }
        |= statement
        |> inContext ParsingFunction


const : Parser Declaration
const =
    succeed
        (\tipe name value ->
            ConstDeclaration
                { tipe = tipe
                , name = name
                , value = value
                }
        )
        |. keyword "const"
        |= typeParser
        |= identifierParser
        |. symbol "="
        |= expression
        |. symbol ";"


uniform : Parser Declaration
uniform =
    succeed
        (\tipe name ->
            UniformDeclaration
                { tipe = tipe
                , name = name
                }
        )
        |. keyword "uniform"
        |= typeParser
        |= identifierParser
        |. symbol ";"


argParser : Parser ( Type, String )
argParser =
    succeed Tuple.pair
        |= typeParser
        |= identifierParser


identifierParser : Parser String
identifierParser =
    getChompedString
        (succeed ()
            |. chompIf (\c -> Char.isAlpha c || c == '_') (Expecting "Letter or underscore")
            |. chompWhile (\c -> Char.isAlphaNum c || c == '_')
        )
        |. spaces


typeParser : Parser Type
typeParser =
    let
        baseParser : Parser Type
        baseParser =
            [ ( "void", TVoid )
            , ( "float", TFloat )
            , ( "int", TInt )
            , ( "bool", TBool )
            , ( "vec2", TVec2 )
            , ( "vec3", TVec3 )
            , ( "vec4", TVec4 )
            , ( "ivec2", TIVec2 )
            , ( "ivec3", TIVec3 )
            , ( "ivec4", TIVec4 )
            , ( "mat2", TMat2 )
            , ( "mat3", TMat3 )
            , ( "mat4", TMat4 )
            ]
                |> List.map (\( s, t ) -> succeed t |. keyword s)
                |> oneOf
    in
    succeed identity
        |. oneOf
            [ keyword "const"
            , succeed ()
            ]
        |= oneOf
            [ succeed TOut
                |. keyword "out"
                |= baseParser
            , succeed TIn
                |. keyword "in"
                |= baseParser
            , baseParser
            ]


statement : Parser Stat
statement =
    (ParserWithContext.lazy <|
        \_ ->
            oneOf
                [ blockParser
                , returnParser
                , breakContinueParser
                , ifParser
                , forParser
                , defParser
                , expressionStatementParser
                ]
    )
        |> inContext ParsingStatement


breakContinueParser : Parser Stat
breakContinueParser =
    oneOf
        [ succeed Break
            |. keyword "break"
        , succeed Continue
            |. keyword "continue"
        ]
        |. symbol ";"


expressionStatementParser : Parser Stat
expressionStatementParser =
    succeed ExpressionStatement
        |= expression
        |. symbol ";"


ifParser : Parser Stat
ifParser =
    succeed (\e s k -> k e s)
        |. keyword "if"
        |. symbol "("
        |= expression
        |. symbol ")"
        |= statement
        |= oneOf
            [ succeed (\b e s -> IfElse e s b)
                |. keyword "else"
                |= statement
            , succeed If
            ]


forParser : Parser Stat
forParser =
    succeed For
        |. keyword "for"
        |. symbol "("
        |= oneOf
            [ succeed Just |= statement
            , succeed Nothing
            ]
        |. symbol ";"
        |= expression
        |. symbol ";"
        |= expression
        |. symbol ")"
        |= statement


returnParser : Parser Stat
returnParser =
    succeed Return
        |. keyword "return"
        |= expression
        |. symbol ";"


blockParser : Parser Stat
blockParser =
    sequence
        { start = "{"
        , item = statement
        , separator = ""
        , end = "}"
        , trailing = Optional
        }
        |> ParserWithContext.map Glsl.block


defParser : Parser Stat
defParser =
    succeed
        (\type_ var val ->
            Decl type_ var val
        )
        |= typeParser
        |= identifierParser
        |= oneOf
            [ succeed Just
                |. symbol "="
                |= expression
            , succeed Nothing
            ]
        |. symbol ";"


expression : Parser Expr
expression =
    prec17Parser
        |> inContext ParsingExpression


prec17Parser : Parser Expr
prec17Parser =
    multiSequenceAssocLeft
        { separators = [ ( \l r -> BinaryOperation l Comma r, symbol "," ) ]
        , item = prec16Parser
        }


prec16Parser : Parser Expr
prec16Parser =
    succeed (\a f -> f a)
        |= prec15Parser
        |= oneOf
            [ succeed (\r l -> BinaryOperation l Assign r)
                |. singleSymbol "="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboAdd r)
                |. symbol "+="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboSubtract r)
                |. symbol "-="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBy r)
                |. symbol "*="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboDiv r)
                |. symbol "/="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboMod r)
                |. symbol "%="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboLeftShift r)
                |. symbol "<<="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboRightShift r)
                |. symbol ">>="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBitwiseAnd r)
                |. symbol "&="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBitwiseOr r)
                |. symbol "|="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBitwiseXor r)
                |. symbol "^="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed identity
            ]


prec15Parser : Parser Expr
prec15Parser =
    succeed (\k f -> f k)
        |= prec14Parser
        |= oneOf
            [ succeed (\t f c -> Ternary c t f)
                -- c is passed in last in the lambda because it's passed
                -- from above
                |. symbol "?"
                |= prec14Parser
                |. symbol ":"
                |= ParserWithContext.lazy (\_ -> prec15Parser)
            , succeed identity
            ]


prec14Parser : Parser Expr
prec14Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l Or r, symbol "||" )
            ]
        , item = prec13Parser
        }


prec13Parser : Parser Expr
prec13Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l Xor r, symbol "^^" )
            ]
        , item = prec12Parser
        }


prec12Parser : Parser Expr
prec12Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l And r, symbol "&&" )
            ]
        , item = prec11Parser
        }


prec11Parser : Parser Expr
prec11Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l BitwiseOr r, singleSymbol "|" )
            ]
        , item = prec10Parser
        }


prec10Parser : Parser Expr
prec10Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l BitwiseXor r, singleSymbol "^" )
            ]
        , item = prec9Parser
        }


prec9Parser : Parser Expr
prec9Parser =
    multiSequenceAssocLeft
        { separators = [ ( \l r -> BinaryOperation l BitwiseAnd r, singleSymbol "&" ) ]
        , item = prec8Parser
        }


singleSymbol : String -> Parser ()
singleSymbol s =
    symbolNotFollowedBy s [ s ]


symbolNotFollowedBy : String -> List String -> Parser ()
symbolNotFollowedBy s nots =
    succeed ()
        |. ParserWithContext.backtrackable (symbol s)
        |. oneOf
            [ succeed ()
                |. oneOf (List.map symbol nots)
                |. ParserWithContext.problem ("Expecting " ++ s ++ " not follwed by any of " ++ String.join ", " nots)
                |> ParserWithContext.backtrackable
            , succeed ()
            ]


prec8Parser : Parser Expr
prec8Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l (RelationOperation Equals) r, symbol "==" )
            , ( \l r -> BinaryOperation l (RelationOperation NotEquals) r, symbol "!=" )
            ]
        , item = prec7Parser
        }


prec7Parser : Parser Expr
prec7Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l (RelationOperation LessThanOrEquals) r, symbol "<=" )
            , ( \l r -> BinaryOperation l (RelationOperation GreaterThanOrEquals) r, symbol ">=" )
            , ( \l r -> BinaryOperation l (RelationOperation LessThan) r, symbolNotFollowedBy "<" [ "<=" ] )
            , ( \l r -> BinaryOperation l (RelationOperation GreaterThan) r, symbolNotFollowedBy ">" [ ">=" ] )
            ]
        , item = prec6Parser
        }


prec6Parser : Parser Expr
prec6Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l ShiftLeft r, symbolNotFollowedBy "<<" [ "=" ] )
            , ( \l r -> BinaryOperation l ShiftRight r, symbolNotFollowedBy ">>" [ "=" ] )
            ]
        , item = prec5Parser
        }


prec5Parser : Parser Expr
prec5Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l Add r, symbolNotFollowedBy "+" [ "=" ] )
            , ( \l r -> BinaryOperation l Subtract r, symbolNotFollowedBy "-" [ "=" ] )
            ]
        , item = prec4Parser
        }


prec4Parser : Parser Expr
prec4Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l By r, symbolNotFollowedBy "*" [ "=" ] )
            , ( \l r -> BinaryOperation l Div r, symbolNotFollowedBy "/" [ "=" ] )
            , ( \l r -> BinaryOperation l Mod r, symbolNotFollowedBy "%" [ "=" ] )
            ]
        , item = prec3Parser
        }


prec3Parser : Parser Expr
prec3Parser =
    oneOf
        [ succeed (UnaryOperation PrefixIncrement)
            |. symbol "++"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation Plus)
            |. singleSymbol "+"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation PrefixDecrement)
            |. symbol "--"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , succeed
            (\c ->
                case c of
                    Float f ->
                        Float -f

                    Int i ->
                        Int -i

                    _ ->
                        UnaryOperation Negate c
            )
            |. singleSymbol "-"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation Invert)
            |. symbol "~"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation Not)
            |. symbol "!"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , prec2Parser
        ]


prec2Parser : Parser Expr
prec2Parser =
    succeed (\a f -> f a)
        |= prec1Parser
        |= ParserWithContext.lazy prec2Suffixes


prec2Suffixes : () -> Parser (Expr -> Expr)
prec2Suffixes () =
    oneOf
        [ succeed (\args k v -> k (Call v args))
            |= sequence
                { start = "("
                , separator = ","
                , item = ParserWithContext.lazy <| \_ -> prec16Parser
                , end = ")"
                , trailing = Forbidden
                }
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\arg k v -> k (BinaryOperation v ArraySubscript arg))
            |. symbol "["
            |= ParserWithContext.lazy (\_ -> prec16Parser)
            |. symbol "]"
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\p k v -> k (Dot v p))
            |. symbol "."
            |= identifierParser
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\k v -> k (UnaryOperation PostfixIncrement v))
            |. symbol "++"
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\k v -> k (UnaryOperation PostfixDecrement v))
            |. symbol "--"
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed identity
        ]


prec1Parser : Parser Expr
prec1Parser =
    oneOf
        [ succeed identity
            |. symbol "("
            |= ParserWithContext.lazy (\_ -> expression)
            |. symbol ")"
        , succeed (Bool True)
            |. keyword "true"
        , succeed (Bool False)
            |. keyword "false"
        , succeed Variable
            |= identifierParser
        , succeed Float |= float
        , succeed Int |= int
        ]


type alias SequenceData =
    { separators : List ( Expr -> Expr -> Expr, Parser () )
    , item : Parser Expr
    }


multiSequenceAssocLeft : SequenceData -> Parser Expr
multiSequenceAssocLeft data =
    succeed identity
        |= data.item
        |> ParserWithContext.andThen
            (\first ->
                loop first (\expr -> multiSequenceHelpLeft data expr)
            )


multiSequenceHelpLeft :
    SequenceData
    -> Expr
    -> Parser (Step Expr Expr)
multiSequenceHelpLeft { separators, item } acc =
    let
        separated : Parser (Step Expr a)
        separated =
            separators
                |> List.map
                    (\( f, parser ) ->
                        succeed (\e -> Loop <| f acc e)
                            |. parser
                            |= item
                    )
                |> oneOf
    in
    oneOf
        [ separated
        , succeed (Done acc)
        ]


preprocess : String -> String
preprocess input =
    input
        |> String.split "\n"
        |> spliceLines
        |> removeComments
        |> expandMacros
        |> String.join "\n"


spliceLines : List String -> List String
spliceLines lines =
    lines
        |> List.foldl
            (\e ( last, acc ) ->
                case last of
                    Nothing ->
                        ( Just e, acc )

                    Just l ->
                        if String.endsWith "\\" l && not (String.endsWith "\\\\" l) then
                            ( Just (String.dropRight 1 l ++ e), acc )

                        else
                            ( Just e, l :: acc )
            )
            ( Nothing, [] )
        |> (\( last, acc ) ->
                case last of
                    Nothing ->
                        List.reverse acc

                    Just l ->
                        List.reverse (l :: acc)
           )


removeComments : List String -> List String
removeComments lines =
    lines
        |> List.map
            (\line ->
                case String.indexes "//" line of
                    head :: _ ->
                        String.left head line

                    [] ->
                        line
            )


expandMacros : List String -> List String
expandMacros lines =
    -- TODO: macros
    lines
