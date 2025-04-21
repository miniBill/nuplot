module Glsl.Parser exposing (expression, file, function, preprocess, statement)

import Glsl exposing (BinaryOperation(..), Declaration(..), Expr(..), RelationOperation(..), Stat(..), Type(..), UnaryOperation(..))
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), chompIf, chompWhile, getChompedString, keyword, loop, oneOf, sequence, succeed, symbol)


file : Parser ( Maybe { version : String }, List Declaration )
file =
    succeed Tuple.pair
        |= oneOf
            [ succeed (\version -> Just { version = version })
                |. symbol "#version "
                |= (chompWhile Char.isDigit
                        |> getChompedString
                   )
            , succeed Nothing
            ]
        |. spaces
        |= sequence
            { start = ""
            , separator = ""
            , item =
                oneOf
                    [ const
                    , uniform
                    , function
                    ]
            , end = ""
            , trailing = Parser.Optional
            , spaces = spaces
            }
        |. Parser.end


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
        |. spaces
        |= identifierParser
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
        |. spaces
        |= typeParser
        |. spaces
        |= identifierParser
        |. spaces
        |. symbol "="
        |. spaces
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
        |. oneOf [ keyword "const" |. spaces, succeed () ]
        |= oneOf
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
    Parser.lazy <| \_ ->
    oneOf
        [ blockParser
        , returnParser
        , breakContinueParser
        , ifParser
        , forParser
        , defParser
        , expressionStatementParser
        ]


breakContinueParser : Parser Stat
breakContinueParser =
    oneOf
        [ succeed Break
            |. keyword "break"
        , succeed Continue
            |. keyword "continue"
        ]
        |. spaces
        |. symbol ";"


expressionStatementParser : Parser Stat
expressionStatementParser =
    succeed ExpressionStatement
        |= expression
        |. spaces
        |. symbol ";"


ifParser : Parser Stat
ifParser =
    succeed (\e s k -> k e s)
        |. keyword "if"
        |. spaces
        |. symbol "("
        |. spaces
        |= expression
        |. spaces
        |. symbol ")"
        |. spaces
        |= statement
        |. spaces
        |= oneOf
            [ succeed (\b e s -> IfElse e s b)
                |. keyword "else"
                |. spaces
                |= statement
            , succeed If
            ]


forParser : Parser Stat
forParser =
    succeed For
        |. keyword "for"
        |. spaces
        |. symbol "("
        |. spaces
        |= oneOf
            [ succeed Just |= statement
            , succeed Nothing
            ]
        |. spaces
        |. symbol ";"
        |. spaces
        |= expression
        |. spaces
        |. symbol ";"
        |. spaces
        |= expression
        |. spaces
        |. symbol ")"
        |. spaces
        |= statement


returnParser : Parser Stat
returnParser =
    succeed Return
        |. keyword "return"
        |. spaces
        |= expression
        |. spaces
        |. symbol ";"


blockParser : Parser Stat
blockParser =
    sequence
        { start = "{"
        , item = statement
        , separator = ""
        , spaces = spaces
        , end = "}"
        , trailing = Parser.Optional
        }
        |> Parser.map Glsl.block


defParser : Parser Stat
defParser =
    succeed
        (\type_ var val ->
            Decl type_ var val
        )
        |= typeParser
        |. spaces
        |= identifierParser
        |. spaces
        |= oneOf
            [ succeed Just
                |. symbol "="
                |. spaces
                |= expression
                |. spaces
            , succeed Nothing
            ]
        |. spaces
        |. symbol ";"


expression : Parser Expr
expression =
    prec17Parser


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
        |. spaces
        |= oneOf
            [ succeed (\r l -> BinaryOperation l Assign r)
                |. singleSymbol "="
                |. spaces
                |= Parser.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboAdd r)
                |. symbol "+="
                |. spaces
                |= Parser.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboSubtract r)
                |. symbol "-="
                |. spaces
                |= Parser.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBy r)
                |. symbol "*="
                |. spaces
                |= Parser.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboDiv r)
                |. symbol "/="
                |. spaces
                |= Parser.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboMod r)
                |. symbol "%="
                |. spaces
                |= Parser.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboLeftShift r)
                |. symbol "<<="
                |. spaces
                |= Parser.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboRightShift r)
                |. symbol ">>="
                |. spaces
                |= Parser.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBitwiseAnd r)
                |. symbol "&="
                |. spaces
                |= Parser.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBitwiseOr r)
                |. symbol "|="
                |. spaces
                |= Parser.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBitwiseXor r)
                |. symbol "^="
                |. spaces
                |= Parser.lazy (\_ -> prec16Parser)
            , succeed identity
            ]


prec15Parser : Parser Expr
prec15Parser =
    succeed (\k f -> f k)
        |= prec14Parser
        |. spaces
        |= oneOf
            [ succeed (\t f c -> Ternary c t f)
                -- c is passed in last in the lambda because it's passed
                -- from above
                |. symbol "?"
                |. spaces
                |= prec14Parser
                |. spaces
                |. symbol ":"
                |. spaces
                |= Parser.lazy (\_ -> prec15Parser)
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
        |. Parser.backtrackable (symbol s)
        |. oneOf
            [ succeed ()
                |. oneOf (List.map symbol nots)
                |. Parser.problem ("Expecting " ++ s ++ " not follwed by any of " ++ String.join ", " nots)
                |> Parser.backtrackable
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
            |= Parser.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation Plus)
            |. singleSymbol "+"
            |= Parser.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation PrefixDecrement)
            |. symbol "--"
            |= Parser.lazy (\_ -> prec3Parser)
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
            |= Parser.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation Invert)
            |. symbol "~"
            |= Parser.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation Not)
            |. symbol "!"
            |= Parser.lazy (\_ -> prec3Parser)
        , prec2Parser
        ]


prec2Parser : Parser Expr
prec2Parser =
    succeed (\a f -> f a)
        |= prec1Parser
        |. spaces
        |= Parser.lazy prec2Suffixes


prec2Suffixes : () -> Parser (Expr -> Expr)
prec2Suffixes () =
    oneOf
        [ succeed (\args k v -> k (Call v args))
            |= sequence
                { start = "("
                , separator = ","
                , item = Parser.lazy <| \_ -> prec16Parser
                , end = ")"
                , trailing = Forbidden
                , spaces = spaces
                }
            |= oneOf
                [ Parser.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\arg k v -> k (BinaryOperation v ArraySubscript arg))
            |. symbol "["
            |. spaces
            |= Parser.lazy (\_ -> prec16Parser)
            |. spaces
            |. symbol "]"
            |= oneOf
                [ Parser.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\p k v -> k (Dot v p))
            |. symbol "."
            |= identifierParser
            |= oneOf
                [ Parser.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\k v -> k (UnaryOperation PostfixIncrement v))
            |. symbol "++"
            |= oneOf
                [ Parser.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\k v -> k (UnaryOperation PostfixDecrement v))
            |. symbol "--"
            |= oneOf
                [ Parser.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed identity
        ]


prec1Parser : Parser Expr
prec1Parser =
    oneOf
        [ succeed identity
            |. symbol "("
            |. spaces
            |= Parser.lazy (\_ -> expression)
            |. spaces
            |. symbol ")"
        , succeed (Bool True)
            |. keyword "true"
        , succeed (Bool False)
            |. keyword "false"
        , succeed Variable
            |= identifierParser
        , succeed Float |= floatParser
        , succeed Int |= intParser
        ]


floatParser : Parser Float
floatParser =
    oneOf
        [ succeed (\c e -> e c)
            |= (oneOf
                    [ succeed ()
                        |. symbol "."
                        |. intParser
                    , succeed ()
                        |. Parser.backtrackable intParser
                        |. symbol "."
                        |. Parser.commit ()
                        |. oneOf
                            [ intParser
                            , succeed 0
                            ]
                    ]
                    |> getChompedString
                    |> Parser.andThen
                        (\s ->
                            let
                                withZero : String
                                withZero =
                                    if String.endsWith "." s then
                                        s ++ "0"

                                    else
                                        s
                            in
                            case String.toFloat withZero of
                                Just f ->
                                    succeed f

                                Nothing ->
                                    Parser.problem ("Cannot parse \"" ++ s ++ "\" as float")
                        )
               )
            |= oneOf
                [ succeed (\e c -> c * 10 ^ toFloat e)
                    |. oneOf [ symbol "e", symbol "E" ]
                    |= oneOf
                        [ succeed identity
                            |. symbol "+"
                            |= intParser
                        , succeed negate
                            |. symbol "-"
                            |= intParser
                        , intParser
                        ]
                , succeed identity
                ]
        , succeed (\c e -> toFloat c * 10 ^ toFloat e)
            |= Parser.backtrackable intParser
            |. oneOf [ symbol "e", symbol "E" ]
            |. Parser.commit ()
            |= oneOf
                [ succeed identity
                    |. symbol "+"
                    |= intParser
                , succeed negate
                    |. symbol "-"
                    |= intParser
                , intParser
                ]
        ]


intParser : Parser Int
intParser =
    succeed ()
        |. chompIf Char.isDigit
        |. chompWhile Char.isDigit
        |> getChompedString
        |> Parser.andThen
            (\s ->
                case String.toInt s of
                    Just i ->
                        succeed i

                    Nothing ->
                        Parser.problem ("Cannot parse \"" ++ s ++ "\" as int")
            )


type alias SequenceData =
    { separators : List ( Expr -> Expr -> Expr, Parser () )
    , item : Parser Expr
    }


multiSequenceAssocLeft : SequenceData -> Parser Expr
multiSequenceAssocLeft data =
    succeed identity
        |= data.item
        |. spaces
        |> Parser.andThen
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
                            |. spaces
                            |= item
                            |. spaces
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


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')
