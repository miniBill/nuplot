module GlslConverter exposing (main)

import Browser
import Element as Element exposing (Element, column, el, fill, height, padding, paragraph, row, scrollbars, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..), Trailing(..), chompIf, chompWhile, getChompedString, loop, oneOf, problem, sequence, spaces, succeed, symbol)


type alias Model =
    { input : String
    , output : String
    }


type Msg
    = Input String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = Element.layout [ width fill, height fill ] << view
        , update = update
        }


init : Model
init =
    let
        i =
            """{



}"""
    in
    { input = i
    , output = toOutput i
    }


view : Model -> Element Msg
view model =
    row
        [ padding 10
        , spacing 10
        , width fill
        , height fill
        ]
        [ input model.input
        , output model.output
        ]


input : String -> Element Msg
input value =
    Input.multiline
        [ width fill
        , height fill
        , spacing 10
        ]
        { onChange = Input
        , text = value
        , placeholder = Nothing
        , label = Input.labelAbove [] <| text "Input (GLSL function or expression)"
        , spellcheck = False
        }


output : String -> Element Msg
output value =
    let
        bordering =
            el
                [ width fill
                , height fill
                , Border.width 1
                , Font.family [ Font.typeface "Fira Code", Font.monospace ]
                , padding 10
                , scrollbars
                ]
    in
    if String.contains "problem" value then
        paragraph [] [ text value ]
            |> bordering

    else
        let
            tc =
                Html.pre [] [ Html.text value ]
                    |> Element.html
                    |> bordering
        in
        column
            [ spacing 10
            , width fill
            , height fill
            ]
            [ text "Output (Elm code)"
            , tc
            ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input i ->
            { model | input = i, output = toOutput i }


toOutput : String -> String
toOutput i =
    case parse i of
        Err e ->
            Debug.toString e

        Ok o ->
            prettyPrint <| simplifyEither o


simplifyEither : Either Function Statement -> Either Function Statement
simplifyEither o =
    case o of
        Left f ->
            Left <| simplifyFunction f

        Right s ->
            Right <| simplifyStatement s


simplifyFunction : Function -> Function
simplifyFunction f =
    { f | body = simplifyStatement f.body }


simplifyStatement : Statement -> Statement
simplifyStatement s =
    case s of
        Nop ->
            Nop

        Expression e n ->
            Expression (simplify e) (simplifyStatement n)

        For v from rel to step inner next ->
            For v (simplify from) rel (simplify to) step (simplifyStatement inner) (simplifyStatement next)

        If cond t f ->
            If (simplify cond) (simplifyStatement t) (simplifyStatement f)

        Return e ->
            Return (simplify e)

        Def t n v next ->
            Def t n (simplify v) (simplifyStatement next)

        Decl t n next ->
            Decl t n (simplifyStatement next)


simplify : Expression -> Expression
simplify e =
    case e of
        BooleanOperation op es ->
            es
                |> List.map simplify
                |> List.concatMap
                    (\c ->
                        case c of
                            BooleanOperation cop ces ->
                                if op == cop then
                                    ces

                                else
                                    [ c ]

                            _ ->
                                [ c ]
                    )
                |> BooleanOperation op

        _ ->
            e


type alias Function =
    { returnType : String
    , name : String
    , args : List ( String, String )
    , body : Statement
    , hasSuffix : Bool
    }


type Statement
    = Expression Expression Statement
    | For String Expression RelationOperation Expression Bool Statement Statement
    | If Expression Statement Statement
    | Return Expression
    | Def String String Expression Statement
    | Decl String String Statement
    | Nop


type RelationOperation
    = LessThanOrEquals
    | LessThan
    | Equals
    | Assign
    | NotEquals
    | GreaterThanOrEquals
    | GreaterThan


type BinaryOperation
    = Add
    | Subtract
    | By
    | Div


type BooleanOperation
    = And
    | Or


type UnaryOperation
    = Negate


type Expression
    = Float Float
    | Int Int
    | Bool Bool
    | Dot Expression String
    | Variable String
    | Constant String
    | Call String (List Expression)
    | Arr Expression Expression
    | Ternary Expression Expression Expression
    | UnaryOperation UnaryOperation Expression
    | BinaryOperation BinaryOperation Expression Expression
    | BooleanOperation BooleanOperation (List Expression)
    | RelationOperation RelationOperation Expression Expression
    | PostfixIncrement Expression
    | PostfixDecrement Expression


type Either a b
    = Left a
    | Right b


parse : String -> Result (List DeadEnd) (Either Function Statement)
parse =
    Parser.run <|
        succeed identity
            |. spaces
            |= oneOf
                [ Parser.map Left functionParser
                , Parser.map Right statementParser
                ]
            |. spaces
            |. Parser.end


functionParser : Parser Function
functionParser =
    succeed (\returnType { name, hasSuffix } args body -> { returnType = returnType, name = name, hasSuffix = hasSuffix, args = args, body = body })
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
        |= statementParser


argParser : Parser ( String, String )
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


typeParser : Parser String
typeParser =
    [ "void", "bool", "int", "float", "vec2", "vec3", "vec4", "mat3" ]
        |> List.map (\s -> succeed s |. symbol s)
        |> oneOf


statementParser : Parser Statement
statementParser =
    Parser.lazy <| \_ ->
    oneOf
        [ blockParser
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
        |= statementParser
        |. spaces
        |= maybeStatementParser


maybeStatementParser : Parser Statement
maybeStatementParser =
    oneOf [ statementParser, succeed Nop ]


forParser : Parser Statement
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
        |= oneOf [ succeed True |. symbol "++", succeed False |. symbol "--" ]
        |. spaces
        |. symbol ")"
        |. spaces
        |= statementParser
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
    succeed Def
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


prettyPrint : Either Function Statement -> String
prettyPrint e =
    case e of
        Left f ->
            prettyPrintFunction f

        Right s ->
            prettyPrintStatement s


prettyPrintFunction : Function -> String
prettyPrintFunction fun =
    if fun.hasSuffix then
        "("
            ++ fun.name
            ++ "Decl, "
            ++ fun.name
            ++ ") = fun"
            ++ String.fromInt (List.length fun.args)
            ++ " "
            ++ fun.returnType
            ++ "T "
            ++ "(\""
            ++ fun.name
            ++ "\" ++ suffix)"
            ++ " "
            ++ String.join " " (List.map (\( t, n ) -> "(" ++ t ++ "T \"" ++ n ++ "\")") fun.args)
            ++ " <| \\"
            ++ String.join " " (List.map Tuple.second fun.args)
            ++ " -> "
            ++ (prettyPrintStatement fun.body |> String.split "\n" |> String.join " ")

    else
        fun.name
            ++ "Couple = fun"
            ++ String.fromInt (List.length fun.args)
            ++ " "
            ++ fun.returnType
            ++ "T \""
            ++ fun.name
            ++ "\" "
            ++ String.join " " (List.map (\( t, n ) -> "(" ++ t ++ "T \"" ++ n ++ "\")") fun.args)
            ++ " <| \\"
            ++ String.join " " (List.map Tuple.second fun.args)
            ++ " ->\n    "
            ++ (prettyPrintStatement fun.body |> String.split "\n" |> String.join "\n    ")
            ++ "\n\n"
            ++ fun.name
            ++ "Decl = Tuple.first "
            ++ fun.name
            ++ "Couple\n"
            ++ fun.name
            ++ " = Tuple.second "
            ++ fun.name
            ++ "Couple"


prettyPrintStatement : Statement -> String
prettyPrintStatement stat =
    case stat of
        Expression e next ->
            "expr "
                ++ prettyPrintExpression e
                ++ " <| \\_ ->\n"
                ++ prettyPrintStatement next

        For var from LessThan to step loop next ->
            (if step then
                "for (\""

             else
                "forDown (\""
            )
                ++ var
                ++ "\", "
                ++ prettyPrintExpression from
                ++ ", "
                ++ prettyPrintExpression to
                ++ ") (\\"
                ++ var
                ++ " ->\n"
                ++ prettyPrintStatement loop
                ++ ") <| \\_ ->\n"
                ++ prettyPrintStatement next

        For var from LessThanOrEquals to step loop next ->
            (if step then
                "forLeq (\""

             else
                "forLeqDown (\""
            )
                ++ var
                ++ ", "
                ++ prettyPrintExpression from
                ++ ", "
                ++ prettyPrintExpression to
                ++ ") (\\"
                ++ var
                ++ " ->\n"
                ++ prettyPrintStatement loop
                ++ ") <| \\_ ->\n"
                ++ prettyPrintStatement next


        For var from GreaterThan to step loop next ->
            (if step then
                "for (\""

             else
                "forDown (\""
            )
                ++ var
                ++ "\", "
                ++ prettyPrintExpression from
                ++ ", "
                ++ prettyPrintExpression to
                ++ ") (\\"
                ++ var
                ++ " ->\n"
                ++ prettyPrintStatement loop
                ++ ") <| \\_ ->\n"
                ++ prettyPrintStatement next

        For var from GreaterThanOrEquals to step loop next ->
            (if step then
                "forLeq (\""

             else
                "forLeqDown (\""
            )
                ++ var
                ++ ", "
                ++ prettyPrintExpression from
                ++ ", "
                ++ prettyPrintExpression to
                ++ ") (\\"
                ++ var
                ++ " ->\n"
                ++ prettyPrintStatement loop
                ++ ") <| \\_ ->\n"
                ++ prettyPrintStatement next

        If cond ifTrue next ->
            "if_ "
                ++ prettyPrintExpression cond
                ++ "(\n"
                ++ prettyPrintStatement ifTrue
                ++ ") <| \\_ ->\n"
                ++ prettyPrintStatement next

        Return e ->
            "(return <| " ++ prettyPrintExpression e ++ ")\n"

        Nop ->
            "nop\n"

        Def t n v next ->
            "def "
                ++ t
                ++ "T \""
                ++ n
                ++ "\" ("
                ++ prettyPrintExpression v
                ++ ") <| \\"
                ++ n
                ++ " -> \n"
                ++ prettyPrintStatement next

        Decl _ _ _ ->
            "TODO branch 'Decl _ _ _' not implemented"
        
        For _ _ _ _ _ _ _ ->
            "TODO branch 'For _ _ _ _ _ _ _' not implemented"


prettyPrintExpression : Expression -> String
prettyPrintExpression e =
    let
        lisp args =
            "(" ++ String.join " " args ++ ")"
    in
    case e of
        Float f ->
            if f == 0 then
                "zero"

            else if f == 1 then
                "one"

            else
                lisp [ "float", String.fromFloat f ]

        Int i ->
            lisp [ "int", String.fromInt i ]

        Bool b ->
            lisp
                [ "bool"
                , if b then
                    "True"

                  else
                    "False"
                ]

        Dot v sw ->
            prettyPrintExpression v ++ "." ++ sw

        Variable v ->
            v

        Constant c ->
            case String.split "_" c of
                [] ->
                    "constants.???"

                h :: t ->
                    let
                        toTitle s =
                            case String.uncons s of
                                Nothing ->
                                    ""

                                Just ( sh, st ) ->
                                    String.cons (Char.toUpper sh) (String.toLower st)
                    in
                    "constants." ++ String.toLower h ++ String.concat (List.map toTitle t)

        Call "float" [ arg ] ->
            lisp [ "floatCast", prettyPrintExpression arg ]

        Call n args ->
            if String.startsWith "vec" n then
                if List.all ((==) (Int 0)) args then
                    n ++ "Zero"

                else
                    lisp <|
                        n
                            :: List.map
                                (\c ->
                                    case c of
                                        Int i ->
                                            prettyPrintExpression (Float <| toFloat i)

                                        _ ->
                                            prettyPrintExpression c
                                )
                                args

            else if n == "atan" then
                if List.length args == 2 then
                    lisp <| "atan2_" :: List.map prettyPrintExpression args

                else
                    lisp <| "atan_" :: List.map prettyPrintExpression args

            else if List.member n [ "sin", "cos", "radians", "round", "floor", "ceil", "sqrt" ] then
                lisp <| (n ++ "_") :: List.map prettyPrintExpression args

            else
                lisp <| n :: List.map prettyPrintExpression args

        Arr l r ->
            lisp [ "arr", prettyPrintExpression l, prettyPrintExpression r ]

        Ternary c l r ->
            lisp [ "ternary\n", prettyPrintExpression c, prettyPrintExpression l, prettyPrintExpression r ]

        UnaryOperation Negate ex ->
            lisp [ "negate_", prettyPrintExpression ex ]

        BinaryOperation op l r ->
            lisp [ prettyPrintBinaryOperation op, prettyPrintExpression l, prettyPrintExpression r ]

        BooleanOperation op es ->
            lisp
                [ prettyPrintBooleanOperation op
                , "["
                , String.join ", " <| List.map prettyPrintExpression es
                , "]"
                ]

        RelationOperation rop l r ->
            lisp [ prettyPrintRelationOperation rop, prettyPrintExpression l, prettyPrintExpression r ]

        PostfixIncrement c ->
            lisp [ "postfixIncrement", prettyPrintExpression c ]

        PostfixDecrement c ->
            lisp [ "postfixDecrement", prettyPrintExpression c ]


prettyPrintRelationOperation : RelationOperation -> String
prettyPrintRelationOperation op =
    case op of
        LessThan ->
            "lt"

        LessThanOrEquals ->
            "leq"

        Equals ->
            "eq"

        Assign ->
            "assign"

        NotEquals ->
            "neq"

        GreaterThanOrEquals ->
            "geq"

        GreaterThan ->
            "gt"


prettyPrintBooleanOperation : BooleanOperation -> String
prettyPrintBooleanOperation op =
    case op of
        And ->
            "ands"

        Or ->
            "ors"


prettyPrintBinaryOperation : BinaryOperation -> String
prettyPrintBinaryOperation op =
    case op of
        Add ->
            "add"

        Subtract ->
            "subtract"

        By ->
            "by"

        Div ->
            "div"
