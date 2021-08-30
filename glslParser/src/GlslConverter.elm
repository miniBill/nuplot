module GlslConverter exposing (main)

import Browser
import Element as Element exposing (Element, column, el, fill, height, padding, row, scrollbarY, spacing, text, width)
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
            """vec2 cmbrot(vec2 x, vec2 y) {
    vec2 c = x + vec2(-y.y, y.x);

    float p = length(c - vec2(0.25, 0));
    if(c.x <= p - 2.0*p*p + 0.25 || length(c + vec2(1,0)) <= 0.25)
        return vec2(0,0);

    vec2 z = c;
    for(int i = 0; i < 4000; i++) {
        z = vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y) + c;
        if(length(z) > 1000000.0) {
            float logLength = log(length(z));
            float nu = log(logLength / log(2.0)) / log(2.0);
            float fi = float(i) - nu;
            return vec2(sin(fi),cos(fi));
        }
    }
    return vec2(0,0);
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
        tc =
            Html.pre [] [ Html.text value ]
                |> Element.html
                |> el
                    [ width fill
                    , height fill
                    , Border.width 1
                    , Font.family [ Font.typeface "Fira Code", Font.monospace ]
                    , padding 10
                    , scrollbarY
                    ]
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
            prettyPrint o


type alias Function =
    { returnType : String
    , name : String
    , args : List ( String, String )
    , body : Statement
    }


type Statement
    = Expression Expression Statement
    | For String Expression RelationOperation Expression Statement Statement
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
    | Dot String String
    | Variable String
    | Call String (List Expression)
    | Ternary Expression Expression Expression
    | UnaryOperation UnaryOperation Expression
    | BinaryOperation BinaryOperation Expression Expression
    | BooleanOperation BooleanOperation Expression Expression
    | RelationOperation RelationOperation Expression Expression


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
    succeed Function
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
        |= statementParser


argParser : Parser ( String, String )
argParser =
    succeed Tuple.pair
        |= typeParser
        |. spaces
        |= identifierParser


identifierParser : Parser String
identifierParser =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> Char.isAlpha c || c == '_')
            |. chompWhile (\c -> Char.isAlphaNum c || c == '_')


typeParser : Parser String
typeParser =
    [ "void", "bool", "int", "float", "vec2", "vec3", "vec4", "mat3" ]
        |> List.map (\s -> succeed s |. symbol s)
        |> oneOf


statementParser : Parser Statement
statementParser =
    Parser.lazy <|
        \_ ->
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
        |. symbol "++"
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
    booleanParser


booleanParser : Parser Expression
booleanParser =
    multiSequence
        { separators =
            [ ( BooleanOperation Or, symbol "||" )
            , ( BooleanOperation And, symbol "&&" )
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
    oneOf
        [ succeed (\a f -> f a)
            |= identifierParser
            |= oneOf
                [ succeed (\sw v -> Dot v sw)
                    |. symbol "."
                    |= identifierParser
                , succeed (\args v -> Call v args)
                    |= sequence
                        { start = "("
                        , separator = ","
                        , item = Parser.lazy <| \_ -> expressionParser
                        , end = ")"
                        , trailing = Forbidden
                        , spaces = spaces
                        }
                , succeed Variable
                ]
        , Parser.andThen tryParseNumber <|
            getChompedString <|
                succeed ()
                    |. chompIf (\c -> c == '.' || Char.isDigit c)
                    |. chompWhile (\c -> c == '.' || Char.isDigit c)
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
    fun.name
        ++ "Couple = "
        ++ "fun"
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
                ++ "(\n"
                ++ prettyPrintStatement next
                ++ ")"

        For var from LessThan to loop next ->
            "for (\""
                ++ var
                ++ "\", "
                ++ prettyPrintExpression from
                ++ ", "
                ++ prettyPrintExpression to
                ++ ") (\\"
                ++ var
                ++ " ->\n"
                ++ prettyPrintStatement loop
                ++ ") ("
                ++ prettyPrintStatement next
                ++ ")"

        For var from LessThanOrEquals to loop next ->
            "forLeq ("
                ++ var
                ++ ", "
                ++ prettyPrintExpression from
                ++ ", "
                ++ prettyPrintExpression to
                ++ ") (\\"
                ++ var
                ++ " ->\n"
                ++ prettyPrintStatement loop
                ++ ") ("
                ++ prettyPrintStatement next
                ++ ")"

        If cond ifTrue next ->
            "if_ "
                ++ prettyPrintExpression cond
                ++ "(\n"
                ++ prettyPrintStatement ifTrue
                ++ ") ("
                ++ prettyPrintStatement next
                ++ ")"

        Return e ->
            "(return " ++ prettyPrintExpression e ++ ")\n"

        Nop ->
            "unsafeNop\n"

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

        For _ _ _ _ _ _ ->
            "TODO branch 'For _ _ _ _ _ _' not implemented"


prettyPrintExpression : Expression -> String
prettyPrintExpression e =
    let
        lisp args =
            "(" ++ String.join " " args ++ ")"
    in
    case e of
        BinaryOperation op l r ->
            lisp [ prettyPrintBinaryOperation op, prettyPrintExpression l, prettyPrintExpression r ]

        Float f ->
            lisp [ "float", String.fromFloat f ]

        Int i ->
            lisp [ "int", String.fromInt i ]

        Dot v sw ->
            v ++ "." ++ sw

        Variable v ->
            v

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
                                        Int 0 ->
                                            "zero"

                                        Int 1 ->
                                            "one"

                                        Int i ->
                                            "(float " ++ String.fromInt i ++ ")"

                                        _ ->
                                            prettyPrintExpression c
                                )
                                args

            else if List.member n [ "sin", "cos" ] then
                lisp <| (n ++ "_") :: List.map prettyPrintExpression args

            else
                lisp <| n :: List.map prettyPrintExpression args

        Ternary _ _ _ ->
            "branch 'Ternary _ _ _' not implemented"

        UnaryOperation Negate ex ->
            lisp [ "negate_", prettyPrintExpression ex ]

        BooleanOperation bop l r ->
            lisp [ prettyPrintBooleanOperation bop, prettyPrintExpression l, prettyPrintExpression r ]

        RelationOperation rop l r ->
            lisp [ prettyPrintRelationOperation rop, prettyPrintExpression l, prettyPrintExpression r ]


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
            "and"

        Or ->
            "or"


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
