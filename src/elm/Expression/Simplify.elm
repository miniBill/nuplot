module Expression.Simplify exposing (simplify)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), RelationOperation(..), UnaryOperation(..), getFreeVariables)
import Expression.Utils exposing (by, cos_, div, i, icomplex, ipow, negate_, one, plus, pow, sin_, two, zero)
import List exposing (concatMap)
import List.Extra as List
import Maybe.Extra as Maybe
import Set


log : String -> a -> a
log =
    {- if False then
           Debug.log

       else
    -}
    always identity


simplify : Expression -> Expression
simplify =
    innerSimplify Dict.empty


{-| Preconditions: context contains simplified expressions
-}
innerSimplify : Dict String Expression -> Expression -> Expression
innerSimplify context expr =
    let
        _ =
            if True || Dict.isEmpty context then
                { context = ""
                , expr = log "innerSimplify" <| Expression.toString expr
                }

            else
                log "innerSimplify" <|
                    { context =
                        Dict.toList context
                            |> List.map (\( k, v ) -> k ++ " = " ++ Expression.toString v)
                            |> String.join "; "
                    , expr = Expression.toString expr
                    }
    in
    log ("innerSimplify " ++ Expression.toString expr) <|
        case expr of
            Variable v ->
                Dict.get v context |> Maybe.withDefault expr

            Replace vars e ->
                let
                    simplifiedVars =
                        Dict.map (\_ -> innerSimplify context) vars
                in
                innerSimplify simplifiedVars e

            UnaryOperation Negate (UnaryOperation Negate e) ->
                innerSimplify context e

            UnaryOperation Negate (Integer ni) ->
                Integer -ni

            UnaryOperation uop e ->
                UnaryOperation uop <| innerSimplify context e

            BinaryOperation bop l r ->
                let
                    ls =
                        innerSimplify context l

                    rs =
                        innerSimplify context r
                in
                case bop of
                    Division ->
                        innerSimplifyDivision context ls rs

                    Power ->
                        innerSimplifyPower context ls rs

            RelationOperation rop l r ->
                let
                    ls =
                        innerSimplify context l

                    rs =
                        innerSimplify context r
                in
                RelationOperation rop ls rs

            AssociativeOperation aop l r o ->
                innerSimplifyAssociative context aop l r o

            Apply name args ->
                innerSimplifyApply context name args

            List es ->
                List <| List.map (innerSimplify context) es

            Integer _ ->
                expr

            Float _ ->
                expr


innerSimplifyApply : Dict String Expression -> String -> List Expression -> Expression
innerSimplifyApply context name args =
    case ( name, List.map (innerSimplify context) args ) of
        ( "sqrt", [ Integer i ] as sargs ) ->
            let
                a =
                    abs i

                f =
                    toFloat a

                s =
                    sqrt f

                r =
                    truncate s
            in
            if r * r == a then
                if i < 0 then
                    icomplex 0 r

                else
                    Integer r

            else
                Apply name sargs

        ( "sinh", [ AssociativeOperation Multiplication l r o ] as sargs ) ->
            case extract (findSpecificVariable "i") (l :: r :: o) of
                Just ( _, rest ) ->
                    innerSimplify context <| by [ i, sin_ <| by rest ]

                Nothing ->
                    Apply name sargs

        ( "cosh", [ AssociativeOperation Multiplication l r o ] as sargs ) ->
            case extract (findSpecificVariable "i") (l :: r :: o) of
                Just ( _, rest ) ->
                    innerSimplify context <| cos_ <| by rest

                Nothing ->
                    Apply name sargs

        ( _, sargs ) ->
            Apply name sargs


innerSimplifyDivision : Dict String Expression -> Expression -> Expression -> Expression
innerSimplifyDivision context ls rs =
    case ( ls, rs ) of
        ( BinaryOperation Division lsn lsd, _ ) ->
            innerSimplify context <| div lsn <| by [ lsd, rs ]

        ( _, Variable "i" ) ->
            innerSimplify context <| by [ ls, negate_ i ]

        _ ->
            if Expression.equals ls rs then
                one

            else
                div ls rs


innerSimplifyPower : Dict String Expression -> Expression -> Expression -> Expression
innerSimplifyPower context ls rs =
    case ( ls, rs ) of
        ( AssociativeOperation Multiplication lm rm om, Integer rsi ) ->
            innerSimplify context <|
                AssociativeOperation Multiplication
                    (ipow lm rsi)
                    (ipow rm rsi)
                    (List.map (\b -> ipow b rsi) om)

        ( Integer il, Integer ir ) ->
            if ir < 0 then
                div one <| Integer <| il ^ -ir

            else
                Integer <| il ^ ir

        ( Variable "i", Integer ir ) ->
            case modBy 4 ir of
                0 ->
                    one

                1 ->
                    i

                2 ->
                    Integer -1

                -- 3
                _ ->
                    negate_ i

        _ ->
            pow ls rs


innerSimplifyAssociative : Dict String Expression -> AssociativeOperation -> Expression -> Expression -> List Expression -> Expression
innerSimplifyAssociative context aop l r o =
    let
        extractop e =
            case e of
                AssociativeOperation iaop il ir io ->
                    if iaop == aop then
                        -- We don't need to extract from il, ir, io because they're simplified
                        il :: ir :: io

                    else
                        [ e ]

                _ ->
                    [ e ]

        build =
            case aop of
                Addition ->
                    plus

                Multiplication ->
                    by
    in
    (l :: r :: o)
        |> List.map (innerSimplify context)
        |> List.concatMap extractop
        |> Trying
        |> try
            (\e ->
                if List.length e > 2 + List.length o then
                    Just e

                else
                    Nothing
            )
            build
        |> mapTrying (squashAndGroupAssociative context aop)
        |> (case aop of
                Addition ->
                    tryExtract (findSpecificInteger 0) (\( _, rest ) -> build rest)

                Multiplication ->
                    tryExtract (findSpecificInteger -1) (\( _, rest ) -> negate_ <| build rest)
                        >> tryExtract findNegate (\( negated, rest ) -> negate_ <| build <| negated :: rest)
                        >> tryExtract findAdditionList (\( adds, rest ) -> plus <| List.map (\x -> by <| x :: rest) adds)
           )
        |> mapFound (innerSimplify context)
        |> tryingToExpression build


type Trying a b
    = Trying a
    | Found b


mapTrying : (a -> c) -> Trying a b -> Trying c b
mapTrying f t =
    case t of
        Trying v ->
            Trying <| f v

        Found a ->
            Found a


mapFound : (b -> c) -> Trying a b -> Trying a c
mapFound f t =
    case t of
        Trying v ->
            Trying v

        Found a ->
            Found <| f a


tryingToExpression : (a -> b) -> Trying a b -> b
tryingToExpression onFailure t =
    case t of
        Trying e ->
            onFailure e

        Found e ->
            e


try : (a -> Maybe b) -> (b -> c) -> Trying a c -> Trying a c
try test toResult incoming =
    case incoming of
        Found _ ->
            incoming

        Trying e ->
            case test e of
                Just r ->
                    Found <| toResult r

                Nothing ->
                    incoming


tryExtract : (a -> Maybe b) -> (( b, List a ) -> c) -> Trying (List a) c -> Trying (List a) c
tryExtract f =
    try <| extract f


extract : (a -> Maybe b) -> List a -> Maybe ( b, List a )
extract filter ls =
    let
        go old incoming =
            case incoming of
                [] ->
                    Nothing

                h :: t ->
                    case filter h of
                        Nothing ->
                            go (h :: old) t

                        Just e ->
                            Just ( e, List.reverse old ++ t )
    in
    go [] ls


findSpecificInteger : Int -> Expression -> Maybe Int
findSpecificInteger j expr =
    case expr of
        Integer i ->
            if i == j then
                Just i

            else
                Nothing

        _ ->
            Nothing


findSpecificVariable : String -> Expression -> Maybe String
findSpecificVariable j expr =
    case expr of
        Variable i ->
            if i == j then
                Just i

            else
                Nothing

        _ ->
            Nothing


findAddition : Expression -> Maybe ( Expression, Expression, List Expression )
findAddition expr =
    case expr of
        AssociativeOperation Addition l r o ->
            Just ( l, r, o )

        _ ->
            Nothing


findAdditionList : Expression -> Maybe (List Expression)
findAdditionList =
    findAddition >> Maybe.map (\( l, r, o ) -> l :: r :: o)


findNegate : Expression -> Maybe Expression
findNegate expr =
    case expr of
        UnaryOperation Negate e ->
            Just e

        _ ->
            Nothing


squashAndGroupAssociative : Dict String Expression -> AssociativeOperation -> List Expression -> List Expression
squashAndGroupAssociative context aop extracted =
    let
        letters =
            getFreeVariables <| List extracted

        sorted =
            List.foldr
                (\var ->
                    List.stableSortWith
                        (\lm rm ->
                            Maybe.withDefault EQ <|
                                Maybe.map2 compare
                                    (polyDegree var lm)
                                    (polyDegree var rm)
                        )
                )
                extracted
                (List.reverse <| Set.toList letters)

        grouped =
            case List.reverse sorted of
                [] ->
                    []

                shead :: stail ->
                    List.foldl
                        (\e ( last, acc ) ->
                            case groupStep e last of
                                Just c ->
                                    ( c, acc )

                                Nothing ->
                                    ( e, last :: acc )
                        )
                        ( shead, [] )
                        stail
                        |> (\( last, acc ) -> last :: acc)
                        |> List.map (innerSimplify context)

        groupStep =
            case aop of
                Addition ->
                    groupStepAddition

                Multiplication ->
                    groupStepMultiplication
    in
    grouped


groupStepAddition : Expression -> Expression -> Maybe Expression
groupStepAddition curr last =
    case ( curr, last ) of
        ( _, Integer 0 ) ->
            Just curr

        ( Integer il, Integer ir ) ->
            Just <| Integer <| il + ir

        ( _, AssociativeOperation Multiplication (Integer c) mon [] ) ->
            if Expression.equals curr mon then
                Just <| by [ Integer (c + 1), mon ]

            else
                Nothing

        ( UnaryOperation Negate nl, _ ) ->
            if Expression.equals nl last then
                Just zero

            else
                Nothing

        ( _, UnaryOperation Negate nr ) ->
            if Expression.equals nr curr then
                Just zero

            else
                Nothing

        ( _, _ ) ->
            if Expression.equals curr last then
                Just <| by [ two, last ]

            else
                Nothing


groupStepMultiplication : Expression -> Expression -> Maybe Expression
groupStepMultiplication curr last =
    log ("groupStepMultiplication \"" ++ Expression.toString curr ++ "\" \"" ++ Expression.toString last ++ "\"") <|
        case ( curr, last ) of
            ( Integer 1, _ ) ->
                Just last

            ( _, Integer 1 ) ->
                Just curr

            ( Integer il, Integer ir ) ->
                Just <| Integer <| il * ir

            ( _, BinaryOperation Power pb pe ) ->
                if Expression.equals curr pb then
                    case pe of
                        Integer pei ->
                            Just (ipow pb <| pei + 1)

                        _ ->
                            Just (BinaryOperation Power pb <| plus [ one, pe ])

                else
                    Nothing

            ( _, _ ) ->
                if Expression.equals curr last then
                    Just <| ipow last 2

                else
                    Nothing


transformAssociativeToDivision : Dict String Expression -> AssociativeOperation -> Expression -> List Expression -> Expression
transformAssociativeToDivision context aop divLcm extracted =
    innerSimplify context <|
        case aop of
            Addition ->
                let
                    _ =
                        log "divLcm" divLcm

                    multiplied =
                        List.map (\e -> by [ divLcm, e ]) extracted

                    _ =
                        log "multiplied" <| List.map Expression.toString multiplied
                in
                div (plus multiplied) divLcm

            Multiplication ->
                List.foldl
                    (\e ( n, d ) ->
                        case e of
                            BinaryOperation Division en ed ->
                                ( en :: n, ed :: d )

                            _ ->
                                ( e :: n, d )
                    )
                    ( [], [] )
                    extracted
                    |> (\( ns, ds ) ->
                            div
                                (by ns)
                                (by ds)
                       )


lcm : Expression -> Expression -> Expression
lcm l r =
    if Expression.equals l r then
        l

    else
        case ( l, r ) of
            ( Integer 1, _ ) ->
                r

            ( _, Integer 1 ) ->
                l

            ( Integer li, Integer ri ) ->
                let
                    gcd ll rr =
                        if ll < rr then
                            gcd rr ll

                        else if rr == 0 then
                            ll

                        else
                            gcd rr (modBy rr ll)
                in
                Integer <| li * ri // gcd li ri

            ( Variable lv, Variable rv ) ->
                if lv == rv then
                    l

                else
                    by [ l, r ]

            _ ->
                by <| log "gcd" [ l, r ]


denominatorLcm : List Expression -> Expression
denominatorLcm le =
    log ("denominatorLcm [" ++ String.join ", " (List.map Expression.toString le) ++ "]") <|
        List.foldl
            (\e a ->
                case e of
                    BinaryOperation Division _ d ->
                        lcm a d

                    _ ->
                        a
            )
            (Integer 1)
            le


polyDegree : String -> Expression -> Maybe Int
polyDegree var expr =
    case expr of
        Integer _ ->
            Just 0

        UnaryOperation _ e ->
            polyDegree var e

        BinaryOperation Division l r ->
            Maybe.map2 (-) (polyDegree var l) (polyDegree var r)

        BinaryOperation Power base (Integer i) ->
            Maybe.map ((*) i) <| polyDegree var base

        BinaryOperation Power _ _ ->
            Nothing

        RelationOperation LessThan _ _ ->
            Nothing

        RelationOperation LessThanOrEquals _ _ ->
            Nothing

        RelationOperation Equals _ _ ->
            Nothing

        RelationOperation GreaterThan _ _ ->
            Nothing

        RelationOperation GreaterThanOrEquals _ _ ->
            Nothing

        AssociativeOperation Addition l r o ->
            Maybe.andThen List.maximum <| Maybe.traverse (polyDegree var) (l :: r :: o)

        AssociativeOperation Multiplication l r o ->
            Maybe.map List.product <| Maybe.traverse (polyDegree var) (l :: r :: o)

        Variable v ->
            if v == var then
                Just 1

            else
                Just 0

        Float _ ->
            Just 0

        Apply _ _ ->
            Nothing

        Replace _ _ ->
            Nothing

        List _ ->
            Nothing