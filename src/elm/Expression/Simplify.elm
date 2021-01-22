module Expression.Simplify exposing (hoistLambda, simplify, sortByDegree, stepSimplify)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), RelationOperation(..), UnaryOperation(..), filterContext, fullSubstitute, genericMatrixAddition, genericMatrixMultiplication, getFreeVariables, partialSubstitute, visit)
import Expression.Derivative
import Expression.Utils exposing (asPoly, by, byShort, cos_, div, factor, i, im, ipow, ipowShort, minus, negate_, one, plus, plusShort, pow, re, sin_, square, two, zero)
import List
import List.Extra as List
import List.MyExtra exposing (groupOneWith)
import Maybe.Extra as Maybe
import Set


simplify : Expression -> Expression
simplify =
    let
        go n e =
            if n <= 0 then
                e

            else
                let
                    res =
                        stepSimplify Dict.empty e
                in
                if Expression.equals e res then
                    e

                else
                    go (n - 1) res
    in
    go 100


stepSimplify : Dict String Expression -> Expression -> Expression
stepSimplify context expr =
    case expr of
        Variable v ->
            Dict.get v context |> Maybe.withDefault expr

        Replace vars e ->
            step1 context
                { andThen = fullSubstitute (filterContext vars)
                , ifChanged = Replace vars
                }
                e

        UnaryOperation Negate (UnaryOperation Negate e) ->
            e

        UnaryOperation Negate (Integer ni) ->
            Integer -ni

        UnaryOperation Negate (List ls) ->
            stepList context
                { andThen = \ss -> List <| List.map (UnaryOperation Negate) ss
                , ifChanged = \ss -> UnaryOperation Negate <| List ss
                }
                ls

        UnaryOperation Negate (BinaryOperation Division (UnaryOperation Negate n) d) ->
            div n d

        UnaryOperation Negate (BinaryOperation Division (Integer i) d) ->
            if i < 0 then
                div (Integer -i) d

            else
                UnaryOperation Negate <| div (Integer i) (stepSimplify context d)

        UnaryOperation Negate e ->
            UnaryOperation Negate <| stepSimplify context e

        Lambda x f ->
            Lambda x <| stepSimplify context f

        BinaryOperation bop l r ->
            step2 context
                { andThen =
                    case bop of
                        Division ->
                            stepSimplifyDivision

                        Power ->
                            stepSimplifyPower
                , ifChanged = BinaryOperation bop
                }
                l
                r

        RelationOperation rop l r ->
            step2 context
                { andThen = RelationOperation rop
                , ifChanged = RelationOperation rop
                }
                l
                r

        AssociativeOperation Multiplication l r [ AssociativeOperation Addition al ar ao ] ->
            plus <| List.map (\o -> by [ l, r, o ]) (al :: ar :: ao)

        AssociativeOperation aop l r o ->
            stepList context
                { andThen = stepSimplifyAssociative context aop
                , ifChanged =
                    if aop == Addition then
                        plus

                    else
                        by
                }
                (l :: r :: o)

        Apply name args ->
            stepList context
                { andThen = stepSimplifyApply name
                , ifChanged = Apply name
                }
                args

        List es ->
            stepList context
                { andThen = List
                , ifChanged = List
                }
                es

        Integer _ ->
            expr

        Float _ ->
            expr


step1 : Dict String Expression -> { andThen : Expression -> Expression, ifChanged : Expression -> Expression } -> Expression -> Expression
step1 context { andThen, ifChanged } arg =
    let
        sarg =
            stepSimplify context arg
    in
    if Expression.equals arg sarg then
        andThen sarg

    else
        ifChanged sarg


step2 : Dict String Expression -> { andThen : Expression -> Expression -> Expression, ifChanged : Expression -> Expression -> Expression } -> Expression -> Expression -> Expression
step2 context { andThen, ifChanged } arg1 arg2 =
    let
        sarg1 =
            stepSimplify context arg1

        sarg2 =
            stepSimplify context arg2
    in
    if Expression.equals arg1 sarg1 && Expression.equals arg2 sarg2 then
        andThen sarg1 sarg2

    else
        ifChanged sarg1 sarg2


stepList : Dict String Expression -> { andThen : List Expression -> Expression, ifChanged : List Expression -> Expression } -> List Expression -> Expression
stepList context { andThen, ifChanged } args =
    let
        sargs =
            List.map (stepSimplify context) args
    in
    if List.map2 Expression.equals args sargs |> List.all identity then
        andThen sargs

    else
        ifChanged sargs


stepSimplifyApply : FunctionName -> List Expression -> Expression
stepSimplifyApply fname sargs =
    case fname of
        UserFunction _ ->
            Apply fname sargs

        KnownFunction name ->
            case ( name, sargs ) of
                ( Sqrt, _ ) ->
                    stepSimplifySqrt sargs

                ( Sinh, [ AssociativeOperation Multiplication l r o ] ) ->
                    case extract (findSpecificVariable "i") (l :: r :: o) of
                        Just ( _, rest ) ->
                            by [ i, sin_ <| by rest ]

                        Nothing ->
                            Apply fname sargs

                ( Cosh, [ AssociativeOperation Multiplication l r o ] ) ->
                    case extract (findSpecificVariable "i") (l :: r :: o) of
                        Just ( _, rest ) ->
                            cos_ <| by rest

                        Nothing ->
                            Apply fname sargs

                ( Dd, [ expr, Variable var ] ) ->
                    Expression.Derivative.derivative var expr

                ( Det, [ expr ] ) ->
                    Expression.Utils.determinant expr

                ( Re, _ ) ->
                    stepSimplifyRe sargs

                ( Im, [ AssociativeOperation Addition l m r ] ) ->
                    plus <| List.map im <| l :: m :: r

                ( Im, [ Variable v ] ) ->
                    if v == "i" then
                        one

                    else
                        zero

                ( Im, [ Integer _ ] ) ->
                    zero

                ( Im, [ Float _ ] ) ->
                    zero

                ( Im, [ AssociativeOperation Multiplication l m r ] ) ->
                    let
                        rest =
                            by (m :: r)
                    in
                    plus [ by [ re l, im rest ], by [ im l, re rest ] ]

                _ ->
                    Apply fname sargs


stepSimplifySqrt : List Expression -> Expression
stepSimplifySqrt sargs =
    case sargs of
        [ Integer j ] ->
            let
                a =
                    abs j

                r =
                    truncate (sqrt <| toFloat a)

                s =
                    if r * r == a then
                        Integer r

                    else
                        let
                            ( outer, inner ) =
                                abs a
                                    |> factor
                                    |> List.foldl
                                        (\( f, e ) ( o, i ) ->
                                            ( o * f ^ (e // 2)
                                            , i * f ^ modBy 2 e
                                            )
                                        )
                                        ( 1, 1 )
                        in
                        case ( outer, inner ) of
                            ( _, 1 ) ->
                                Integer outer

                            ( 1, _ ) ->
                                Apply (KnownFunction Sqrt) [ Integer inner ]

                            _ ->
                                by [ Integer outer, Apply (KnownFunction Sqrt) [ Integer inner ] ]
            in
            if j < 0 then
                by [ Variable "i", s ]

            else
                s

        _ ->
            Apply (KnownFunction Sqrt) sargs


stepSimplifyRe : List Expression -> Expression
stepSimplifyRe sargs =
    case sargs of
        [ BinaryOperation Division (AssociativeOperation Addition l m r) d ] ->
            plus <| List.map (\c -> re <| div c d) (l :: m :: r)

        [ BinaryOperation Division n (Integer d) ] ->
            div (re n) (Integer d)

        [ UnaryOperation Negate c ] ->
            negate_ <| re c

        [ AssociativeOperation Addition l m r ] ->
            plus <| List.map re <| l :: m :: r

        [ Variable v ] ->
            if v == "i" then
                zero

            else
                Variable v

        [ Integer i ] ->
            Integer i

        [ Float i ] ->
            Float i

        [ AssociativeOperation Multiplication l m r ] ->
            let
                rest =
                    by (m :: r)
            in
            minus (by [ re l, re rest ]) (by [ im l, im rest ])

        _ ->
            Apply (KnownFunction Re) sargs


stepSimplifyDivision : Expression -> Expression -> Expression
stepSimplifyDivision ls rs =
    case ( ls, rs ) of
        ( BinaryOperation Division lsn lsd, _ ) ->
            div lsn <| by [ lsd, rs ]

        ( _, Variable "i" ) ->
            by [ ls, negate_ i ]

        ( _, Integer 1 ) ->
            ls

        ( Integer 0, _ ) ->
            ls

        ( Integer n, Integer d ) ->
            if modBy d n == 0 then
                Integer (n // d)

            else
                div ls rs

        ( _, AssociativeOperation Addition l m r ) ->
            let
                ( reals, immaginaries ) =
                    (l :: m :: r)
                        |> List.foldl
                            (\e ( ar, ai ) ->
                                case isImmaginary e of
                                    Just k ->
                                        ( ar, k :: ai )

                                    Nothing ->
                                        ( e :: ar, ai )
                            )
                            ( [], [] )

                plusReals =
                    plus reals

                plusImmaginaries =
                    plus immaginaries

                inverted =
                    div
                        (minus plusReals <| by [ Variable "i", plusImmaginaries ])
                        (plus [ square plusReals, square plusImmaginaries ])
            in
            case ( immaginaries, ls ) of
                ( [], _ ) ->
                    div ls rs

                ( _, Integer 1 ) ->
                    inverted

                _ ->
                    by [ ls, inverted ]

        _ ->
            if Expression.equals ls rs then
                one

            else
                div ls rs


isImmaginary : Expression -> Maybe Expression
isImmaginary e =
    case e of
        Variable "i" ->
            Just one

        UnaryOperation Negate c ->
            Maybe.map negate_ <| isImmaginary c

        AssociativeOperation Multiplication ll mm rr ->
            let
                nonIs =
                    List.filterNot ((==) (Variable "i")) (ll :: mm :: rr)
            in
            case modBy 4 <| List.length rr + 2 - List.length nonIs of
                0 ->
                    Nothing

                1 ->
                    Just <| by nonIs

                2 ->
                    Nothing

                _ ->
                    Just <| negate_ <| by nonIs

        _ ->
            Nothing


stepSimplifyPower : Expression -> Expression -> Expression
stepSimplifyPower ls rs =
    case ( ls, rs ) of
        ( AssociativeOperation Multiplication lm rm om, Integer rsi ) ->
            AssociativeOperation Multiplication
                (ipow lm rsi)
                (ipow rm rsi)
                (List.map (\b -> ipow b rsi) om)

        ( AssociativeOperation Addition _ _ _, Integer rsi ) ->
            if rsi < 0 then
                BinaryOperation Division one (ipow ls -rsi)

            else
                case rsi of
                    0 ->
                        Integer 1

                    1 ->
                        ls

                    _ ->
                        by [ ls, ipow ls (rsi - 1) ]

        ( Integer il, Integer ir ) ->
            if ir < 0 then
                div one <| Integer <| il ^ -ir

            else
                Integer <| il ^ ir

        ( Integer b, BinaryOperation Division (Integer en) (Integer ed) ) ->
            if b == 0 then
                Integer 0

            else if b == 1 then
                Integer 1

            else
                let
                    appr =
                        floor <| toFloat b ^ (toFloat en / toFloat ed)
                in
                if appr ^ ed == b ^ en then
                    Integer appr

                else
                    pow (Integer (b ^ en)) (div one (Integer ed))

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


stepSimplifyAssociative : Dict String Expression -> AssociativeOperation -> List Expression -> Expression
stepSimplifyAssociative context aop args =
    let
        extractop e =
            case e of
                UnaryOperation Negate (AssociativeOperation iaop il ir io) ->
                    if iaop == aop then
                        if aop == Addition then
                            List.map (UnaryOperation Negate) (il :: ir :: io)

                        else
                            UnaryOperation Negate il :: ir :: io

                    else
                        [ e ]

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

        groupStep =
            case aop of
                Addition ->
                    groupStepAddition

                Multiplication ->
                    groupStepMultiplication

        andThen : List Expression -> Expression
        andThen ls =
            ls
                |> groupOneWith groupStep
                |> (case aop of
                        Addition ->
                            tryExtract (findSpecificInteger 0) (\( _, rest ) -> rest)

                        Multiplication ->
                            tryExtract (findSpecificInteger -1) (\( _, rest ) -> [ negate_ <| build rest ])
                                >> tryExtract findNegate (\( negated, rest ) -> [ negate_ <| build <| negated :: rest ])
                   )
                |> sortByDegree aop
                |> build
    in
    args
        |> List.concatMap extractop
        |> sortByDegree aop
        |> stepList context
            { ifChanged = build
            , andThen = andThen
            }


tryExtract : (a -> Maybe b) -> (( b, List a ) -> List a) -> List a -> List a
tryExtract f g es =
    es
        |> extract f
        |> Maybe.map g
        |> Maybe.withDefault es


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


findNegate : Expression -> Maybe Expression
findNegate expr =
    case expr of
        UnaryOperation Negate e ->
            Just e

        _ ->
            Nothing


sortByDegree : AssociativeOperation -> List Expression -> List Expression
sortByDegree aop ee =
    let
        letters =
            List ee
                |> getFreeVariables
                |> Set.toList
                |> List.sortBy (\s -> ( String.length s, s ))
                |> (\l ->
                        if aop == Addition then
                            List.reverse l

                        else
                            l
                   )

        by f x y =
            if aop == Addition then
                compare -(f x) -(f y)

            else
                compare (f x) (f y)
    in
    List.foldl
        (\var ->
            List.stableSortWith
                (by <|
                    \e ->
                        -- "i" doesn't have higher nonsimplified powers, and we want it later
                        if var == "i" && aop == Addition then
                            negate <| Maybe.withDefault -1 <| polyDegree var e

                        else
                            Maybe.withDefault -1 <| polyDegree var e
                )
        )
        ee
        letters


groupStepAddition : Expression -> Expression -> Maybe Expression
groupStepAddition left right =
    case Set.toList <| getFreeVariables left of
        [ var ] ->
            case
                Result.map2 Tuple.pair
                    (asPoly var left |> Result.map Dict.toList)
                    (asPoly var right |> Result.map Dict.toList)
            of
                Ok ( [ ( dl, cl ) ], [ ( dr, cr ) ] ) ->
                    if dl == dr then
                        Just <| byShort [ plusShort [ cl, cr ], ipowShort (Variable var) dl ]

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            case ( left, right ) of
                ( _, Integer 0 ) ->
                    Just left

                ( Integer il, Integer ir ) ->
                    Just <| Integer <| il + ir

                ( _, AssociativeOperation Multiplication (Integer c) mon [] ) ->
                    if Expression.equals left mon then
                        Just <| by [ Integer (c + 1), mon ]

                    else
                        Nothing

                ( UnaryOperation Negate nl, _ ) ->
                    if Expression.equals nl right then
                        Just zero

                    else
                        Nothing

                ( _, UnaryOperation Negate nr ) ->
                    if Expression.equals nr left then
                        Just zero

                    else
                        Nothing

                ( BinaryOperation Division ln ld, BinaryOperation Division rn rd ) ->
                    Just <| div (plus [ by [ ln, rd ], by [ rn, ld ] ]) (by [ ld, rd ])

                ( BinaryOperation Division ln ld, r ) ->
                    Just <| div (plus [ ln, by [ ld, r ] ]) ld

                ( l, BinaryOperation Division rn rd ) ->
                    Just <| div (plus [ by [ l, rd ], rn ]) rd

                ( List _, List _ ) ->
                    addMatrices left right

                _ ->
                    if Expression.equals left right then
                        Just <| by [ two, right ]

                    else
                        Nothing


asList : Expression -> Maybe (List Expression)
asList l =
    case l of
        List u ->
            Just u

        _ ->
            Nothing


groupStepMultiplication : Expression -> Expression -> Maybe Expression
groupStepMultiplication left right =
    case ( left, right ) of
        ( Integer 1, l ) ->
            Just l

        ( c, Integer 1 ) ->
            Just c

        ( _, Integer 0 ) ->
            Just <| Integer 0

        ( Integer 0, _ ) ->
            Just <| Integer 0

        ( Integer il, Integer ir ) ->
            Just <| Integer <| il * ir

        ( AssociativeOperation Addition l m r, _ ) ->
            Just <| plus <| by [ l, right ] :: by [ m, right ] :: List.map (\o -> by [ o, right ]) r

        ( _, AssociativeOperation Addition l m r ) ->
            Just <| plus <| by [ left, l ] :: by [ left, m ] :: List.map (\o -> by [ left, o ]) r

        ( BinaryOperation Power lb le, BinaryOperation Power rb re ) ->
            if Expression.equals lb rb then
                case ( le, re ) of
                    ( Integer lei, Integer rei ) ->
                        Just (ipow lb <| lei + rei)

                    _ ->
                        Just (pow lb <| plus [ le, re ])

            else
                Nothing

        ( BinaryOperation Power pb pe, l ) ->
            if Expression.equals l pb then
                case pe of
                    Integer pei ->
                        Just (ipow pb <| pei + 1)

                    _ ->
                        Just (BinaryOperation Power pb <| plus [ one, pe ])

            else
                Nothing

        ( _, BinaryOperation Power pb pe ) ->
            if Expression.equals left pb then
                case pe of
                    Integer pei ->
                        Just (ipow pb <| pei + 1)

                    _ ->
                        Just (BinaryOperation Power pb <| plus [ one, pe ])

            else
                Nothing

        ( BinaryOperation Division ln ld, r ) ->
            Just <| div (by [ ln, r ]) ld

        ( l, BinaryOperation Division rn rd ) ->
            Just <| div (by [ l, rn ]) rd

        ( List _, List _ ) ->
            multiplyMatrices left right

        ( Lambda x f, c ) ->
            Just <| partialSubstitute x c f

        _ ->
            if Expression.equals left right then
                Just <| ipow right 2

            else
                Nothing


addMatrices : Expression -> Expression -> Maybe Expression
addMatrices =
    genericMatrixAddition
        { asList = asList
        , plus = plus
        , toList = List
        }


multiplyMatrices : Expression -> Expression -> Maybe Expression
multiplyMatrices =
    genericMatrixMultiplication
        { asList = asList
        , by = by
        , plus = plus
        , toList = List
        }


polyDegree : String -> Expression -> Maybe Int
polyDegree var expr =
    let
        res =
            case expr of
                Integer _ ->
                    Just 0

                UnaryOperation Negate e ->
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
                    Maybe.map List.sum <| Maybe.traverse (polyDegree var) (l :: r :: o)

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

                Lambda _ _ ->
                    Nothing
    in
    res


hoistLambda : Expression -> Expression
hoistLambda q =
    q
        |> visit
            (\expr ->
                case expr of
                    Lambda x f ->
                        Just <| Lambda x <| hoistLambda f

                    BinaryOperation Power b e ->
                        case ( hoistLambda b, hoistLambda e ) of
                            ( Lambda x f, he ) ->
                                Just <| Lambda x <| hoistLambda <| BinaryOperation Power f he

                            _ ->
                                Nothing

                    AssociativeOperation Multiplication l m r ->
                        let
                            step curr last =
                                case curr of
                                    Lambda x f ->
                                        Just <| hoistLambda <| partialSubstitute x last f

                                    _ ->
                                        Nothing

                            grouped =
                                groupOneWith step <| hoistLambda l :: hoistLambda m :: List.map hoistLambda r
                        in
                        Just <|
                            case grouped of
                                [] ->
                                    Integer 1

                                [ x ] ->
                                    x

                                x :: y :: zs ->
                                    AssociativeOperation Multiplication x y zs

                    Apply fn args ->
                        case ( fn, List.map hoistLambda args ) of
                            ( KnownFunction Plot, _ ) ->
                                Nothing

                            ( _, [ Lambda x f ] ) ->
                                Just <| Lambda x <| hoistLambda <| Apply fn [ f ]

                            _ ->
                                Nothing

                    _ ->
                        Nothing
            )
