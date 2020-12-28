module Expression.Simplify exposing (hoistLambda, simplify, sortByDegree)

import Dict exposing (Dict)
import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), FunctionName(..), KnownFunction(..), RelationOperation(..), UnaryOperation(..), filterContext, fullSubstitute, genericMatrixAddition, genericMatrixMultiplication, getFreeVariables, partialSubstitute, visit)
import Expression.Derivative
import Expression.Utils exposing (by, cos_, div, i, icomplex, ipow, negate_, one, plus, pow, sin_, two, zero)
import List
import List.Extra as List
import List.MyExtra exposing (groupWith)
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
            fullSubstitute (filterContext vars) e

        UnaryOperation Negate (UnaryOperation Negate e) ->
            e

        UnaryOperation Negate (Integer ni) ->
            Integer -ni

        UnaryOperation Negate (List ls) ->
            List <| List.map (UnaryOperation Negate) ls

        UnaryOperation Negate e ->
            UnaryOperation Negate <| stepSimplify context e

        Lambda x f ->
            Lambda x <| stepSimplify context f

        BinaryOperation bop l r ->
            let
                ls =
                    stepSimplify context l

                rs =
                    stepSimplify context r
            in
            case bop of
                Division ->
                    stepSimplifyDivision ls rs

                Power ->
                    stepSimplifyPower ls rs

        RelationOperation rop l r ->
            let
                ls =
                    stepSimplify context l

                rs =
                    stepSimplify context r
            in
            RelationOperation rop ls rs

        AssociativeOperation aop l r o ->
            stepSimplifyAssociative aop <| List.map (stepSimplify context) (l :: r :: o)

        Apply name args ->
            stepSimplifyApply name <| List.map (stepSimplify context) args

        List es ->
            List <| List.map (stepSimplify context) es

        Integer _ ->
            expr

        Float _ ->
            expr


stepSimplifyApply : FunctionName -> List Expression -> Expression
stepSimplifyApply fname sargs =
    case fname of
        UserFunction _ ->
            Apply fname sargs

        KnownFunction name ->
            case ( name, sargs ) of
                ( Sqrt, [ Integer i ] ) ->
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
                        Apply fname sargs

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

                _ ->
                    Apply fname sargs


stepSimplifyDivision : Expression -> Expression -> Expression
stepSimplifyDivision ls rs =
    case ( ls, rs ) of
        ( BinaryOperation Division lsn lsd, _ ) ->
            div lsn <| by [ lsd, rs ]

        ( _, Variable "i" ) ->
            by [ ls, negate_ i ]

        _ ->
            if Expression.equals ls rs then
                one

            else
                div ls rs


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


stepSimplifyAssociative : AssociativeOperation -> List Expression -> Expression
stepSimplifyAssociative aop args =
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
    in
    args
        |> List.concatMap extractop
        |> sortByDegree aop
        |> groupWith groupStep
        |> (case aop of
                Addition ->
                    tryExtract (findSpecificInteger 0) (\( _, rest ) -> rest)

                Multiplication ->
                    tryExtract (findSpecificInteger -1) (\( _, rest ) -> [ negate_ <| build rest ])
                        >> tryExtract findNegate (\( negated, rest ) -> [ negate_ <| build <| negated :: rest ])
                        >> tryExtract findAdditionList (\( adds, rest ) -> [ plus <| List.map (\x -> by <| x :: rest) adds ])
           )
        |> sortByDegree aop
        |> build


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

        ( List _, List _ ) ->
            addMatrices curr last

        _ ->
            if Expression.equals curr last then
                Just <| by [ two, last ]

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
groupStepMultiplication curr last =
    case ( curr, last ) of
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
            Just <| plus <| by [ l, last ] :: by [ m, last ] :: List.map (\o -> by [ o, last ]) r

        ( _, AssociativeOperation Addition l m r ) ->
            Just <| plus <| by [ curr, l ] :: by [ curr, m ] :: List.map (\o -> by [ curr, o ]) r

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
            if Expression.equals curr pb then
                case pe of
                    Integer pei ->
                        Just (ipow pb <| pei + 1)

                    _ ->
                        Just (BinaryOperation Power pb <| plus [ one, pe ])

            else
                Nothing

        ( List _, List _ ) ->
            multiplyMatrices curr last

        ( Lambda x f, c ) ->
            Just <| partialSubstitute x c f

        _ ->
            if Expression.equals curr last then
                Just <| ipow last 2

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


transformAssociativeToDivision : Dict String Expression -> AssociativeOperation -> Expression -> List Expression -> Expression
transformAssociativeToDivision context aop divLcm extracted =
    case aop of
        Addition ->
            let
                multiplied =
                    List.map (\e -> by [ divLcm, e ]) extracted
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
                by [ l, r ]


denominatorLcm : List Expression -> Expression
denominatorLcm le =
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
hoistLambda =
    visit <|
        \expr ->
            case expr of
                Lambda x f ->
                    Just <| Lambda x <| hoistLambda f

                BinaryOperation Power b e ->
                    case ( hoistLambda b, hoistLambda e ) of
                        ( Lambda x f, he ) ->
                            Just <| Lambda x <| hoistLambda <| BinaryOperation Power f he

                        ( _, _ ) ->
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
                            groupWith step <| hoistLambda l :: hoistLambda m :: List.map hoistLambda r
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
