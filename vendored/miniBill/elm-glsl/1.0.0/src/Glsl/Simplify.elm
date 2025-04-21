module Glsl.Simplify exposing (expr, stat)

import Glsl exposing (Expr(..), Stat(..), UnaryOperation(..))


stat : Stat -> Stat
stat root =
    case root of
        Return e ->
            map stat root Return expr e

        ExpressionStatement e ->
            map stat root ExpressionStatement expr e

        If e s1 ->
            map2 stat root If expr e stat s1

        IfElse e s1 s2 ->
            map3 stat root IfElse expr e stat s1 stat s2

        Decl tipe name val ->
            map stat root (Decl tipe name) (Maybe.map expr) val

        For init check step loop ->
            map4 stat root For (Maybe.map stat) init expr check expr step stat loop

        Block a b children ->
            map stat root (Block (stat a) (stat b)) (List.map stat) children

        Nop ->
            Nop

        Break ->
            Break

        Continue ->
            Continue


expr : Expr -> Expr
expr root =
    case root of
        Ternary c t f ->
            map3 expr root Ternary expr c expr t expr f

        Dot l r ->
            map2 expr root Dot expr l identity r

        BinaryOperation l op r ->
            map3 expr root BinaryOperation expr l identity op expr r

        UnaryOperation Negate (Int i) ->
            Int -i

        UnaryOperation Negate (Float f) ->
            Float -f

        UnaryOperation op l ->
            map expr root (UnaryOperation op) expr l

        Call l r ->
            map2 expr root Call expr l (List.map expr) r

        -- Bool _ ->
        -- Int _ ->
        -- Float _ ->
        -- Uint _ ->
        -- Double _ ->
        -- Variable _ ->
        _ ->
            root


map : (a -> a) -> a -> (b -> a) -> (b -> b) -> b -> a
map rootKind root ctor kind val =
    let
        vals : b
        vals =
            kind val
    in
    if vals == val then
        root

    else
        rootKind (ctor vals)


map2 : (a -> a) -> a -> (b -> c -> a) -> (b -> b) -> b -> (c -> c) -> c -> a
map2 rootKind root ctor kind1 val1 kind2 val2 =
    let
        val1s : b
        val1s =
            kind1 val1

        val2s : c
        val2s =
            kind2 val2
    in
    if val1s == val1 && val2s == val2 then
        root

    else
        rootKind (ctor val1s val2s)


map3 : (a -> a) -> a -> (b -> c -> d -> a) -> (b -> b) -> b -> (c -> c) -> c -> (d -> d) -> d -> a
map3 rootKind root ctor kind1 val1 kind2 val2 kind3 val3 =
    let
        val1s : b
        val1s =
            kind1 val1

        val2s : c
        val2s =
            kind2 val2

        val3s : d
        val3s =
            kind3 val3
    in
    if val1s == val1 && val2s == val2 && val3s == val3 then
        root

    else
        rootKind (ctor val1s val2s val3s)


map4 : (a -> a) -> a -> (b -> c -> d -> e -> a) -> (b -> b) -> b -> (c -> c) -> c -> (d -> d) -> d -> (e -> e) -> e -> a
map4 rootKind root ctor kind1 val1 kind2 val2 kind3 val3 kind4 val4 =
    let
        val1s : b
        val1s =
            kind1 val1

        val2s : c
        val2s =
            kind2 val2

        val3s : d
        val3s =
            kind3 val3

        val4s : e
        val4s =
            kind4 val4
    in
    if val1s == val1 && val2s == val2 && val3s == val3 && val4s == val4 then
        root

    else
        rootKind (ctor val1s val2s val3s val4s)
