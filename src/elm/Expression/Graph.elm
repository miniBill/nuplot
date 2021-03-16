module Expression.Graph exposing (Graph(..), fromExpression, toExpression, toString)

import Expression exposing (AssociativeOperation, BinaryOperation, Expression(..), FunctionName(..), KnownFunction(..), RelationOperation(..), UnaryOperation, getFreeVariables)
import Expression.Simplify
import Expression.Utils exposing (minus)
import Result.Extra as Result
import Set


type Graph
    = Explicit2D Expression
    | Relation2D Expression
    | Implicit2D Expression Expression
    | Polar2D Expression
    | VectorField2D Expression
    | Parametric2D Expression Expression
    | Implicit3D Expression
    | Contour Expression
    | GraphList (List Graph)


type GraphExpression
    = GFloat Float
    | GVariable String
    | GAssociative AssociativeOperation GraphExpression GraphExpression (List GraphExpression)
    | GBinary BinaryOperation GraphExpression GraphExpression
    | GUnary UnaryOperation GraphExpression
    | GApply FunctionName (List GraphExpression)
    | GList (List GraphExpression)


fromExpression : Expression -> Graph
fromExpression =
    let
        go expr =
            let
                free =
                    getFreeVariables expr
            in
            case expr of
                Lambda x f ->
                    fromExpression <| Expression.partialSubstitute x (Variable "x") f

                Replace ctx (RelationOperation rop l r) ->
                    fromExpression (RelationOperation rop (Replace ctx l) (Replace ctx r))

                RelationOperation rop l r ->
                    if Set.member "z" <| free then
                        Implicit3D expr

                    else if Set.member "r" free then
                        Polar2D <| minus l r

                    else
                        case ( l, rop, Set.member "y" <| getFreeVariables r ) of
                            ( Variable "y", Equals, False ) ->
                                Explicit2D r

                            ( _, Equals, _ ) ->
                                Implicit2D l r

                            _ ->
                                Relation2D expr

                List ls ->
                    if Set.member "r" free then
                        GraphList <| List.map fromExpression ls

                    else if Set.member "t" free then
                        case ls of
                            [ x, y ] ->
                                Parametric2D x y

                            _ ->
                                GraphList <| List.map fromExpression ls

                    else
                        GraphList <| List.map fromExpression ls

                Apply (KnownFunction Simplify) [ e ] ->
                    go <| Expression.Simplify.simplify e

                _ ->
                    if Set.member "z" free then
                        Implicit3D expr

                    else if Set.member "r" free then
                        Polar2D expr

                    else if Set.member "y" free then
                        Contour expr

                    else
                        Explicit2D expr
    in
    go << Expression.Simplify.hoistLambda


toString : Graph -> String
toString g =
    case g of
        Explicit2D e ->
            "Explicit2D " ++ Expression.toString e

        Relation2D e ->
            "Relation2D " ++ Expression.toString e

        Implicit2D l r ->
            "Implicit2D " ++ Expression.toString (RelationOperation Equals l r)

        Polar2D e ->
            "Polar2D " ++ Expression.toString e

        Parametric2D x y ->
            "Parametric2D " ++ Expression.toString x ++ ", " ++ Expression.toString y

        Implicit3D e ->
            "Implicit3D " ++ Expression.toString e

        Contour c ->
            "Contour " ++ Expression.toString c

        VectorField2D e ->
            "VectorField2D" ++ Expression.toString e

        GraphList gs ->
            "GraphList [" ++ String.join ", " (List.map toString gs) ++ "]"


toExpression : Graph -> Expression
toExpression g =
    case g of
        Explicit2D e ->
            e

        Relation2D e ->
            e

        Implicit2D l r ->
            RelationOperation Equals l r

        Implicit3D e ->
            e

        Contour e ->
            e

        Polar2D e ->
            e

        VectorField2D e ->
            e

        Parametric2D x y ->
            List [ x, y ]

        GraphList [ c ] ->
            toExpression c

        GraphList gs ->
            List <| List.map toExpression gs
