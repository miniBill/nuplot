module UI.Glsl.Sphere exposing (isSphereEquation, toGlsl)

import Expression exposing (AssociativeOperation(..), BinaryOperation(..), Expression(..), UnaryOperation(..))
import UI.Glsl.Code exposing (floatToGlsl)


type Addend
    = Known Float
    | Center String Float
    | Unknown


isSphereEquation : Expression -> Maybe { center : { x : Float, y : Float, z : Float }, radius : Float }
isSphereEquation e =
    let
        toCenter v c =
            if v == "x" || v == "y" || v == "z" then
                Just <| Center v c

            else
                Nothing

        asKnown c =
            case c of
                UnaryOperation Negate (Integer i) ->
                    Just (toFloat -i)

                UnaryOperation Negate (Float f) ->
                    Just -f

                Integer i ->
                    Just (toFloat i)

                Float f ->
                    Just f

                _ ->
                    Nothing

        findVariable =
            List.filterMap
                (\t ->
                    case t of
                        Variable v ->
                            Just v

                        _ ->
                            Nothing
                )

        extractAddends c =
            case c of
                AssociativeOperation Addition l m r ->
                    List.concatMap extractAddends <| l :: m :: r

                UnaryOperation Negate (Float f) ->
                    [ Known f ]

                BinaryOperation Power (Variable v) (Integer 2) ->
                    [ Maybe.withDefault Unknown <| toCenter v 0 ]

                BinaryOperation Power (AssociativeOperation Addition l m r) (Integer 2) ->
                    let
                        list =
                            l :: m :: r

                        knowns =
                            List.filterMap asKnown list
                    in
                    if List.length knowns /= List.length r + 1 then
                        [ Unknown ]

                    else
                        case findVariable list of
                            [ v ] ->
                                [ Center v -(List.sum knowns) ]

                            _ ->
                                [ Unknown ]

                Integer i ->
                    [ Known <| toFloat i ]

                Float f ->
                    [ Known f ]

                _ ->
                    case asKnown c of
                        Just f ->
                            [ Known f ]

                        Nothing ->
                            [ Unknown ]

        addends =
            extractAddends e

        radius =
            addends
                |> List.filterMap
                    (\s ->
                        case s of
                            Known f ->
                                Just -f

                            _ ->
                                Nothing
                    )
                |> List.sum

        getCenter v =
            addends
                |> List.filterMap
                    (\s ->
                        case s of
                            Center w k ->
                                if v == w then
                                    Just k

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
                |> (\l ->
                        case l of
                            [ k ] ->
                                Just k

                            _ ->
                                Nothing
                   )

        centerX =
            getCenter "x"

        centerY =
            getCenter "y"

        centerZ =
            getCenter "z"
    in
    if List.any ((==) Unknown) addends then
        Nothing

    else
        Maybe.map3 (\cx cy cz -> { center = { x = cx, y = cy, z = cz }, radius = sqrt <| max 0 radius })
            centerX
            centerY
            centerZ


toGlsl : String -> { x : Float, y : Float, z : Float } -> Float -> String
toGlsl suffix center radius =
    """
    bool bisect""" ++ suffix ++ """(vec3 o, vec3 d, float max_distance, out vec3 found) {
        vec3 center = vec3(""" ++ floatToGlsl center.x ++ """,""" ++ floatToGlsl center.y ++ """,""" ++ floatToGlsl center.z ++ """);

        vec3 to_center = o - center;
        float b = dot(to_center, d);
        float c = dot(to_center, to_center) - """ ++ floatToGlsl (radius * radius) ++ """;
        float delta = b*b - c;
        if(delta < 0.0)
            return false;
        float x = -b - sqrt(delta);
        if(x < 0.000001 * max_distance)
            return false;
        found = o + x * d;
        return true;
    }
    """
