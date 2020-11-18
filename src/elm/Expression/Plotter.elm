module Expression.Plotter exposing (Bounds, defaultBounds, getPng)

import Array exposing (Array)
import Array.Extra as Array
import Complex
import Dict
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import Image exposing (Pixel)


type alias Bounds =
    { minx : Float
    , miny : Float
    , maxx : Float
    , maxy : Float
    , steps : Int
    }


defaultBounds : Bounds
defaultBounds =
    { minx = -5
    , maxx = 5
    , miny = -5
    , maxy = 5
    , steps = 300
    }


colors : { black : number, red : number, green : number, white : number }
colors =
    { black = 0xFF
    , red = 0xF01010FF
    , green = 0x10F010FF
    , white = 0xFFFFFFFF
    }


getPng : Bounds -> Graph -> String
getPng bounds graph =
    drawGraph bounds graph
        |> Image.fromArray2d
        |> Image.toPngUrl


type alias Canvas =
    Array (Array Pixel)


emptyCanvas : Bounds -> Canvas
emptyCanvas bounds =
    let
        emptyRow =
            Array.repeat bounds.steps colors.black
    in
    Array.repeat bounds.steps emptyRow


drawGraph : Bounds -> Graph -> Canvas
drawGraph bounds graph =
    case graph of
        Explicit2D e ->
            explicit2d bounds e

        Relation2D rop l r ->
            relation2d bounds rop l r

        Implicit2D l r ->
            implicit2d bounds l r


screenToX : Bounds -> Int -> Float
screenToX { minx, maxx, steps } sx =
    minx + toFloat sx * (maxx - minx) / (toFloat steps - 1)


xToScreen : Bounds -> Float -> Int
xToScreen { minx, maxx, steps } x =
    round <| (x - minx) / (maxx - minx) * (toFloat steps - 1)


screenToY : Bounds -> Int -> Float
screenToY { miny, maxy, steps } sy =
    miny + toFloat sy * (maxy - miny) / (toFloat steps - 1)


yToScreen : Bounds -> Float -> Int
yToScreen { miny, maxy, steps } y =
    round <| (y - miny) / (maxy - miny) * (toFloat steps - 1)


explicit2d : Bounds -> Expression -> Canvas
explicit2d bounds e =
    let
        dx =
            deltaX bounds
    in
    axes2 bounds
        |> sweepX bounds
            (\sx x canvas ->
                let
                    px =
                        x - dx

                    py =
                        Complex.real <| Expression.value (Dict.singleton "x" <| Complex.fromReal px) e

                    psy =
                        yToScreen bounds py

                    y =
                        Complex.real <| Expression.value (Dict.singleton "x" <| Complex.fromReal x) e

                    sy =
                        yToScreen bounds y
                in
                List.range (min sy psy) (max sy psy)
                    |> List.foldl (\drawingSy -> set sx drawingSy colors.white) canvas
            )


sweep : Bounds -> (Int -> Canvas -> Canvas) -> Canvas -> Canvas
sweep { steps } f =
    let
        go i a =
            if i >= steps then
                a

            else
                go (i + 1) (f i a)
    in
    go 0


sweepX : Bounds -> (Int -> Float -> Canvas -> Canvas) -> Canvas -> Canvas
sweepX bounds f =
    sweep bounds (\sx -> f sx (screenToX bounds sx))


sweepY : Bounds -> (Int -> Float -> Canvas -> Canvas) -> Canvas -> Canvas
sweepY bounds f =
    sweep bounds (\sy -> f sy (screenToY bounds sy))


axes2 : Bounds -> Canvas
axes2 bounds =
    let
        xzero =
            xToScreen bounds 0

        yzero =
            yToScreen bounds 0
    in
    emptyCanvas bounds
        |> sweep bounds (\sx -> set sx yzero colors.red)
        |> sweep bounds (\sy -> set xzero sy colors.green)
        |> set xzero yzero colors.black


set : Int -> Int -> Pixel -> Canvas -> Canvas
set x y pixel =
    Array.update y (Array.set x pixel)


relation2d : Bounds -> RelationOperation -> Expression -> Expression -> Canvas
relation2d bounds rop l r =
    let
        e =
            RelationOperation rop l r
    in
    axes2 bounds
        |> sweepX bounds
            (\sx x ->
                sweepY bounds
                    (\sy y ->
                        if 0 < dvalue x y e then
                            set sx sy colors.white

                        else
                            identity
                    )
            )


deltaX : Bounds -> Float
deltaX { minx, maxx, steps } =
    (maxx - minx) / (toFloat steps - 1)


deltaY : Bounds -> Float
deltaY { miny, maxy, steps } =
    (maxy - miny) / (toFloat steps - 1)


implicit2d : Bounds -> Expression -> Expression -> Canvas
implicit2d bounds l r =
    let
        dx =
            deltaX bounds

        dy =
            deltaY bounds

        e =
            RelationOperation LessThan l r
    in
    axes2 bounds
        |> sweepX bounds
            (\sx x ->
                sweepY bounds
                    (\sy y ->
                        let
                            here =
                                dvalue x y e > 0

                            up =
                                dvalue x (y - dy) e > 0

                            left =
                                dvalue (x - dx) y e > 0

                            ul =
                                dvalue (x - dx) (y - dy) e > 0
                        in
                        if here /= up || here /= left || here /= ul then
                            set sx sy colors.white

                        else
                            identity
                    )
            )


dvalue : Float -> Float -> Expression -> Float
dvalue x y e =
    Complex.real <|
        Expression.value
            (Dict.fromList
                [ ( "x", Complex.fromReal x )
                , ( "y", Complex.fromReal y )
                ]
            )
            e
