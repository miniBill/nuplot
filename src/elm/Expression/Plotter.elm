module Expression.Plotter exposing (screenToX)

import Array exposing (Array)
import Array.Extra as Array
import Color
import Complex exposing (Complex(..))
import Dict
import Expression exposing (Expression(..), Graph(..), RelationOperation(..))
import Image exposing (Pixel)
import UI.Theme as Theme


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
    , steps = Theme.imageWidth
    }


colors : { black : number, red : number, green : number, blue : number, white : number }
colors =
    { black = 0xFF
    , red = 0xF01010FF
    , green = 0x10F010FF
    , white = 0xFFFFFFFF
    , blue = 0x1010F0FF
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

        Contour e ->
            contour bounds e


screenToX : Bounds -> Int -> Float
screenToX { minx, maxx, steps } sx =
    minx + toFloat sx * (maxx - minx) / (toFloat steps - 1)


xToScreen : Bounds -> Float -> Int
xToScreen { minx, maxx, steps } x =
    round <| (x - minx) / (maxx - minx) * (toFloat steps - 1)


screenToY : Bounds -> Int -> Float
screenToY { miny, maxy, steps } sy =
    maxy - toFloat sy * (maxy - miny) / (toFloat steps - 1)


yToScreen : Bounds -> Float -> Int
yToScreen { miny, maxy, steps } y =
    round <| ((maxy - y) / (maxy - miny) * (toFloat steps - 1))


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


sweep : Bounds -> (Int -> a -> a) -> a -> a
sweep { steps } f =
    let
        go i a =
            if i >= steps then
                a

            else
                go (i + 1) (f i a)
    in
    go 0


sweepX : Bounds -> (Int -> Float -> a -> a) -> a -> a
sweepX bounds f =
    sweep bounds (\sx -> f sx (screenToX bounds sx))


sweepY : Bounds -> (Int -> Float -> a -> a) -> a -> a
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
                            set sx sy colors.blue

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


contour : Bounds -> Expression -> Canvas
contour bounds e =
    let
        valueToColor (Complex cr ci) =
            let
                z =
                    Complex -cr ci

                logRadius =
                    logBase 2 <|
                        Complex.abs z

                -- theta \in [0, 1]
                theta =
                    (pi - Complex.arg z) / (2 * pi)

                -- thetaSix \in [-6,6]
                thetaSix =
                    0.5 + Complex.arg z * 6 / pi

                thetaNeigh =
                    0.05

                thetaDelta =
                    abs (thetaSix - toFloat (round thetaSix)) / thetaNeigh

                powerRemainder =
                    logRadius - (toFloat <| floor logRadius)

                squished =
                    powerRemainder * 0.4 + 0.3

                l =
                    if thetaDelta < 1 then
                        squished * thetaDelta + (1 - thetaDelta)

                    else
                        squished

                { red, green, blue } =
                    Color.hsl theta 1 l
                        |> Color.toRgba

                r =
                    clamp 0 255 <| round <| red * 255

                g =
                    clamp 0 255 <| round <| green * 255

                b =
                    clamp 0 255 <| round <| blue * 255
            in
            r * 0x01000000 + g * 0x00010000 + b * 0x0100 + 0xFF
    in
    initialize2 bounds (\_ x _ y -> valueToColor <| cvalue x y e)


initialize2 : Bounds -> (Int -> Float -> Int -> Float -> Pixel) -> Canvas
initialize2 bounds f =
    Array.initialize bounds.steps
        (\sy ->
            let
                y =
                    screenToY bounds sy
            in
            Array.initialize bounds.steps
                (\sx ->
                    let
                        x =
                            screenToX bounds sx
                    in
                    f sx x sy y
                )
        )


dvalue : Float -> Float -> Expression -> Float
dvalue x y e =
    Complex.real <| cvalue x y e


cvalue : Float -> Float -> Expression -> Complex
cvalue x y e =
    Expression.value
        (Dict.fromList
            [ ( "x", Complex.fromReal x )
            , ( "y", Complex.fromReal y )
            ]
        )
        e
