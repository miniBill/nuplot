module Glsl.Constants exposing
    ( gl_FragColor, gl_FragCoord
    , zero, one, minusOne
    )

{-|


# Variables

@docs gl_FragColor, gl_FragCoord


# Numbers

@docs zero, one, minusOne

-}

import Glsl exposing (Expression, Float_, Vec4, float1, unsafeVar)


gl_FragColor : Expression Vec4
gl_FragColor =
    unsafeVar "gl_FragColor"


gl_FragCoord : Expression Vec4
gl_FragCoord =
    unsafeVar "gl_FragCoord"


zero : Expression Float_
zero =
    float1 0


one : Expression Float_
one =
    float1 1


minusOne : Expression Float_
minusOne =
    float1 -1
