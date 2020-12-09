module SimplificationTest exposing (suite)

import Dict
import Expect
import Expression exposing (AssociativeOperation(..), Expression(..))
import Expression.Simplify
import Expression.Utils exposing (a, b, by, c, complex, cos_, cosh_, div, double, f, g, i, int, ipow, m, minus, n, negate_, one, plus, r, sin_, sinh_, sqrt_, square, triple, two, x, z, zero)
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        toTests ( from, to ) =
            let
                simplified =
                    Expression.Simplify.simplify from

                doublySimplified =
                    Expression.Simplify.simplify simplified
            in
            [ test (Expression.toString from ++ " simplifies to " ++ Expression.toString to) <|
                \_ ->
                    Expect.equal
                        (Expression.toString to ++ " = " ++ Debug.toString to)
                        (Expression.toString simplified ++ " = " ++ Debug.toString simplified)
            , test ("Simplification is idempotent for " ++ Expression.toString from) <|
                \_ ->
                    Expect.equal
                        (Expression.toString simplified ++ " = " ++ Debug.toString simplified)
                        (Expression.toString doublySimplified ++ " = " ++ Debug.toString doublySimplified)
            ]

        toTest ( aop, from, to ) =
            let
                sorted =
                    Expression.Simplify.sortByDegree aop from
            in
            test (Expression.toString (List from) ++ " sorts (" ++ Debug.toString aop ++ ") to " ++ Expression.toString (List to)) <|
                \_ ->
                    Expect.equal
                        (Expression.toString (List to) ++ " = " ++ Debug.toString to)
                        (Expression.toString (List sorted) ++ " = " ++ Debug.toString sorted)
    in
    describe "The Expression.Simplify module"
        [ describe "Expression.Simplify.simplify" <|
            List.concatMap toTests simplificationTests
        , describe "Expression.Simplify.sortByDegree" <|
            List.map toTest sortTests
        ]


simplificationTests : List ( Expression, Expression )
simplificationTests =
    let
        byself var =
            by [ var, var ]

        simplified s =
            ( s, s )
    in
    [ simplified a
    , simplified <| plus [ a, b ]
    , simplified <| minus a b
    , simplified <| by [ a, b ]
    , simplified <| div a b
    , simplified <| by [ a, b, c ]
    , simplified <| div (by [ a, b ]) c
    , ( by [ div a b, c ]
      , --div (by [ a, c ]) b
        by [ div a b, c ]
      )
    , ( div (div a b) c, div a <| by [ b, c ] )
    , ( div a a, one )
    , ( Replace
            (Dict.fromList
                [ ( "a", double x )
                , ( "b", triple z )
                ]
            )
            (plus [ double a, square b ])
      , plus
            [ by [ Integer 4, x ]
            , by [ Integer 9, square z ]
            ]
      )
    , ( Replace
            (Dict.singleton "a" b)
            (by [ a, a, a ])
      , ipow b 3
      )
    , ( Replace (Dict.singleton "a" one) a, one )
    , ( Replace
            (Dict.fromList
                [ ( "cov", Integer 3 )
                ]
            )
            (byself <| Variable "cov")
      , Integer 9
      )
    , simplified <| by [ two, x ]
    , ( square i, Integer -1 )
    , ( ipow i 69, i )
    , ( by [ a, negate_ <| by [ b, i ] ], negate_ <| by [ a, b, i ] )
    , ( by [ complex a b, minus a <| by [ i, b ] ], plus [ square a, square b ] )
    , simplified <|
        div
            (plus [ by [ a, x ], by [ b, x ] ])
            (by [ a, b, n ])
    , ( sinh_ <| by [ i, x ], by [ sin_ x, i ] )
    , ( cosh_ <| by [ i, x ], cos_ x )
    , simplified <| plus [ Float 0.5, triple i ]
    , ( div one i, negate_ i )
    , ( byself i, Integer -1 )
    , simplified <| negate_ a
    , ( sqrt_ <| negate_ one, i )
    , ( let
            afmn =
                by [ a, f, m, n ]
        in
        plus [ afmn, negate_ afmn ]
      , zero
      )

    {-
       , ( "5+10i", icomplex 5 10, "5 + 10i" )
       , ( "2+i", icomplex 2 1, "2 + i" )
       , ( "(5+10i)/(2+i)", div (icomplex 5 10) (icomplex 2 1), "(5 + 10i)/(2 + i)" )
       , ( "sincosx", sin_ (cos_ x), "sin(cos(x))" )
       , ( "sqrtabsx", sqrt_ (abs_ x), "sqrt(abs(x))" )
       , ( "abs(xx)", abs_ (byself x), "abs(x*x)" )
       , ( "absxx", by [ abs_ x, x ], "abs(x)*x" )
       , ( "abs(x^4)abs(sqrtx)", by [ abs_ (ipow x 4), abs_ (sqrt_ x) ], "abs(x^4)*abs(sqrt(x))" )
       , ( "sinxcosy", by [ sin_ x, cos_ y ], "sin(x)*cos(y)" )
       , ( "asinxcosy", by [ asin_ x, cos_ y ], "asin(x)*cos(y)" )
       , ( "bsinxcosy", by [ b, sin_ x, cos_ y ], "b*sin(x)*cos(y)" )
       , ( "sqrt*x", sqrt_ x, "sqrt(x)" )
       , straight "sin(-x)" <| sin_ <| negate_ x
       , straight "cos(-x)" <| cos_ <| negate_ x
       , ( "(-cos(x)abs(x)x^2)/x"
         , div
               (negate_ <| by [ cos_ x, abs_ x, square x ])
               x
         , "-(cos(x)*abs(x)*x²)/x"
         )
       , ( "x^2(-xcos(x)abs(x"
         , by [ square x, negate_ <| by [ x, cos_ x, abs_ x ] ]
         , "x²*-(x*cos(x)*abs(x))"
         )
       , ( "(2cos(x)abs(x)x^2)/x"
         , div
               (by [ two, cos_ x, abs_ x, square x ])
               x
         , "2cos(x)*abs(x)*x²/x"
         )
       , ( "-x^2+2x^2"
         , plus
               [ negate_ <| square x
               , double <| square x
               ]
         , "-(x²) + 2x²"
         )
       , ( "-x^3cosxabsx"
         , negate_ <| by [ ipow x 3, cos_ x, abs_ x ]
         , "-(x³*cos(x)*abs(x))"
         )
       , ( "2x^3cosxabsx"
         , by [ two, ipow x 3, cos_ x, abs_ x ]
         , "2x³*cos(x)*abs(x)"
         )
       , ( "-x^3cosxabsx+2x^3cosxabsx"
         , plus
               [ negate_ <| by [ ipow x 3, cos_ x, abs_ x ]
               , by [ two, ipow x 3, cos_ x, abs_ x ]
               ]
         , "-(x³*cos(x)*abs(x)) + 2x³*cos(x)*abs(x)"
         )
       , straight "(c - d)/(d - c)" <|
           div
               (minus c d)
               (minus d c)
       , straight "(-c + d)/(-d + c)²" <|
           div
               (plus [ negate_ c, d ])
               (square <| plus [ negate_ d, c ])
       , ( "(3/10)/a"
         , div
               (div (Integer 3) (Integer 10))
               a
         , "3/10/a"
         )
    -}
    {- , ( Replace
             (Dict.fromList
                 [ ( "c", plus [ div x a, div x b ] )
                 , ( "f", plus [ div (minus y one) a, div (plus [ y, one ]) b ] )
                 ]
             )
             (List [ div c n, div f n ])
       , List []
       )
    -}
    {-
         , ( "[nsqrt(cc+ff)][cx/a+x/b;f(y-1)/a+(y+1)/b]{c/n,f/n}"
         , Replace
               (Dict.fromList
                   [ ( "n", sqrt_ (plus [ byself c, byself f ]) )
                   ]
               )
           <|
               Replace
                   (Dict.fromList
                       [ ( "c", plus [ div x a, div x b ] )
                       , ( "f", plus [ div (minus y one) a, div (plus [ y, one ]) b ] )
                       ]
                   )
               <|
                   List [ div c n, div f n ]
         , "[n = sqrt(c*c + f*f)] [c = x/a + x/b; f = (y - 1)/a + (y + 1)/b] {c/n, f/n}"
         )
       , ( "[g(xx-yy)^(3/2)+1/10]gra{x/(gg),y/(gg)}"
         , let
               gg =
                   byself g
           in
           Replace
               (Dict.fromList
                   [ ( "g"
                     , plus
                           [ pow (minus (byself x) (byself y)) (div (Integer 3) two)
                           , div one <| Integer 10
                           ]
                     )
                   ]
               )
               (unaryFunc "gra" <| List [ div x gg, div y gg ])
         , "[g = (x*x - y*y)^(3/2) + 1/10] gra{x/(g*g), y/(g*g)}"
         )
    -}
    ]


sortTests : List ( AssociativeOperation, List Expression, List Expression )
sortTests =
    [ ( Addition, [ a, b ], [ a, b ] )
    , ( Addition, [ square x, x ], [ square x, x ] )
    , ( Addition, [ by [ a, x ], by [ b, x ] ], [ by [ a, x ], by [ b, x ] ] )
    , ( Addition, [ square a, square b ], [ square a, square b ] )
    , ( Multiplication, [ c, b, a ], [ a, b, c ] )
    , ( Multiplication, [ int 3, i ], [ int 3, i ] )
    , let
        afmn =
            by [ a, f, m, n ]

        agmr =
            by [ a, g, m, r ]
      in
      ( Addition, [ afmn, agmr, negate_ afmn ], [ afmn, negate_ afmn, agmr ] )
    ]
