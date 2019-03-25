module ParserTest exposing (suite)

import Expect
import Expression exposing (BinaryOperation(..), Expression(..), minus, plus)
import Parser
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        toTest ( from, to ) =
            test ("has the same behaviour of the java one on " ++ from) <|
                \_ ->
                    let
                        parsed =
                            Parser.parse from
                    in
                    case parsed of
                        Ok e ->
                            Expression.equals e to
                                |> Expect.true "correctly parsed"

                        Err e ->
                            Expect.fail e
    in
    describe "The Parser module"
        [ describe "Parser.parse" <|
            List.map toTest tests
        ]


tests : List ( String, Expression )
tests =
    let
        a =
            Variable "a"

        b =
            Variable "b"

        c =
            Variable "c"

        f =
            Variable "f"

        n =
            Variable "n"

        x =
            Variable "x"

        y =
            Variable "y"

        z =
            Variable "z"
    in
    [ ( "[a=2x;b=3z]2a+b^2"
      , Replace
            [ ( "a", double x ), ( "b", triple z ) ]
            (plus [ double a, square b ])
      )
    , ( "[a2x;b3z]2a+b^2"
      , Replace
            [ ( "a", double x ), ( "b", triple z ) ]
            (plus [ double a, square b ])
      )
    , ( "[ab]aaa"
      , Replace
            [ ( "a", b ) ]
            (by [ a, a, a ])
      )
    , ( "(bx+ax)/(abn)"
      , div
            (plus [ by [ b, x ], by [ a, x ] ])
            (by [ a, b, n ])
      )
    , ( "[cx/a+x/b;f(y-1)/a+(y+1)/b]{c/n,f/n}"
      , List
            [ div
                (plus [ by [ b, x ], by [ a, x ] ])
                (by [ a, b, n ])
            , div
                (plus [ by [ b, y ], minus b, by [ a, y ], a ])
                (by [ a, b, n ])
            ]
      )
    , ( "[nsqrt(cc+ff)][cx/a+x/b;f(y-1)/a+(y+1)/b]{c/n,f/n}"
      , List
            [ div
                (plus [ by [ b, x ], by [ a, x ] ])
                (by [ a, b, sqroot <| plus [ square c, square f ] ])
            , div
                (plus [ by [ b, y ], minus b, by [ a, y ], a ])
                (by [ a, b, sqroot <| plus [ square c, square f ] ])
            ]
      )
    ]



{-
   parseOrFail("[g(xx-yy)^(3/2)+1/10]gra{x/(gg),y/(gg)}");
   justParse("2x");
   assertSimplify("sinh(ix)", "isin(x)");
   assertSimplify("cosh(ix)", "cos(x)");
   assertSimplify("(a+ib)(a-ib)", "a^2+b^2");
   assertSimplify("i^2", "-1");
   assertSimplify("i^69", "i");
   justParse("0.5+3i");
   assertSimplify("1/i", "-i");
   assertSimplify("i*i", "-1");
   assertSimplify("sqrt(-1)", "i");
   final ICalculable numerator = parseOrFail("5+10i");
   final Expression denominator = parseOrFail("2+i");
   assertSimplify(SimplificationHelper.simplify(numerator.divide(denominator)), "4+3i");
   justParse("sincosx", "sin(cos(x))");
   justParse("sqrtabsx", "sqrt(abs(x))");
   justParse("abs(xx)", "x^2");
   justParse("abs(x^4)abs(sqrtx)", "x^4sqrt(x)");
   justParse("sinxcosy", "sin(x)cos(y)");
   assertSimplify("sin(-x)", "-sinx");
   assertSimplify("cos(-x)", "cosx");
   assertSimplify("(-cos(x)abs(x)x^2)/x", "-xcosxabsx");
   assertSimplify("x^2(-xcos(x)abs(x)", "-x^3cosxabsx");
   assertSimplify("(2cos(x)abs(x)x^2)/x", "2xcosxabsx");
   assertSimplify("-x^2+2x^2", "x^2");
   assertSimplify("-x^3cosxabsx+2x^3cosxabsx", "x^3cosxabsx");
   setLogToNormal();
   assertEquals("Default not honored", Parser.parseOrDefault("^", Int.MINUSONE), Int.MINUSONE);
   setLogToFail();
   assertSimplify("(c-d)/(d-c)", "-1");
   final ICalculable n = parseOrFail("-c+d");
   final Expression d = parseOrFail("(-d+c)^2");
   assertTrue("Couldn't divide :(", n.compatible(d, Operation.DIVISION));
   assertSimplify("(-c+d)/(-d+c)^2", "-1/(-d+c)");
   assertSimplify("(3/10)/a", "3/(10*a)");
   assertSimplify("(3-2x)/(1-x)^2)", "(3+-2x)/((1+-x)^2)");
   assertSimplify("ddx^2,x", "2x");
   assertSimplify("dd(x^2,x)", "2x");
   assertSimplify("ddxsinx,x", "sin(x)+xcos(x)");
   assertSimplify("ddsin(x^2),x", "2xcos(x^2)");
   assertSimplify("ddsin(x^2),x", "2xcos(x^2)");
   justParse("ii(ln(a),a,b,c)");
   Expression integral = parseOrFail("ii(ln(t),t,1,a)");
   assertEquals("Fundamental theorem", "ln(a)", SimplificationHelper.simplify(derivativeOrFail(integral, 'a'))
   .toString());
   integral = parseOrFail("ii(ln(t),t,a+1,a^2)");
   final Object integralSim = parseOrFail("2aln(a^2)-ln(a+1)");
   assertSimplify(derivativeOrFail(integral, 'a').toString(), integralSim.toString());
   justParse("ii(ln(x),x,0,x+1)");
   justParse("ii(lnx,x,0,x+1)", "ii(ln(x),x,0,x+1)");
   justParse("iiln(x),x,0,x+1", "ii(ln(x),x,0,x)+1");
   justParse("ii(expx,x,lnx,x)", "ii(e^x,x,ln(x),x)");
   assertSimplify("ii(a,x,0,x)", "ax");
   checkWithDerivatives("ln(x+sqrt(x^2+1))", "sqrt(x^2+1)/abs(x^2+1)", "?", 'x');
   justParse("a=b", "a=b");
   justParse("a<b", "a<b");
   justParse("a>b", "a>b");
   justParse("a<=b", "a≤b");
   justParse("a>=b", "a≥b");
   assertSimplify("ea", "ae");
   justParse("sin(x)cos(x)");
   justParse("cos(x)sin(x)");
   assertSimplify("sinxcosx-cosxsinx", "0");
   checkWithDerivatives("sinxcosx", "sin(x)cos(x)", "cos(x)^2-sin(x)^2", "-4sin(x)cos(x)", 'x');
   assertSimplify("xx+1", "x^2+1");
   final IValue dzero = justParse("1/x");
   final double dval = dzero.value('x', 0).toDouble();
   assertTrue("Implicit div0 is not Infinity", Double.isInfinite(dval));
   final IValue explicitZero = parseOrFail("1/0");
   final double explicitVal = explicitZero.dvalue();
   assertTrue("Explicit div0 is not Infinity", Double.isInfinite(explicitVal));
   final IValue explicitSZero = justParse("1/0");
   final double explicitSVal = explicitSZero.dvalue();
   assertTrue("Explicit simplfied div0 is not NaN", Double.isInfinite(explicitSVal));
   checkWithDerivatives("e^x", "e^x", "?", 'x');
   checkWithDerivatives("ln(ex)", "1/x", "-1/x^2", 'x');
   assertSimplify("exp(x)", "e^x");
   assertSimplify("(a^(b+c))/(a^b)", "a^c");
   assertSimplify("e^(lna)", "a");
   assertSimplify("ln(e^a)", "a");
   justParse("e^(-xx)", "e^(-x^2)");
   justParse("y=x", "y=x");
   justParse("x=y", "x=y");
   justParse("y=xx", "y=x^2");
   justParse("xx=y", "x^2=y");
   justParse("x-1=0", "x-1=0");
   justParse("x=-x", "x=-x");
   justParse("x=-1", "x=-1");
   justParse("[]a", "a");
   justParse("1/1", "1");
   justParse("(1/1 )", "1");
   justParse("x(1/1)", "x");
   justParse("1/(a+sqrt(b))");
   justParse("x(1/(a+sqrt(b))", "x/(a+sqrt(b))");
   justParse("x (1/(a-sqrt(b)))*(1/1)", "x/(a-sqrt(b))");
   final Expression expr = parseOrFail("x (1/(a+sqrt(b)))*(1/(a-sqrt(b))");
   final Object exprSim = SimplificationHelper.simplify(expr);
   final String exprSimString = exprSim.toString();
   final String cleaned = Cleaner.clean(exprSimString);
   assertEquals("Simplification failed", "x/(a^2-abs(b))", cleaned);
   justParse("+", "0");
   assertException("(");
   assertException("[");
   assertException("{");
   assertException("*a");
   assertException("a*");
   assertException("ln");
   assertException("..");
   assertException("@@");
   assertException("@-");
   assertException("");
   assertException("ln+");
   assertException("[a]");
   assertException("[a=]");
   assertException("sin");
   assertException("iisqrtx");
   assertException("1000000000000000000000");
   assertSimplify("sqrt(1/4)", "1/2", false);
   assertSimplify("'(1/4)^2-(.25)^.5", "-7/16", false);
   justParse("a+a", "2a");
   justParse("a*a", "a^2");
   justParse("a/a", "1");
   justParse("a-a", "0");
   justParse("a^a", "a^a");
   assertSimplify("-(-d+c)^-1", "-1/(-d+c)");
   justParse("-2+1", "-1");
   justParse("0-0", "0");
   justParse("1/3+1/2", "5/6");
   justParse("1/2-1/3", "1/6");
   final ISubstitutible sum = parseOrFail("a+b");
   final Expression partial = sum.partialSubstitute('a', 0.5);
   checkSimplify(partial, "0.5+b");
   checkWithDerivatives("lnx+lne", "ln(x)+1", "1/x", "-1/x^2", 'x');
   justParse("1/ln(1/x)");
   final Matrix matA = matrixOrFail("{{1,2},{3,4}}");
   final Expression matAI = matA.inverse();
   ICalculable matI = SimplificationHelper.simplify(matA.multiply(matAI));
   assertTrue("A*Inverse didn't give 1", matI.isOne());
   matI = SimplificationHelper.simplify(matAI.multiply(matA));
   assertTrue("Inverse*A didn't give 1", matI.isOne());
   justParse("{{a,b},{c,d}}");
   assertSimplify("det{{a,b},{c,d}}", "-bc+ad");
   justParse("{{a,b},{c,d", "{{a,b},{c,d}}");
   assertSimplify("{{a,b},{c,d}}*{{e,f},{g,h}}", "{{ae+bg,af+bh},{ce+dg,cf+dh}}");
   assertSimplify("{{a,b},{c,d}}+{{e,f},{g,h}}", "{{a+e,b+f},{c+g,d+h}}");
   assertSimplify("{{a,b},{c,d}}+{{e,f},{g,h}}+{{e,b},{g,d}}", "{{a+2e,2b+f},{c+2g,2d+h}}");
   assertSimplify("[a{{a,b},{c,d}}]aa^-1", "{{1,0},{0,1}}");
   assertSimplify("[a{{a,b},{c,d}}](1*a)a^-1", "{{1,0},{0,1}}");
   justParse("a/+b", "a/b");
   justParse("(-1)^x");
   justParse("b(-1)^x");
   justParse("-1/x^2");
   assertSimplify("norm(2,x,y)", "sqrt(x^2+y^2)");
   assertSimplify("norm(3,x,y)", "(abs(x)^3+abs(y)^3)^(1/3)");
   assertSimplify("norm(4,x,y)", "(x^4+y^4)^(1/4)");
   justParse("(a-b)(a+b)", "a^2-b^2");
   justParse("(a+b)(a+b)", "(a+b)^2");
   justParse("aa", "a^2");
   justParse("aabb", "b^2a^2");// TODO: Fix
   justParse("c(a+b)");
   assertSimplify("a++a", "2a");
   justParse("a+-b", "a-b");
   justParse("a-+b", "a-b");
   justParse("a+++++-+-+b", "a+b");
   assertSimplify("(a+b)+(c+d)", "a+b+c+d");
   final ICalculable left = justParse("a+b");
   final Expression right = justParse("c+d");
   assertSimplify(left.add(right), "a+b+c+d");
   assertSimplify("bc/c", "b");
   assertSimplify("a/-1", "-a");
   assertSimplify("a/-b", "-a/b");
   assertSimplify("a*-b", "-a*b");
   assertSimplify("0@3", "1/3");
   assertSimplify("@3", "1/3");
   assertSimplify("@", "0");
   assertSimplify(".", "0.0");
   assertSimplify("'.", "0", false);
   checkWithDerivatives("pw(x>0,e^(-x)-1,1/(x+1))", "pw(x>0,-e^(-x),-1/(x+1)^2)", "pw(x>0,e^(-x),2/(x+1)^3)", 'x');
   checkWithDerivatives("pwx>0,1/ln(1/x),sqrt(4-e^x)", "pw(x>0,1/ln(1/x),sqrt(4-e^x))",
   "pw(x>0,1/(xln(1/x)^2),(-e^x)/(2sqrt(4-e^x)))", "?", 'x');
   assertSimplify("(3-2x)/(1-x^2)", "(3-2x)/(1-x^2)");
   assertSimplify("(x^2)^2", "x^4");
   justParse("x^(1/3)");
   assertSimplify("e^(ip)+1", "0");
   assertSimplify("a^(2-1*1)", "a");
   checkWithDerivatives("e^sin(px)", "e^(sin(px))", "pcos(px)e^(sin(px))",
   "p(pcos(px)^2e^(sin(px))-psin(px)e^(sin(px)))", 'x');
   assertSimplify("(2/3x)^2", "4/9x^2");
   final IValue expr = parseOrFail("(2+3i)^(0.5)");
   final INumber val = expr.value(ValueList.EMPTY);
   assertEquals("Explicit power failed",
   SimplificationHelper.simplify(parseOrFail("1.6610457949760584+0.8861611175861872i")), val);
   final double dval = expr.dvalue();
   assertEquals(-0.10493601005240251, dval, 0.00000000000000001);
   checkWithDerivatives("e^tanx", "e^(sin(x)/cos(x))", "e^(sin(x)/cos(x))/cos(x)^2", "?", 'x');
   final IValue sqrt = parseOrFail("sqrtx");
   checkPrecision(sqrt.dvalue('x', 9.0), 3.0, 0.00);
   assertSimplify("e^tanx", "e^(sin(x)/cos(x))");
   justParse("2+2*2", "6");
   justParse("2/2*2", "2");
   justParse("2*2/2", "2");
   checkWithDerivatives("s+tv+1/2at^2", "v+at", "a", 't');
   checkWithDerivatives("sinhx", "(e^x-e^(-x))/2", "1/2e^x+1/2e^(-x)", "1/2e^x-1/2e^(-x)", 'x');
   assertSimplify("sqrt(x)^2", "absx");
   assertSimplify("-sqrt(5/6)", "-1/6sqrt(30)");
   setLogToNormal();
   final Expression matA = parseOrFail("{{a},{b},{c}}");
   final ISubstitutible tosub = parseOrFail("aa");
   final Expression subbed = tosub.partialSubstitute('a', matA);
   SimplificationHelper.simplify(subbed); // this used to trigger an error
   setLogToFail();
   assertSimplify("sin(0)", "0");
   assertSimplify("sin(x)^2+cos(x)^2", "1");
   checkWithDerivatives("sin(x)/cos(x)", "1/cos(x)^2", "(2sin(x))/cos(x)^3", 'x');
   UserFunction ball = null;
   try{
   ball = Parser.parseUserFunction("ball(r,d,x,y):=norm(d,x,y)<r");
   catch(final ParserException e){
   fail(e.toString());
   FunctionToken.setUserFunctions(new UserFunctionList(ball));
   assertSimplify("ball(r,2,x,y)", "sqrt(x^2+y^2)<r");
   // clean up
   FunctionToken.setUserFunctions(new UserFunctionList());
   checkWithDerivatives("x", "1", "0", 'x');
   assertSimplify("(x^2x)/(x^2)", "x");
   assertSimplify("(x^2)^2/(xx)", "x^2");
   checkWithDerivatives("atan(y/x)", "(-y)/(y^2+x^2)", "(2xy)/(y^2+x^2)^2", 'x');
   checkWithDerivatives("xatan(y/x)", "(atan(y/x)y^2+atan(y/x)x^2-xy)/(y^2+x^2)", "?", 'x');
   checkWithDerivatives("xsinx", "xsin(x)", "sin(x)+xcos(x)", "2cos(x)-xsin(x)", 'x');
   assertSimplify("tanxcosx", "sinx");
   checkWithDerivatives("x^2", "2x", "2", 'x');
   assertSimplify("x^2x^3", "x^5");
   justParse("x^x");
   justParse("ln(x)");
   justParse("ln(x)x^x");
   justParse("x^x+ln(x)x^x");
   checkWithDerivatives("x^x", "x^x+ln(x)x^x", "?", 'x');
   checkWithDerivatives("0x+3y+xx", "3y+x^2", "2x", "2", 'x');
   checkWithDerivatives("ax^2+bx+c", "2ax+b", "2a", 'x');

-}
