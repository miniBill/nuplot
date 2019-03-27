package test.expressions.functions.piecewise;

import org.junit.Test;

import test.TestUtils;

public class AbsTest extends TestUtils{
	@Test
	public void testAbs(){
		justParse("sqrt(x^2)", "abs(x)");
		justParse("abssqrtx", "sqrt(x)");
		justParse("(absx)^6", "x^6");
		justParse("abs(x^6)", "x^6");
		justParse("absabsx", "abs(x)");
		justParse("abs1", "1");
		justParse("abs(-1)", "1");
		checkWithDerivatives("abs(x-1)", "sign(x-1)", "?", 'x');
		assertSimplify("[a=xx+yy-1]absa/a", "abs(x^2+y^2-1)/(x^2+y^2-1)");
	}
}
