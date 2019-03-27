package test;

import meplot.solver.AbstractSolver;

import org.junit.Test;

public final class SimplificationTest extends TestUtils{
	@Test
	public void testCross(){
		assertSimplify("(x^2y^2)/(xy)", "xy");
		assertSimplify("(xy)/(x^2y^2)", "1/(xy)", false);
		assertSimplify("(ax+ya)/a", "x+y");
		AbstractSolver.activateCross();
		assertSimplify("(b(c-d)+a(d-c))/(c-d)", "-a+b");
		AbstractSolver.deactivateCross();
	}

	@Test
	public void testFactor(){
		assertSimplify("b^2+ba*2+a^2-4ab", "(b-a)^2");
		assertSimplify("sqrt(b^2+ba*2+a^2-4ab)", "abs(b-a)");
	}
}
