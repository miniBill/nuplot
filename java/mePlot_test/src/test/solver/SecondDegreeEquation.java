package test.solver;

import org.junit.Test;

public final class SecondDegreeEquation extends SolverUtils{
	@Test
	public void testGeneric(){
		checkSolve("axx+bx+c=0", "Passaggi:\naxx+bx+c=0\nax^2+bx+c=0\n°x=(-b+sqrt(b^2-4ac))/(2a)\n"
				+ "|x=(sqrt(b^2-4ac)-b)/(2a)\n°x=(-b-sqrt(b^2-4ac))/(2a)");
	}
}
