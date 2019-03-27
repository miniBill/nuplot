package test;

import meplot.expressions.Letter;
import meplot.expressions.visitors.simplification.SimplificationHelper;

import org.junit.Test;

import test.solver.SolverUtils;

public class ExperimentalTest extends SolverUtils{
	@Test
	public void collectTest(){
		assertSimplify("ab(cd-ef)+gh(-ef+cd)", "(ab+gh)(cd-ef)");
		assertSimplify("(ab(cd-ef)+gh(-ef+cd))/(ab+gh)", "cd-ef");
	}

	@Test
	public void testGeneric(){
		checkSolve("(x+a)(x+b)=0", "!");
	}

	@Test
	public void testMixture(){
		checkSolve("xx=2,x<0", "!", true);
	}

	@Test
	public void testSquare(){
		checkSolve("[x{{w,x},{y,z}}]]xx=1", "!");
		checkSolve("[x{{r,s,t},{u,v,w},{x,y,z}]]xx=1", "!");
	}

	@Test
	public void testRoots(){
		checkRoots(Letter.X, 'x', "0");
		checkRoots(SimplificationHelper.simplify(parseOrFail("x^3-1")), 'x', "1");
		checkRoots(SimplificationHelper.simplify(parseOrFail("x^2-5x+6")), 'x', "2,3");
		checkRoots(SimplificationHelper.simplify(parseOrFail("x^2-2ax+a^2")), 'x', "a");
	}

	@Test
	public void testSolver(){
		checkSolve("y=coshx", "");
		checkSolve("sinx=0", "");
	}

	@Test
	public void testInequalities2(){
		checkSolve("ax^2+bx+c>0", "", true);
	}
}
