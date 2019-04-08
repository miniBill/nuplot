package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.Letter;
import meplot.expressions.operations.Lambda;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.parser.utils.Cleaner;
import meplot.solver.AbstractSolver;

import org.junit.Test;
import platform.lists.IIterable;

public final class LambdaTest extends TestUtils {
	private static Lambda lambdaOrFail(final String string) {
		final Object candidate = parseOrFail(string);
		if (candidate instanceof Lambda)
			return (Lambda) candidate;
		fail(string + " did not parse to Lambda");
		return null;
	}

	@Test
	public void testParse() {
		final Lambda toInverse = lambdaOrFail("x=>(1/x)");
		final Expression applied = toInverse.multiply(new Letter('a'));
		assertSimplify(applied, "1/a");
	}

	@Test
	public void testCompose() {
		final Lambda toInverse = lambdaOrFail("x=>(1/x)");
		final Lambda toMirror = lambdaOrFail("z=>(1-z)");
		final Expression compose1 = toInverse.multiply(toMirror);
		assertSimplify(compose1, "x=>(1/(1-x))");
		final Expression compose2 = toMirror.multiply(toInverse);
		assertSimplify(compose2, "z=>((-1+z)/z)");
		final Object scon2 = SimplificationHelper.simplify(compose2);
		if (!(scon2 instanceof Lambda))
			fail("compose2 wasn't lambda");
		final Lambda lcom2 = (Lambda) scon2;
		final Lambda brother = new Lambda('x', parseOrFail("(x-1)/x"));
		assertEquals("Changing variable doesn't make equal", lcom2, brother);
	}

	@Test
	public void testSum() {
		final Lambda toInverse = lambdaOrFail("x=>(1/x)");
		final Lambda toMirror = lambdaOrFail("z=>(1-z)");
		final ICalculable compose = toMirror.multiply(toInverse);
		final Expression inverse = new Lambda('x', parseOrFail("(1-x)/x"));
		assertSimplify(SimplificationHelper.simplify(compose.add(inverse)), "z=>0");
	}

	@Test
	public void testSpan() {
		final Expression span = parseOrFail("span(z=>(1/z))");
		final Expression sym = SimplificationHelper.simplify(span);
		assertEquals("Aww :(", "{z=>1/z,z=>z}", sym.toString());
		assertEquals("Aww2 :(", "{z=>1/z,z=>z}",
				IIterable.getLast(SimplificationHelper.stepSimplify(sym)).toString());

		final Expression span2 = parseOrFail("span(z=>(1-z),z=>(1/z))");
		AbstractSolver.activateCross();
		setLogToNormal();
		final Expression res = SimplificationHelper.simplify(span2);
		setLogToFail();
		AbstractSolver.deactivateCross();
		assertEquals("Ou?", "{z=>(1-z),z=>1/z,z=>z,z=>(z-1)/z,z=>z/(z-1)," + "z=>1/(1-z),z=>(-z)/(1-z)}",
				Cleaner.clean(res.toString()));
	}
}
