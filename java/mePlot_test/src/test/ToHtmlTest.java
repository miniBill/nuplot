package test;

import static org.junit.Assert.assertEquals;
import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.BooleanOp;
import meplot.solver.Solution;
import meplot.solver.Solver;

import org.junit.Test;

public final class ToHtmlTest extends TestUtils{
	@Test
	public void testMatrix(){
		final Matrix a = matrixOrFail("{{x,y},{z,w}}");
		final Expression expr = new BooleanOp(a.multiply(a.minverse()), '=', Int.ONE);
		final Solver s = new Solver();
		final Solution solution = s.solve(expr);
		final StringBuffer buffer = new StringBuffer();
		solution.toHtml(buffer);
		assertEquals("Passaggi:<br/>\n"
				+ "${(\\table {{{{w}{x}}{-{z}{y}}}/{{{w}{x}}{-{y}{z}}}},{{0}/{{{w}{x}}{-{y}{z}}}};"
				+ " {{0}/{{{w}{x}}{-{y}{z}}}},{{{-{y}{z}}+{{x}{w}}}/{{{w}{x}}{-{y}{z}}}})}={1}$<br/>\n"
				+ "${(\\table {{{{w}{x}}{-{z}{y}}{-{w}{x}}+{{y}{z}}}/{{{w}{x}}{-{y}{z}}}},"
				+ "{{{{w}{x}*{0}}{-{y}{z}*{0}}}/{{{w}{x}}{-{y}{z}}}};"
				+ " {{{{w}{x}*{0}}{-{y}{z}*{0}}}/{{{w}{x}}{-{y}{z}}}},"
				+ "{{{-{y}{z}}+{{x}{w}}{-{w}{x}}+{{y}{z}}}/{{{w}{x}}{-{y}{z}}}})}={0}$<br/>\n"
				+ "${(\\table {{0}/{{{w}{x}}{-{y}{z}}}},{{}/{{{w}{x}}{-{y}{z}}}};"
				+ " {{}/{{{w}{x}}{-{y}{z}}}},{{0}/{{{w}{x}}{-{y}{z}}}})}={0}$<br/>\n"
				+ "${(\\table {0},{{0}/{{{w}{x}}{-{y}{z}}}}; {{0}/{{{w}{x}}{-{y}{z}}}},{0})}={0}$<br/>\n"
				+ "${(\\table {0},{0}; {0},{0})}={0}$<br/>\n"
				+ "$\\{\\table {{0}={0}}; {{0}={0}}; {{0}={0}}; {{0}={0}}$<br/>\n"
				+ "$\\{\\table {∀}; {{0}={0}}; {{0}={0}}; {{0}={0}}$<br/>\n"
				+ "$\\{\\table {∀}; {∀}; {{0}={0}}; {{0}={0}}$<br/>\n" + "$\\{\\table {∀}; {∀}; {∀}; {{0}={0}}$<br/>\n"
				+ "$\\{\\table {∀}; {∀}; {∀}; {∀}$<br/>\n" + "$∀$<br/>\n" + "", buffer.toString());
	}

	@Test
	public void testMatrix2(){
		final Expression a = parseOrFail("x^{{a,b},{c,d}}=0");
		final StringBuffer buffer = new StringBuffer();
		a.toHtml(buffer);
		assertEquals("{{x}^{(\\table {a},{b}; {c},{d})}}={0}", buffer.toString());
	}

	@Test
	public void testMatrix3(){
		final Expression a = parseOrFail("[a{{x,y},{z,w}}]aa=1");
		final StringBuffer buffer = new StringBuffer();
		a.toHtml(buffer);
		assertEquals("{{(\\table {x},{y}; {z},{w})}{(\\table {x},{y}; {z},{w})}}={1}", buffer.toString());
	}
}
