package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import meplot.expressions.Expression;
import meplot.expressions.other.Monomial;
import meplot.expressions.other.Poly;
import meplot.expressions.visitors.simplification.SimplificationHelper;

import org.junit.Test;

public class MonomialTest extends TestUtils{
	@Test
	public void testLeading(){
		final Expression arg = parseOrFail("1+-x^2");
		final Expression sim = SimplificationHelper.simplify(arg);
		if(!Poly.isPoly(sim, 'x'))
			fail("1+-x^2 is not a poly?");
		final Poly poly = new Poly(sim, 'x');
		final Object lead = poly.getLeadingTerm();
		assertEquals("-x^2", lead.toString());
	}

	@Test
	public void testCoeff(){
		final Monomial m = new Monomial(parseOrFail("-1/2x^1"), 'x');
		final Object ex = m.partialSimplify();
		assertEquals("-1/2x", ex.toString());
	}

	@Test
	public void testExponent(){
		final Monomial x = new Monomial(parseOrFail("x"), 'x');
		assertEquals(x.getExponent(), 1);
		final Monomial xs = new Monomial(parseOrFail("xx"), 'x');
		assertEquals(xs.getExponent(), 2);
		final Monomial xc = new Monomial(parseOrFail("xxx"), 'x');
		assertEquals(xc.getExponent(), 3);
		final Monomial ax = new Monomial(parseOrFail("ax"), 'x');
		assertEquals(ax.getExponent(), 1);
	}
}
