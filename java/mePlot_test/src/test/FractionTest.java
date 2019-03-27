package test;

import static org.junit.Assert.assertEquals;
import meplot.expressions.numbers.Fraction;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.simplification.SimplificationHelper;

import org.junit.Test;

public class FractionTest{
	@Test
	public void testSimplification(){
		final Fraction f = new Fraction(Int.MINUSONE, new Int(-2));
		final Object sim = SimplificationHelper.simplify(f);
		assertEquals("1/2", sim.toString());
	}
}
