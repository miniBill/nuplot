package test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import meplot.expressions.IValue;

import org.junit.Test;

public final class FullDoubleTest extends TestUtils{
	@Test
	public void testIsFullDouble(){
		final IValue expr = parseOrFail("sinxsiny");
		assertTrue("sinxsiny not full double", expr.isFullDouble());

		final IValue expr2 = parseOrFail("e^i");
		assertFalse("e^i full double", expr2.isFullDouble());
	}

	@Test
	public void testFullDouble(){
		final IValue expr = parseOrFail("2*3");
		final double d = expr.fdvalue('t', 0);
		checkPrecision(d, 6, 0);

		final IValue expr2 = parseOrFail("e^(ip)");
		final double d2 = expr2.fdvalue('t', 0);
		checkPrecision(d2, 1, 0);
	}
}
