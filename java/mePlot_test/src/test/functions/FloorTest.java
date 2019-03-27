package test.functions;

import static org.junit.Assert.assertEquals;
import meplot.expressions.Expression;
import meplot.expressions.visitors.simplification.SimplificationHelper;

import org.junit.Test;

import test.TestUtils;

public class FloorTest extends TestUtils{
	@Test
	public void testValue(){
		final Expression flo = parseOrFail("floor.4");
		final Object sim = SimplificationHelper.simplify(flo);
		assertEquals("0", sim.toString());

		final Expression flo2 = parseOrFail("floori");
		final Object sim2 = SimplificationHelper.simplify(flo2);
		assertEquals("0", sim2.toString());
	}
}
