package test.parser.tokens;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import meplot.expressions.exceptions.DerivationException;
import meplot.expressions.visitors.derivative.DerivativeHelper;
import meplot.parser.tokens.FakeFunction;

import org.junit.Test;

import test.TestUtils;

public final class FakeFunctionTest extends TestUtils{
	@Test
	public void testConstructor(){
		try{
			new FakeFunction(-1);
			fail("FakeFunction built with negative arguments");
		}
		catch(final IllegalArgumentException e){
			assertEquals("java.lang.IllegalArgumentException: "
					+ "Negative number of needed arguments", e.toString());
		}
		new FakeFunction(0);
		new FakeFunction(1);
	}

	@Test
	public void testDerivatives(){
		final FakeFunction f = new FakeFunction(3);
		try{
			DerivativeHelper.derivative(f, 'x');
			fail("derivative didn't fail for FakeFunction");
		}
		catch(final DerivationException e){
			assertEquals(e.toString(),
					"meplot.expressions.exceptions.DerivationException: Fakefunction derivative");
		}
		try{
			DerivativeHelper.stepDerivative(f, 'y');
			fail("stepDerivative didn't fail for FakeFunction");
		}
		catch(final DerivationException e){
			assertEquals(e.toString(),
					"meplot.expressions.exceptions.DerivationException: Fakefunction stepDerivative");
		}
	}
}
