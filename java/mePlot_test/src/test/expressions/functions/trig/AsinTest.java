package test.expressions.functions.trig;

import static org.junit.Assert.assertEquals;
import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.functions.trig.Asin;
import meplot.expressions.functions.trig.TrigMath;
import meplot.expressions.numbers.Complex;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;

import org.junit.Test;

import test.TestUtils;

public final class AsinTest extends TestUtils{
	@Test
	public void testParse(){
		final Expression input = parseOrFail("asinx");
		final Expression value = new Asin(Letter.X);
		assertEquals("Ohnoes", value.toFullString(), input.toFullString());
	}

	@Test
	public void testComplex(){
		INumber arg = Complex.I.multiply(Int.THREE).add(Int.ONE);
		INumber result = TrigMath.asin(arg);
		assertSimplify(result, "0.30431750935047985+1.8727504090165425i");
	}

	@Test
	public void testPrecision(){
		checkAsinPrecision(0, 0);
		checkAsinPrecision(0.1, 0.1);
		checkAsinPrecision(-0.1, 0.1);
		checkAsinPrecision(0.9, 1.61);
		checkAsinPrecision(-0.9, 1.61);
	}

	private static void checkAsinPrecision(double input, double precision){
		checkPrecision(TrigMath.asin(Math.sin(input)), input, precision);
	}
}
