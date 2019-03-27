package test;

import static org.junit.Assert.assertEquals;
import meplot.expressions.IValue;
import meplot.expressions.Letter;
import meplot.expressions.functions.exp.ExpMath;
import meplot.expressions.functions.operations.Integral;
import meplot.expressions.functions.trig.TrigMath;
import meplot.expressions.list.ValueList;
import meplot.expressions.numbers.Complex;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.IComplex;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.OperationsMath;

import org.junit.Test;

public class PrecisionTest extends TestUtils{
	private static void checkPrecision(final IValue val, final double target,
			final double percent){
		checkPrecision(val.value(ValueList.EMPTY).toDouble(), target, percent);
	}

	private static void checkPrecision(final String string, final double target,
			final double percent){
		checkPrecision(parseOrFail(string), target, percent);
	}

	@Test
	public void testPrecision(){
		final double e = Math.E;
		checkPrecision(ExpMath.ln(e), 1, 1.78);
		checkPrecision(OperationsMath.exp(2), e * e, 0.00);
		checkPrecision(OperationsMath.pow(2, 3), 8, 0.00);
		checkPrecision(OperationsMath.pow(2, 0), 1, 0.00);
		checkPow(2, 0.5, 2.01);
		checkPow(0.5, 5, 0);
		checkPow(2.3, 3.5, 1.39);
		checkPow(5.3, 5.5, 8.40);
		checkPow(5.3, 1e-8, 1e-7);
		checkPow(e, 0.001, 0.01);
		checkPow(e, 4.234, 7.24);
		checkPow(0.5, 2.5, 10.66);
		checkPow(0.5, 5e20, 0);
		checkPrecision(ExpMath.ln(0.5), Math.log(0.5), 5.80);
		checkPrecision(OperationsMath.exp(-0.69), Math.exp(-0.69), 0.02);
		checkPrecision(OperationsMath.exp(-2.1), Math.exp(-2.1), 0.16);
		checkPrecision(OperationsMath.exp(-3.1), Math.exp(-3.1), 0.16);
		final Complex arg = new Complex(0.3, 0.5);
		checkPrecision(TrigMath.sin(arg).toDouble(), 0.33, 1);
		checkPrecision(TrigMath.cos(arg).toDouble(), 1.07, 0.66);
		checkPrecision(ExpMath.sqrt(new Dou(2)), Math.sqrt(2.0), 0);
		checkPrecision(parseOrFail("sin(-5.0+iy)").value('y', 5.0).toDouble(), 71.16, 1);
		checkPrecision("arg(i+1)", Math.sqrt(2) / 2.0, 11.08);
		checkPrecision("ii(2x,x,0,1)", 1, 0.01);
		checkPrecision("log1000", 3, 0.46);
		final double check = new Integral(Letter.X, Letter.X, Int.ZERO, Int.TWO).dvalue(
				'x', -12);
		assertEquals(2, check, 0.000000000000001);
		justParse("0.3");

		checkPrecision("im(2+3i)", 3, 0.0);
		checkPrecision("re(1/2+5i)", 0.5, 0.0);

		checkPrecision(OperationsMath.pow(-8, 2.0 / 3.0), -2, 1.32);

		final IComplex ip = Complex.I.multiply(new Dou(Math.PI));
		checkPrecision(ExpMath.exp(ip), -1, 1);

		checkPrecision(parseOrFail("cbrt(-125)").dvalue(), -5, 0.98);
		checkPrecision(parseOrFail("(-125)^(1/3)").dvalue(), -5, 0.98);
	}

	private static void checkPow(final double lbase, final double lexp,
			final double precision){
		checkPrecision(OperationsMath.pow(lbase, lexp), Math.pow(lbase, lexp), precision);
	}
}
