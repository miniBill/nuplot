package meplot.expressions.operations;

import meplot.expressions.functions.FunctionsMath;
import meplot.expressions.functions.exp.ExpMath;
import meplot.expressions.functions.trig.TrigMath;
import meplot.expressions.numbers.Complex;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.Fraction;
import meplot.expressions.numbers.IComplex;
import meplot.expressions.numbers.IInt;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;

public final class OperationsMath{
	public static double dpow(final INumber base, final INumber exp){
		if(base.isReal() && exp.isReal())
			return dpow(base.real(), exp.real());
		final INumber nexp = base.multiply(ExpMath.ln(exp));
		if(nexp.isReal())
			return pow(Math.E, nexp.real());
		return pow(Dou.E, nexp.toComplex()).toDouble();
	}

	private static double dpow(final IReal base, final IReal exp){
		if(exp instanceof Int)
			return intPow(base.toDouble(), ((Int)exp).getValue());

		if(exp instanceof Fraction){
			final Fraction fexp = (Fraction)exp;
			final int expnum = fexp.fgetNumerator().getValue();
			final int expden = fexp.fgetDenominator().getValue();
			final double nbase = intPow(base.toDouble(), expnum);
			if(nbase < 0){
				if(expden % 2 == 0)
					return Double.NaN;
				return -pow(-nbase, 1.0 / expden);
			}
			return pow(nbase, 1.0 / expden);
		}

		return pow(base.toDouble(), exp.toDouble());
	}

	public static double exp(final double exp){
		final int bigExp = FunctionsMath.floor(exp);
		final double smallResult = smallexp(exp - bigExp);
		final double bigResult = intPow(Math.E, bigExp);
		return smallResult * bigResult;
	}

	/**
	 * pow(base,exponent).
	 * 
	 * @param exp
	 */
	private static double intPow(double base, int exp){
		if(exp == 0)
			return 1;
		if(exp < 0)
			return 1.0 / intPow(base, -exp);

		double result = 1;
		while(exp > 0){
			if((exp & 1) != 0)
				result *= base;
			base *= base;
			exp /= 2;
		}
		return result;
	}

	public static double max(final double arg0, final double arg1){
		return arg0 > arg1 ? arg0 : arg1;
	}

	public static INumber max(final INumber arg0, final INumber arg1){
		return arg0.greaterThan(arg1) ? arg0 : arg1;
	}

	public static double min(final double arg0, final double arg1){
		return arg0 < arg1 ? arg0 : arg1;
	}

	public static INumber min(final INumber arg0, final INumber arg1){
		return arg0.lessThan(arg1) ? arg0 : arg1;
	}

	public static int modpow(int base, int exp, final int mod){
		int result = 1;
		while(exp > 0){
			if((exp & 1) == 1)
				result = result * base % mod;
			exp >>= 1;
			base = base * base % mod;
		}
		return result;
	}

	public static double pow(final double base, final double exp){
		if(base < 0)
			return pow(base * base, exp / 2) * TrigMath.cos(exp * Math.PI);

		if(exp < 0)
			return 1.0 / pow(base, -exp);

		// better approximation for small exponents
		if(Math.abs(exp) < 0.00000001)
			return exp / 10.0 + 1.0;

		if(exp - FunctionsMath.floor(exp) == 0)
			return intPow(base, (int)exp);

		return innerPow(base, exp);
	}

	private static double innerPow(final double base, final double exp){
		final double newExp = ExpMath.ln(base) * exp;
		if(newExp > 0)
			return exp(newExp);
		return 1.0 / exp(-newExp);
	}

	public static double pow(final double base, final INumber exp){
		if(exp.isReal())
			return pow(base, exp.real());
		final INumber lexp = ExpMath.ln(exp);
		final INumber nexp = new Dou(base).multiply(lexp);
		return pow(Dou.E, nexp).toDouble();
	}

	private static double pow(final double base, final IReal exp){
		if(exp.isInt())
			return intPow(base, exp.intify().getValue());
		return pow(base, exp.toDouble());
	}

	public static int pow(final int base, final int exp){
		if(exp == 0)
			return 1;
		int curr = base;
		for(int i = 1; i < exp; i++)
			curr *= base;
		return curr;
	}

	public static IInt pow(final int base, final IInt exp){
		return new Int(pow(base, exp.getValue()));
	}

	private static IInt pow(final IInt base, final IInt exp){
		return pow(base.getValue(), exp);
	}

	private static INumber pow(final INumber base, final IInt exponent){
		if(exponent.isZero())
			return Int.ONE;
		INumber toret = base;
		for(int i = 1; i < exponent.getValue(); i++)
			toret = toret.multiply(base);
		return toret;
	}

	public static INumber pow(final INumber base, final INumber exp){
		if(base.isReal() && exp.isReal())
			return pow(base.real(), exp.real());
		if(exp instanceof Int)
			return pow(base, (Int)exp);
		final INumber nexp = exp.multiply(ExpMath.ln(base));
		if(nexp.isReal())
			return new Dou(pow(Math.E, nexp.real()));
		return pow(Dou.E, nexp.toComplex());
	}

	private static INumber pow(final IReal base, final IComplex exp){
		final INumber real = pow(base, exp.real());
		final double cosx = TrigMath.cos(exp.immaginary().toDouble());
		final double sinx = TrigMath.sin(exp.immaginary().toDouble());
		final Complex imm = new Complex(cosx, sinx);
		return imm.multiply(real);
	}

	public static IReal pow(final IReal base, final IReal exp){
		if(exp.isInt()){
			if(base.isInt())
				return pow(base.intify(), exp.intify());
			return new Dou(intPow(base.toDouble(), exp.intify().getValue()));
		}
		return new Dou(pow(base.toDouble(), exp.toDouble()));
	}

	private static double smallexp(final double arg){
		double temp = 1.0 + arg / 256.0;
		temp *= temp;
		temp *= temp;
		temp *= temp;
		temp *= temp;
		temp *= temp;
		temp *= temp;
		temp *= temp;
		temp *= temp;
		return temp;
	}

	private OperationsMath(){
	}
}
