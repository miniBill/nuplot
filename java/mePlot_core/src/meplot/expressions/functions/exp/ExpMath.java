package meplot.expressions.functions.exp;

import meplot.expressions.numbers.Complex;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.IComplex;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.OperationsMath;

public final class ExpMath{
	/**
	 * Ln(10).
	 */
	private static final double DLNTEN = 2.3025850929940456840;
	/**
	 * Ln(10).
	 */
	public static final Dou LNTEN = new Dou(2.3025850929940456840);

	private ExpMath(){

	}

	private static IComplex ln(final IComplex arg){
		return new Complex(ln(Math.abs(arg.norm())), arg.arg());
	}

	// TODO: improve accuracy
	public static double ln(final double arg){
		final double exponent = Double.doubleToLongBits(arg) >> 32;
		return (exponent - 1072632447.0) / 1512775.0;
	}

	public static INumber ln(final INumber arg){
		if(arg.isReal() && !arg.real().isNegative())
			return new Dou(ln(arg.toDouble()));
		return ln(arg.toComplex());
	}

	public static double cbrt(final double arg){
		if(arg > 0)
			return OperationsMath.pow(Math.E, ln(Math.abs(arg)) / 3.0);
		return -OperationsMath.pow(Math.E, ln(Math.abs(arg)) / 3.0);
	}

	public static INumber cbrt(final INumber arg){
		return exp(ln(arg).divide(Int.THREE));
	}

	public static double log(final double arg){
		return ln(arg) / DLNTEN;
	}

	private static double sqrt(final double arg){
		return Math.sqrt(arg);
	}

	public static INumber sqrt(final INumber arg){
		if(arg.isReal())
			return rsqrt(arg.real());
		final IComplex carg = arg.toComplex();
		final double norm = carg.norm();
		double argreal = carg.toDouble();
		final double real = sqrt((norm + argreal) / 2.0);
		final double immaginary = sqrt((norm - argreal) / 2.0) * sign(carg.immaginary().toDouble());
		if(immaginary == 0.0)
			return new Dou(real);
		return new Complex(real, immaginary);
	}

	private static double sign(double arg){
		if(arg > 0)
			return 1;
		if(arg < 0)
			return -1;
		return 0;
	}

	public static INumber log(final INumber arg){
		return ln(arg).divide(LNTEN);
	}

	private static IReal rsqrt(final IReal arg){
		return new Dou(Math.sqrt(arg.toDouble()));
	}

	public static double dcbrt(final IReal arg){
		final double base = arg.toDouble();
		if(base < 0)
			return -OperationsMath.pow(-base, 1.0 / 3.0);
		return OperationsMath.pow(base, 1.0 / 3.0);
	}

	public static double dcbrt(final INumber arg){
		final INumber lnarg = ln(arg);
		final INumber rexp = lnarg.divide(Int.THREE);
		return dexp(rexp);
	}

	public static double dexp(final INumber arg){
		return OperationsMath.pow(Math.E, arg);
	}

	public static double dln(final INumber arg){
		if(arg.isReal())
			return ln(arg.toDouble());
		return ln(arg.toComplex()).toDouble();
	}

	public static double dsqrt(final INumber arg){
		if(arg.isReal())
			return sqrt(arg.toDouble());
		final IComplex carg = arg.toComplex();
		final double norm = carg.norm();
		return sqrt((carg.toDouble() + norm) / 2.0);
	}

	public static INumber exp(final INumber arg){
		if(arg.isReal())
			return OperationsMath.pow(Dou.E, arg.real());
		return OperationsMath.pow(Dou.E, arg.toComplex());
	}

	public static double dlog(final INumber arg){
		return dln(arg) / DLNTEN;
	}
}
