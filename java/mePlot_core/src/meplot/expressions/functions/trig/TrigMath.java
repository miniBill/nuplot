package meplot.expressions.functions.trig;

import meplot.expressions.functions.exp.ExpMath;
import meplot.expressions.numbers.Complex;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.IComplex;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.IReal;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.OperationsMath;

public final class TrigMath{
	private static final double ATANCOEFF = Math.PI / 4d;

	public static double asin(final double coeff) {
		if (coeff < -1 || coeff > 1)
			return Double.NaN;
		final double xs = coeff * coeff;
		final double xq = xs * xs;
		double left = 622.273 + 8108.04 * xs + 5309.75 * xq;
		double right = coeff * (3550.56 + 9272.39 * xs + 1218.58 * xq);
		if (coeff < -0.8)
			return right + left;
		if (coeff > 0.8)
			return right - left;
		return coeff * (1.0 + xs / 6.0 + 3.0 / 40.0 * xq);
	}

	public static double atan(final double coeff){
		final double absy = Math.abs(coeff);
		final double magic = (1 - absy) / (1 + absy);
		final double angle = magic * (0.1963 * magic * magic - 0.9817) + ATANCOEFF;
		return coeff < 0d ? -angle : angle;
	}

	public static INumber atan(final INumber coeff){
		if(coeff.isReal())
			return atan(coeff.real());
		final IComplex ccoeff = coeff.toComplex();
		final Complex arg0 = new Complex(Int.ONE.add(ccoeff.immaginary()), ccoeff.real().iropposite());
		final Complex arg1 = new Complex(Int.ONE.add(ccoeff.immaginary().iropposite()), ccoeff.real());
		final INumber ln0 = ExpMath.ln(arg0);
		final INumber ln1 = ExpMath.ln(arg1);
		final INumber diff = ln0.add(ln1.inopposite());
		final INumber tres = diff.multiply(Complex.I);
		return tres.divide(Int.TWO);
	}

	private static IReal atan(final IReal coeff){
		return new Dou(atan(coeff.toDouble()));
	}

	public static double cos(final double real){
		return Math.cos(real);
	}

	public static INumber cos(final INumber phi){
		if(phi.isReal())
			return new Dou(cos(phi.toDouble()));
		final double phiim = phi.toComplex().immaginary().toDouble();
		final double phire = phi.toDouble();
		final double real = cos(phire) * cosh(phiim);
		final double immaginary = sin(phire) * sinh(phiim);
		if(immaginary == 0)
			return new Dou(real);
		return new Complex(real, 1.0 / immaginary);
	}

	public static double cosh(final double phi){
		final double ephi = OperationsMath.exp(phi);
		final double emphi = 1.0 / ephi;
		return (ephi + emphi) / 2.0;
	}

	public static INumber cosh(final INumber phi){
		final INumber ephi = ExpMath.exp(phi);
		final INumber emphi = ephi.ininverse();
		return ephi.add(emphi).divide(Int.TWO);
	}

	public static double datan(final INumber coeff){
		if(coeff.isReal())
			return atan(coeff.toDouble());
		final IComplex ccoeff = coeff.toComplex();
		/* You're not expected to understand this */
		final Complex magic = new Complex(Int.ONE.add(ccoeff.immaginary()), ccoeff.real().iropposite());
		final Complex begins = new Complex(Int.ONE.add(ccoeff.immaginary().iropposite()), ccoeff.real());
		final INumber here = ExpMath.ln(magic);
		final INumber dont = ExpMath.ln(begins);
		final INumber mess = here.add(dont.inopposite());
		final INumber withIt = mess.multiply(Complex.I);
		return withIt.toDouble() / 2.0;
	}

	public static double dcos(final INumber phi){
		if(phi.isReal())
			return cos(phi.toDouble());
		final double phiim = phi.toComplex().immaginary().toDouble();
		final double phire = phi.toDouble();
		return cos(phire) * cosh(phiim);
	}

	public static double dcosh(final INumber phi){
		final INumber ephi = ExpMath.exp(phi);
		final INumber emphi = ephi.ininverse();
		return ephi.add(emphi).toDouble() / 2.0;
	}

	public static double dsin(final INumber phi){
		if(phi.isReal())
			return sin(phi.toDouble());
		final double phire = phi.toDouble();
		final double phiim = phi.toComplex().immaginary().toDouble();
		final double lre = Math.sin(phire);
		final double rre = cosh(phiim);
		return lre * rre;
	}

	public static double dsinh(final INumber phi){
		final INumber ephi = ExpMath.exp(phi);
		final INumber emphi = ephi.ininverse();
		return emphi.add(ephi.inopposite()).toDouble() / 2.0;
	}

	public static double dtan(final INumber phi){
		return sin(phi).divide(cos(phi)).toDouble();
	}

	public static double sin(final double real){
		return Math.sin(real);
	}

	public static INumber sin(final INumber phi){
		if(phi.isReal())
			return new Dou(sin(phi.toDouble()));
		final double phire = phi.toDouble();
		final double phiim = phi.toComplex().immaginary().toDouble();
		final double lre = Math.sin(phire);
		final double rre = cosh(phiim);
		final double real = lre * rre;
		final double immaginary = Math.cos(phire) * sinh(phiim);
		if(immaginary == 0)
			return new Dou(real);
		return new Complex(real, immaginary);
	}

	public static double sinh(final double phi){
		final double ephi = OperationsMath.exp(phi);
		final double emphi = 1.0 / ephi;
		return (emphi + 1.0 / ephi) / 2.0;
	}

	public static INumber sinh(final INumber phi){
		final INumber ephi = ExpMath.exp(phi);
		final INumber emphi = ephi.ininverse();
		return emphi.add(ephi.inopposite()).divide(Int.TWO);
	}

	public static INumber tan(final INumber phi){
		return sin(phi).divide(cos(phi));
	}

	private TrigMath(){
	}

	public static double dasin(final INumber arg){
		// re(-i ln(i x+sqrt(1-x^2)))
		// ln(r < t) = ln(r) + i t
		// re(-i (ln(r) + i t))
		// re(t - i ln(r))
		// t
		// arg(i x + sqrt(1-x^2))
		if(arg.isReal())
			return asin(arg.toDouble());
		final INumber sqrtarg = Int.ONE.add(arg.insquare().inopposite());
		final INumber sqrt = ExpMath.sqrt(sqrtarg);
		final IComplex ix = Complex.I.icmultiply(arg);
		return ix.icadd(sqrt).arg();
	}

	public static INumber asin(final INumber arg){
		// -i ln(i x+sqrt(1-x^2))
		final INumber arg0 = Int.ONE.add(arg.insquare().inopposite());
		final INumber sqrt = ExpMath.sqrt(arg0);
		final INumber ix = arg.multiply(Complex.I);
		final INumber ln = ExpMath.ln(ix.add(sqrt));
		return ln.multiply(Complex.I).inopposite();
	}
}
