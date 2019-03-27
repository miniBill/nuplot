package meplot.expressions.numbers;

import meplot.expressions.Expression;
import meplot.expressions.functions.trig.TrigMath;
import meplot.expressions.list.IValueList;

public final class Complex extends Number implements IComplex{
	private static final double ALMOST_EQUAL_THRESOLD = 0.01;
	public static final Complex I = new Complex(Int.ZERO, Int.ONE);
	private final IReal realPart;
	private final IReal immaginaryPart;

	public Complex(final IReal real, final IReal immaginary){
		realPart = real;
		immaginaryPart = immaginary;
	}

	public Complex(final double real, final double immaginary){
		realPart = new Dou(real);
		immaginaryPart = new Dou(immaginary);
	}

	public Complex(final INumber real, final INumber immaginary){
		realPart = real.real();
		immaginaryPart = immaginary.real();
	}

	public IReal immaginary(){
		return immaginaryPart;
	}

	private IComplex divide(final IComplex iComplex){
		return multiply(iComplex.icinverse());
	}

	public INumber divide(final INumber arg){
		if(arg instanceof IReal)
			return divide(arg.real());
		return divide(arg.toComplex());
	}

	public IComplex divide(final IReal arg){
		return new Complex(realPart.divide(arg), immaginaryPart.divide(arg));
	}

	private IReal denorm(){
		return realPart.rsquare().add(immaginaryPart.rsquare());
	}

	private IComplex conjugate(){
		return new Complex(realPart, immaginaryPart.iropposite());
	}

	public boolean isReal(){
		return immaginaryPart.isZero();
	}

	public double norm(){
		final double dreal = realPart.toDouble();
		final double dimmaginary = immaginaryPart.toDouble();
		return norm(dreal, dimmaginary);
	}

	public static double norm(final double real, final double immaginary){
		return Math.sqrt(real * real + immaginary * immaginary);
	}

	public double arg(){
		return arg(realPart.toDouble(), immaginaryPart.toDouble());
	}

	public static double arg(final double real, final double immaginary){
		if(real > 0)
			return TrigMath.atan(immaginary / real);
		if(real < 0){
			if(immaginary >= 0)
				return Math.PI + TrigMath.atan(immaginary / real);
			return -Math.PI + TrigMath.atan(immaginary / real);
		}
		if(immaginary > 0)
			return Math.PI / 2;
		if(immaginary < 0)
			return -Math.PI / 2;
		return 0; // would be: undefined
	}

	public INumber multiply(final INumber arg){
		return icmultiply(arg);
	}

	public IComplex icmultiply(final INumber arg){
		if(arg instanceof IReal)
			return multiply(arg.real());
		return multiply(arg.toComplex());
	}

	private IComplex multiply(final IComplex arg){
		final IReal real = realPart.multiply(arg.real()).add(
				immaginaryPart.multiply(arg.immaginary()).iropposite());
		final IReal immaginary = realPart.multiply(arg.immaginary()).add(
				immaginaryPart.multiply(arg.real()));
		return new Complex(real, immaginary);
	}

	public IComplex multiply(final IReal arg){
		return new Complex(realPart.multiply(arg), immaginaryPart.multiply(arg));
	}

	public boolean compatible(final Expression elementAt, final char operation){
		if(elementAt instanceof Number)
			return true;
		return super.compatible(elementAt, operation);
	}

	public INumber add(final INumber arg){
		return icadd(arg);
	}

	public IComplex icadd(final INumber arg){
		if(arg instanceof IReal)
			return new Complex(realPart.add(arg.real()), immaginaryPart);
		IComplex carg = arg.toComplex();
		return new Complex(realPart.add(carg.real()), immaginaryPart.add(carg.immaginary()));
	}

	public void toString(final StringBuffer buffer){
		if(realPart.isZero()){
			if(immaginaryPart.isZero())
				buffer.append('0');
			else
				if(immaginaryPart.equals(Int.MINUSONE))
					buffer.append("-i");
				else{
					if(!immaginaryPart.isOne())
						buffer.append(immaginaryPart);
					buffer.append('i');
				}
			return;
		}

		buffer.append(realPart);
		if(immaginaryPart.isZero())
			return;
		if(immaginaryPart.isOne())
			buffer.append("+i");
		else
			if(immaginaryPart.equals(Int.MINUSONE))
				buffer.append("-i");
			else{
				if(!immaginaryPart.isNegative())
					buffer.append('+');

				buffer.append(immaginaryPart);
				buffer.append('i');
			}
	}

	public INumber inopposite(){
		return new Complex(realPart.iropposite(), immaginaryPart.iropposite());
	}

	public IReal real(){
		return realPart;
	}

	// this returns a/this
	public INumber invdivide(final INumber arg){
		return arg.multiply(icinverse());
	}

	public Expression invdivide(final Expression arg){
		return arg.multiply(inverse());
	}

	public INumber ininverse(){
		return icinverse();
	}

	public IComplex icinverse(){
		return conjugate().divide(denorm());
	}

	public Expression innerSimplify(){
		if(immaginaryPart.isZero())
			return realPart;
		return new Complex(realPart.irSimplify(), immaginaryPart.irSimplify());
	}

	public void toFullString(final StringBuffer buffer){
		buffer.append("Cx(");
		realPart.toFullString(buffer);
		buffer.append(',');
		immaginaryPart.toFullString(buffer);
		buffer.append(')');
	}

	public double dvalue(final char letter, final double value){
		return toDouble();
	}

	public boolean lessThan(final Number arg){
		return norm() < arg.norm();
	}

	public boolean greaterThan(final Number arg){
		return norm() > arg.norm();
	}

	public IComplex toComplex(){
		return this;
	}

	public boolean isAlmostEqual(final Number rightVal){
		if(rightVal instanceof Complex){
			final Complex cright = (Complex)rightVal;
			final double dreal = Math.abs(cright.toDouble() - real().toDouble());
			final double dimmaginary = Math.abs(cright.immaginary().toDouble()
					- immaginary().toDouble());
			final double nor = norm();
			if(nor == 0){
				final double rnor = cright.norm();
				if(rnor == 0)
					return true;
				return (dreal + dimmaginary) / rnor < ALMOST_EQUAL_THRESOLD;
			}
			return dreal + dimmaginary < ALMOST_EQUAL_THRESOLD;
		}
		return isAlmostEqual(new Complex(rightVal.real(), Int.ZERO));
	}

	public Expression innerStepSimplify(){
		return innerSimplify();
	}

	public double toDouble(){
		return realPart.toDouble();
	}

	public boolean isZero(){
		return realPart.isZero() && immaginaryPart.isZero();
	}

	public boolean isOne(){
		return realPart.isOne() && immaginaryPart.isZero();
	}

	public boolean isFullDouble(){
		return isReal();
	}

	public INumber nmatrixDvalue(final IValueList valueList){
		return new Complex(realPart.douvalue(valueList),
				immaginaryPart.douvalue(valueList));
	}

	public boolean isIdentical(final Expression other){
		if(!(other instanceof IComplex))
			return false;
		final IComplex oth = (IComplex)other;
		return realPart.isIdentical(oth.real())
				&& immaginaryPart.isIdentical(oth.immaginary());
	}

	public INumber insquare(){
		final INumber nre = realPart.insquare()
				.add(immaginaryPart.insquare().inopposite());
		final IReal nim = Int.TWO.multiply(realPart
				.multiply(immaginaryPart));
		return new Complex(nre, nim);
	}

	public void toHtml(final StringBuffer buffer){
		if(realPart.isZero()){
			if(immaginaryPart.isZero())
				buffer.append('0');
			else
				if(immaginaryPart.equals(Int.MINUSONE))
					buffer.append("-i");
				else{
					if(!immaginaryPart.isOne())
						immaginaryPart.toWrappedHtml(buffer);
					buffer.append('i');
				}
			return;
		}

		realPart.toWrappedHtml(buffer);
		if(immaginaryPart.isZero())
			return;
		if(immaginaryPart.isOne())
			buffer.append("+i");
		else
			if(immaginaryPart.equals(Int.MINUSONE))
				buffer.append("-i");
			else{
				if(!immaginaryPart.isNegative())
					buffer.append('+');

				immaginaryPart.toWrappedHtml(buffer);
				buffer.append('i');
			}
	}
}
