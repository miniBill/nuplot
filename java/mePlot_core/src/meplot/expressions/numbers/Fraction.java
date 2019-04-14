package meplot.expressions.numbers;

import meplot.expressions.Expression;
import meplot.expressions.ICalculable;
import meplot.expressions.functions.FunctionsMath;
import meplot.expressions.list.IValueList;
import meplot.expressions.operations.IDivision;
import meplot.expressions.operations.Operation;
import meplot.expressions.operations.OperationsMath;
import meplot.expressions.visitors.simplification.SimplificationHelper;

public class Fraction extends Real implements IDivision{
	public static final Fraction ONEHALF = new Fraction(Int.ONE, Int.TWO);
	private final IInt numerator;
	private final IInt denominator;

	public Fraction(final IInt num, final IInt den){
		numerator = num;
		denominator = den;
	}

	public Fraction(final int intPart, final int decPart, final int decLen,
			final int infPart, final int infLen){
		final int decPow = OperationsMath.pow(10, decLen);
		if(infPart == 0){
			numerator = new Int(intPart * decPow + decPart);
			denominator = new Int(decPow);
		}
		else{
			final int infPow = OperationsMath.pow(10, infLen);
			numerator = new Int(intPart * decPow * (infPow - 1) + decPart * (infPow - 1)
					+ infPart);
			denominator = new Int(decPow * (infPow - 1));
		}
	}

	public Fraction(final Fraction from){
		numerator = from.numerator;
		denominator = from.denominator;
	}

	public final IReal irinverse(){
		return new Fraction(denominator, numerator);
	}

	public Expression innerSimplify(){
		return irSimplify();
	}

	public final IReal irSimplify(){
		if(denominator.isOne())
			return numerator;
		if(numerator.isZero())
			return Int.ZERO;
		if(numerator.equals(denominator))
			return Int.ONE;
		if(denominator.equals(Int.MINUSONE))
			return numerator.iropposite();

		int nValue = numerator.getValue();
		int dValue = denominator.getValue();

		final int gcd = FunctionsMath.gcd(Math.abs(nValue), Math.abs(dValue));

		if(gcd == 1){
			if(dValue >= 0){
				simplified = true;
				return this;
			}
		}
		else{
			nValue /= gcd;
			dValue /= gcd;
		}

		if(dValue < 0){
			nValue *= -1;
			dValue *= -1;
		}

		final Fraction toret = new Fraction(new Int(nValue), new Int(dValue));
		toret.simplified = true;
		return toret;
	}

	public final IReal add(final IReal arg){
		if(arg instanceof Fraction){
			final Fraction farg = (Fraction)arg;
			final IInt nad = numerator.multiply(farg.denominator);
			final IInt and = farg.numerator.multiply(denominator);
			return new Fraction(nad.add(and), denominator.multiply(farg.denominator));
		}
		if(arg instanceof Int){
			final Int iarg = (Int)arg;
			final IInt nsum = iarg.multiply(denominator);
			final IInt newnum = numerator.add(nsum);
			return new Fraction(newnum, denominator);
		}
		// a is a Dou
		return arg.add(this);
	}

	public final IReal multiply(final IReal arg){
		if(arg instanceof Fraction){
			final Fraction farg = (Fraction)arg;
			return new Fraction(numerator.multiply(farg.numerator),
					denominator.multiply(farg.denominator));
		}
		if(arg instanceof Int)
			return new Fraction(numerator.multiply((Int)arg), denominator);
		// a is Dou
		return arg.multiply(this);
	}

	public final IReal divide(final IReal arg){
		if(arg instanceof Fraction){
			final Fraction farg = (Fraction)arg;
			return new Fraction(numerator.multiply(farg.denominator),
					denominator.multiply(farg.numerator));
		}
		if(arg instanceof Int)
			return new Fraction(numerator, denominator.multiply((Int)arg));
		// a is Dou
		return arg.irinverse().multiply(this);
	}

	public final boolean compatible(final Expression elementAt, final char operation){
		if(elementAt instanceof INumber)
			return true;
		return super.compatible(elementAt, operation);
	}

	public final boolean isZero(){
		return numerator.isZero();
	}

	public final boolean isOne(){
		final ICalculable simp = SimplificationHelper.simplify(this);
		if(simp instanceof Fraction)
			return false;
		return simp.isOne();
	}

	public void toString(final StringBuilder buffer){
		numerator.toString(buffer);
		if(denominator.isOne())
			return;
		buffer.append(Operation.DIVISION);
		denominator.toString(buffer);
	}

	public final IReal iropposite(){
		return new Fraction(numerator.iiopposite(), denominator);
	}

	public final IReal abs(){
		return new Fraction(numerator.iiabs(), denominator.iiabs());
	}

	public final Expression getNumerator(){
		return numerator;
	}

	public final Expression getDenominator(){
		return denominator;
	}

	public void toFullString(final StringBuilder buffer){
		buffer.append("Frac(");
		numerator.toFullString(buffer);
		buffer.append(',');
		denominator.toFullString(buffer);
		buffer.append(')');
	}

	public Expression innerStepSimplify(){
		return innerSimplify();
	}

	public final IInt fgetDenominator(){
		return denominator;
	}

	public final IInt fgetNumerator(){
		return numerator;
	}

	public final double toDouble(){
		return numerator.toDouble() / denominator.toDouble();
	}

	public final INumber nmatrixDvalue(final IValueList valueList){
		return new Dou(toDouble());
	}

	public final boolean isInt(){
		return denominator.equals(Int.ONE);
	}

	public final IDou douvalue(final IValueList valueList){
		return new Dou(toDouble());
	}

	public boolean isIdentical(final Expression other){
		if(!(other instanceof Fraction))
			return false;
		final Fraction oth = (Fraction)other;
		return numerator.isIdentical(oth.numerator)
				&& denominator.isIdentical(oth.denominator);
	}

	public INumber insquare(){
		return new Fraction(numerator.isquare(),denominator.isquare());
	}

	public void toHtml(final StringBuilder buffer){
		numerator.toWrappedHtml(buffer);
		buffer.append('/');
		denominator.toWrappedHtml(buffer);
	}
}
