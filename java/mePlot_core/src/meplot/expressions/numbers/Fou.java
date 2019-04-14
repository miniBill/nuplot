package meplot.expressions.numbers;

import meplot.expressions.Expression;

public final class Fou extends Fraction{
	public Fou(final int intPart, final int decPart, final int decLen, final int infPart,
			final int infLen, final String value){
		super(intPart, decPart, decLen, infPart, infLen);
		svalue = value;
	}

	private final String svalue;

	public Expression innerSimplify(){
		return new Fraction(this);
	}

	public Expression innerStepSimplify(){
		return new Fraction(this);
	}

	public void toString(final StringBuilder buffer){
		if(svalue == null)
			super.toString(buffer);
		else
			buffer.append(svalue);
	}

	public void toFullString(final StringBuilder buffer){
		buffer.append("Fou(");
		super.toFullString(buffer);
		buffer.append(')');
	}

	public boolean isIdentical(final Expression other){
		if(!(other instanceof Fou))
			return false;
		final Fou oth = (Fou)other;
		return toDouble() == oth.toDouble();
	}
}
