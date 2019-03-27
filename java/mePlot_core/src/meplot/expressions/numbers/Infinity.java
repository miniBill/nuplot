package meplot.expressions.numbers;

import meplot.expressions.Expression;
import meplot.expressions.list.IValueList;
import platform.NotImplementedException;

public final class Infinity extends Real implements IDou{
	/**
	 * Sign of the infinity. True for positive.
	 */
	private final boolean sign;

	// ESCA-JAVA0007:
	public static final IDou POSITIVE = new Infinity(true);
	// ESCA-JAVA0007:
	public static final IDou NEGATIVE = new Infinity(false);

	private Infinity(final boolean sign){
		this.sign = sign;
	}

	public IDou douvalue(final IValueList valueList){
		return this;
	}

	public boolean isIdentical(final Expression other){
		if(!(other instanceof Infinity))
			return false;
		final Infinity oth = (Infinity)other;
		return oth.sign == sign;
	}

	public IReal add(final IReal arg){
		if(arg instanceof Infinity){
			final Infinity oth = (Infinity)arg;
			if(oth.sign == sign)
				return this;
			return Indefinite.ISTANCE;
		}
		return this;
	}

	public IReal multiply(final IReal arg){
		if(arg instanceof Infinity){
			final Infinity oth = (Infinity)arg;
			if(oth.sign == sign)
				return POSITIVE;
			return NEGATIVE;
		}
		if(arg.isZero())
			return Indefinite.ISTANCE;
		if(arg.isPositive())
			return this;
		return iropposite();
	}

	public IReal divide(final IReal arg){
		if(arg instanceof Infinity || arg.isZero())
			return Indefinite.ISTANCE;
		if(arg.isPositive())
			return this;
		return iropposite();
	}

	public IReal iropposite(){
		return sign ? NEGATIVE : POSITIVE;
	}

	public IReal irinverse(){
		return Int.ZERO;
	}

	public IReal abs(){
		return POSITIVE;
	}

	public boolean isInt(){
		return false;
	}

	public double toDouble(){
		return sign ? Double.POSITIVE_INFINITY : Double.NEGATIVE_INFINITY;
	}

	public void toFullString(final StringBuffer buffer){
		if(sign)
			buffer.append("INFTY(+)");
		else
			buffer.append("INFTY(-)");
	}

	public void toString(final StringBuffer buffer){
		if(sign)
			buffer.append("+∞");
		else
			buffer.append("-∞");
	}

	public INumber insquare(){
		return POSITIVE;
	}

	public void toHtml(final StringBuffer buffer){
		throw new NotImplementedException();
	}
}
