package meplot.expressions.numbers;

import meplot.expressions.Expression;
import meplot.expressions.list.IValueList;

public final class Dou extends Real implements IDou{
	public static final Dou E = new Dou(Math.E);
	private final double value;

	public Dou(final double value){
		this.value = value;
	}

	public IReal add(final IReal arg){
		if(isZero())
			return arg;
		return new Dou(value + arg.toDouble());
	}

	public IReal multiply(final IReal arg){
		if(isZero())
			return Int.ZERO;
		if(isOne())
			return arg;
		return new Dou(value * arg.toDouble());
	}

	public IReal divide(final IReal arg){
		return new Dou(value / arg.toDouble());
	}

	public boolean compatible(final Expression elementAt, final char operation){
		if(elementAt instanceof Dou || elementAt instanceof AbstractInteger)
			return true;
		return super.compatible(elementAt, operation);
	}

	public boolean isOne(){
		return value == 1;
	}

	public boolean isZero(){
		return value == 0;
	}

	public void toString(final StringBuffer buffer){
		buffer.append(value);
	}

	public double toDouble(){
		return value;
	}

	public IReal irinverse(){
		return new Dou(1d / value);
	}

	public IReal iropposite(){
		return new Dou(-value);
	}

	public IReal abs(){
		if(value < 0)
			return iropposite();
		return this;
	}

	public void toFullString(final StringBuffer buffer){
		buffer.append("Dou(");
		toString(buffer);
		buffer.append(')');
	}

	public boolean isInt(){
		return false;
	}

	public IDou douvalue(final IValueList valueList){
		return this;
	}

	public boolean isIdentical(final Expression other){
		if(!(other instanceof IDou))
			return false;
		final IDou oth = (IDou)other;
		return value == oth.toDouble();
	}

	public INumber insquare(){
		return new Dou(value * value);
	}

	public void toHtml(final StringBuffer buffer){
		buffer.append(value);
	}
}
