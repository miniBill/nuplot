package meplot.expressions.numbers;

import meplot.expressions.Expression;
import meplot.expressions.list.IValueList;

public abstract class AbstractInteger extends Real implements IInt, IDou{
	private final int value;

	protected AbstractInteger(final int value){
		this.value = value;
	}

	public final IInt add(final IInt arg){
		return new Int(value + arg.getValue());
	}

	public final boolean isOne(){
		return value == 1;
	}

	public final boolean isZero(){
		return value == 0;
	}

	public final boolean compatible(final Expression elementAt, final char operation){
		if(elementAt instanceof Int)
			return true;
		return super.compatible(elementAt, operation);
	}

	public final IReal abs(){
		return iiabs();
	}

	public final IInt iiabs(){
		if(value < 0)
			return iiopposite();
		return this;
	}

	public final boolean isInt(){
		return true;
	}

	public final double toDouble(){
		return value;
	}

	public final IInt intify(){
		return this;
	}

	public final IInt multiply(final IInt arg){
		return new Int(value * arg.getValue());
	}

	public final IDou douvalue(final IValueList valueList){
		return this;
	}

	public final boolean isIdentical(final Expression other){
		if(!(other instanceof IInt))
			return false;
		return isIdentical((IInt)other);
	}

	public final IReal irinverse(){
		return new Fraction(Int.ONE, this);
	}

	public final boolean isIdentical(final IInt oth){
		return value == oth.getValue();
	}

	public final int getValue(){
		return value;
	}

	public final INumber insquare(){
		return isquare();
	}
}
