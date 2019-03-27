package meplot.expressions.numbers;

public final class Int extends AbstractInteger{
	public static final IInt ZERO = new Int(0);
	public static final IInt MINUSONE = new Int(-1);
	public static final IInt ONE = new Int(1);
	public static final IInt TWO = new Int(2);
	public static final IInt THREE = new Int(3);
	public static final IInt FOUR = new Int(4);

	public Int(final int value){
		super(value);
		simplified = true;
	}

	public void toString(final StringBuffer buffer){
		buffer.append(getValue());
	}

	public IReal add(final IReal arg){
		if(getValue() == 0)
			return arg;
		if(arg instanceof Int)
			return add((Int)arg);
		return arg.add(this);
	}

	public IReal multiply(final IReal arg){
		if(getValue() == 1)
			return arg;
		if(getValue() == 0)
			return Int.ZERO;
		if(arg instanceof Int)
			return multiply((Int)arg);
		return arg.multiply(this);
	}

	public IReal divide(final IReal arg){
		if(arg instanceof Int)
			return new Fraction(this, (Int)arg);
		return multiply(arg.irinverse());
	}

	public IReal iropposite(){
		return iiopposite();
	}

	public IInt iiopposite(){
		return new Int(-getValue());
	}

	public void toFullString(final StringBuffer buffer){
		buffer.append("Int(");
		buffer.append(getValue());
		buffer.append(')');
	}

	public IInt isquare(){
		final int value = getValue();
		return new Int(value * value);
	}

	public void toHtml(final StringBuffer buffer){
		buffer.append(getValue());
	}
}
