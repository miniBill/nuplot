package meplot.expressions.numbers;

import platform.log.Log;
import platform.log.LogLevel;

public abstract class Real extends Number implements IReal{
	public final boolean isReal(){
		return true;
	}

	public final INumber add(final INumber arg){
		if(arg instanceof Real)
			return add((Real)arg);
		return arg.add(this);
	}

	public abstract IReal add(IReal arg);

	public final INumber multiply(final INumber arg){
		if(arg instanceof IReal)
			return multiply((IReal)arg);
		return arg.multiply(this);
	}

	public abstract IReal multiply(IReal arg);

	public final INumber divide(final INumber arg){
		if(arg instanceof IReal)
			return divide((IReal)arg);
		if(arg instanceof Complex)
			return ((Complex)arg).invdivide(this); // this returns this/a
		Log.log(LogLevel.ERROR, "Real divided by nonreal noncomplex");
		return Int.ZERO;
	}

	public abstract IReal divide(IReal arg);

	public final IReal real(){
		return this;
	}

	public abstract IReal iropposite();

	public final INumber inopposite(){
		return iropposite();
	}

	public final IReal rsquare(){
		return multiply(this);
	}

	public abstract IReal irinverse();

	public final INumber ininverse(){
		return irinverse();
	}

	public final boolean isPositive(){
		return toDouble() > 0;
	}

	public final boolean isNegative(){
		return toDouble() < 0;
	}

	public IReal irSimplify(){
		return this;
	}

	public abstract IReal abs();

	public abstract boolean isInt();

	public final double dvalue(final char letter, final double value){
		return toDouble();
	}

	public final boolean lessThan(final Number arg){
		if(arg.isReal())
			return toDouble() < arg.toDouble();
		return arg.greaterThan(this);
	}

	public final boolean greaterThan(final Number arg){
		if(arg.isReal())
			return toDouble() > arg.toDouble();
		return arg.lessThan(this);
	}

	public final double norm(){
		return Math.abs(toDouble());
	}

	public final IComplex toComplex(){
		return new Complex(this, Int.ZERO);
	}

	public final boolean isAlmostEqual(final Number rightVal){
		if(rightVal.isReal()){
			final double dval = toDouble();
			if(dval == 0){
				final double rdval = rightVal.toDouble();
				return rdval == 0 || Math.abs((rdval - dval) / rdval) < EQUAL_THRESOLD;
			}
			return Math.abs((dval - rightVal.toDouble()) / dval) < EQUAL_THRESOLD;
		}
		return new Complex(this, Int.ZERO).isAlmostEqual(rightVal);
	}

	public final boolean isFullDouble(){
		return true;
	}

	public IInt intify(){
		return new Int((int)Math.floor(toDouble()));
	}
}
