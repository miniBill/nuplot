package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.IFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.visitors.IExpressionNonsymbolicFunctionVisitor;

public final class Ackermann extends NonsymbolicFunction{
	public Ackermann(final Expression[] args){
		super(args);
	}

	public IFunction fill(final Expression[] args){
		return new Ackermann(args);
	}

	protected double dvalue(final INumber[] arg){
		return value(arg).toDouble();
	}

	private int ivalue(final int arg0, final int arg1, final int arg2){
		int arg1L = arg1;
		int arg2L = arg2;
		while(true){
			if(arg2L == 0)
				return arg0 + arg1L;
			if(arg1L == 0){
				if(arg2L < 3)
					return arg2L - 1;
				return arg0;
			}
			final int temp = arg2L;
			arg2L = temp - 1;
			arg1L = ivalue(arg0, arg1L - 1, temp);
		}
	}

	protected Expression innerSimplify(final Expression[] arg){
		if(arg[2].isZero())
			return arg[0].add(arg[1]);
		if(arg[1].isZero()){
			if(arg[2] instanceof INumber){
				INumber narg2 = (INumber)arg[2];
				if(narg2.lessThan(Int.THREE)){
					return narg2.add(Int.MINUSONE);
				}
			}
			return arg[0];
		}

		final int[] iarg = new int[arg.length];
		for(int c = 0; c < arg.length; c++)
			if(arg[c] instanceof Int)
				iarg[c] = ((Int)arg[c]).getValue();
			else
				return this;
		return new Int(ivalue(iarg[0], iarg[1], iarg[2]));
	}

	protected Expression innerStepSimplify(final Expression[] args){
		return innerSimplify(args);
	}

	protected INumber value(final INumber[] arg){
		final int[] iarg = new int[arg.length];
		for(int c = 0; c < arg.length; c++)
			if(arg[c] instanceof Int)
				iarg[c] = ((Int)arg[c]).getValue();
			else
				return Int.ZERO;
		return new Int(ivalue(iarg[0], iarg[1], iarg[2]));
	}

	/**
	 * {@inheritDoc}
	 */
	public String getName(){
		return "ack";
	}

	public int needs(){
		return 3;
	}

	protected double fdvalue(final double[] arg){
		return ivalue((int)arg[0], (int)arg[1], (int)arg[2]);
	}

	public Expression accept(final IExpressionNonsymbolicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
