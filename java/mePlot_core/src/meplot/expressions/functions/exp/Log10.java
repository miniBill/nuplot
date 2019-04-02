package meplot.expressions.functions.exp;

import meplot.expressions.Expression;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Power;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public final class Log10 extends NonsymbolicMonicFunction{
	public Log10(final Expression value){
		super(value);
	}

	public boolean isZero(){
		return getArgument().isOne();
	}

	public boolean isOne(){
		return isTen(getArgument());
	}

	private static boolean isTen(final Expression arg){
		return arg instanceof Int && ((Int)arg).getValue() == 10;
	}

	public Expression innerSimplify(final Expression arg){
		if(arg.isOne())
			return Int.ZERO;
		if(isTen(arg))
			return Int.ONE;
		if(arg instanceof Power){
			final Power parg = (Power)arg;
			if(isTen(parg.getBase()))
				return parg.getExponent();
		}
		return new Log10(arg);
	}

	public INumber value(final INumber val){
		return ExpMath.log(val);
	}

	public double dvalue(final INumber val){
		return ExpMath.dlog(val);
	}

	public IFunction fill(final Expression expr){
		return new Log10(expr);
	}

	public String getName(){
		return "log";
	}

    protected double fdvalue(final double arg){
		return ExpMath.log(arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
