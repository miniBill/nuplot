package meplot.expressions.functions.piecewise;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionsMath;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public final class Sign extends NonsymbolicMonicFunction{
	public Sign(final Expression value){
		super(value);
	}

	/**
	 * {@inheritDoc}
	 */
	public INumber value(final INumber arg){
		return FunctionsMath.sign(arg);
	}

	/**
	 * {@inheritDoc}
	 */
	public double dvalue(final INumber arg){
		return FunctionsMath.dsign(arg);
	}

	/**
	 * {@inheritDoc}
	 */
	public IFunction fill(final Expression expression){
		return new Sign(expression);
	}

	/**
	 * {@inheritDoc}
	 */
	public String getName(){
		return "sign";
	}

    /**
	 * {@inheritDoc}
	 */
	protected double fdvalue(final double arg){
		if(arg > 0)
			return 1;
		else
			if(arg < 0)
				return -1;
			else
				return 0;
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
