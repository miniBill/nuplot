package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionsMath;
import meplot.expressions.functions.IFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.visitors.IExpressionMonicFunctionVisitor;

public final class Floor extends NonsymbolicMonicFunction{
	public Floor(final Expression value){
		super(value);
	}

	public INumber value(final INumber arg){
		return FunctionsMath.floor(arg);
	}

	public double dvalue(final INumber arg){
		return FunctionsMath.floor(arg.toDouble());
	}

	public IFunction fill(final Expression expression){
		return new Floor(expression);
	}

	public String getName(){
		return "floor";
	}

    protected double fdvalue(final double arg){
		return FunctionsMath.floor(arg);
	}

	public Expression accept(final IExpressionMonicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
