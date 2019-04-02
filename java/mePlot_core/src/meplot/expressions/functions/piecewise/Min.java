package meplot.expressions.functions.piecewise;

import meplot.expressions.Expression;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.operations.OperationsMath;
import meplot.expressions.visitors.IExpressionNonsymbolicFunctionVisitor;

public final class Min extends NonsymbolicFunction{
	public Min(final Expression[] expr){
		super(expr);
	}

	public IFunction fill(final Expression[] args){
		return new Min(args);
	}

	public String getName(){
		return "min";
	}

	protected double dvalue(final INumber[] arg){
		final double adval = arg[0].toDouble();
		final double bdval = arg[1].toDouble();
		return OperationsMath.min(adval, bdval);
	}

	protected INumber value(final INumber[] arg){
		return OperationsMath.min(arg[0], arg[1]);
	}

	public int needs(){
		return 2;
	}

	public Expression accept(final IExpressionNonsymbolicFunctionVisitor visitor){
		return visitor.visit(this);
	}

	protected Expression innerSimplify(final Expression[] vals){
		if(vals[0] instanceof INumber && vals[1] instanceof INumber)
			return OperationsMath.min((INumber)vals[0], (INumber)vals[1]);
		return fill(vals);
	}

	public double fdvalue(final double[] arg){
		return OperationsMath.min(arg[0], arg[1]);
	}
}
