package meplot.expressions.functions.piecewise;

import meplot.expressions.Expression;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.other.NonsymbolicFunction;
import meplot.expressions.numbers.INumber;
import meplot.expressions.operations.OperationsMath;
import meplot.expressions.visitors.IExpressionNonsymbolicFunctionVisitor;

public final class Max extends NonsymbolicFunction{
	public Max(final Expression[] expr){
		super(expr);
	}

	public Max(final Expression absa, final Expression absb){
		super(new Expression[]{absa, absb});
	}

	public IFunction fill(final Expression[] args){
		return new Max(args);
	}

	public String getName(){
		return "max";
	}

	protected double dvalue(final INumber[] arg){
		final double adval = arg[0].toDouble();
		final double bdval = arg[1].toDouble();
		return OperationsMath.max(adval, bdval);
	}

	protected INumber value(final INumber[] arg){
		return OperationsMath.max(arg[0], arg[1]);
	}

	public int needs(){
		return 2;
	}

	protected Expression innerSimplify(final Expression[] vals){
		if(vals[0] instanceof INumber && vals[1] instanceof INumber)
			return OperationsMath.max((INumber)vals[0], (INumber)vals[1]);
		return fill(vals);
	}

	public double fdvalue(final double[] arg){
		return OperationsMath.max(arg[0], arg[1]);
	}

	public Expression accept(final IExpressionNonsymbolicFunctionVisitor visitor){
		return visitor.visit(this);
	}
}
