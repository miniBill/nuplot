package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionsMath;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.piecewise.Abs;
import meplot.expressions.functions.piecewise.Max;
import meplot.expressions.numbers.INumber;
import meplot.expressions.operations.OperationsMath;

public final class MaxNorm extends NonsymbolicExpandFunction{
	public MaxNorm(final Expression[] expr){
		super(expr);
	}

	public IFunction fill(final Expression[] args){
		return new MaxNorm(args);
	}

	public String getName(){
		return "mn";
	}

	protected double fdvalue(final double[] arg){
		final double absa = Math.abs(arg[0]);
		final double absb = Math.abs(arg[1]);
		return OperationsMath.max(absa, absb);
	}

	protected double dvalue(final INumber[] arg){
		final double absa = Math.abs(arg[0].toDouble());
		final double absb = Math.abs(arg[1].toDouble());
		return OperationsMath.max(absa, absb);
	}

	protected INumber value(final INumber[] arg){
		final INumber absa = FunctionsMath.abs(arg[0]);
		final INumber absb = FunctionsMath.abs(arg[1]);
		return OperationsMath.max(absa, absb);
	}

	public int needs(){
		return 2;
	}

    protected Expression expand(final Expression[] args){
		final Expression absa = new Abs(args[0]);
		final Expression absb = new Abs(args[1]);
		return new Max(absa, absb);
	}
}
