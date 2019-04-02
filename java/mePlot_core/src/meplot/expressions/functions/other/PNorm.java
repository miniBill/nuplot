package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.FunctionsMath;
import meplot.expressions.functions.IFunction;
import meplot.expressions.functions.piecewise.Abs;
import meplot.expressions.numbers.INumber;
import meplot.expressions.operations.OperationsMath;
import meplot.expressions.operations.Power;

public final class PNorm extends NonsymbolicExpandFunction{
	public PNorm(final Expression[] expr){
		super(expr);
	}

	public IFunction fill(final Expression[] args){
		return new PNorm(args);
	}

	public String getName(){
		return "norm";
	}

	protected double fdvalue(final double[] arg){
		final double num = arg[0];
		final double absa = Math.abs(arg[1]);
		final double absb = Math.abs(arg[2]);
		final double base = OperationsMath.pow(absa, num) + OperationsMath.pow(absb, num);
		return OperationsMath.pow(base, 1.0 / num);
	}

	protected double dvalue(final INumber[] arg){
		final double num = arg[0].toDouble();
		final double absa = Math.abs(arg[1].toDouble());
		final double absb = Math.abs(arg[2].toDouble());
		final double base = OperationsMath.pow(absa, num) + OperationsMath.pow(absb, num);
		return OperationsMath.pow(base, 1.0 / num);
	}

	protected INumber value(final INumber[] arg){
		final INumber num = arg[0];
		final INumber absa = FunctionsMath.abs(arg[1]);
		final INumber absb = FunctionsMath.abs(arg[2]);
		final INumber base = OperationsMath.pow(absa, num).add(
				OperationsMath.pow(absb, num));
		return OperationsMath.pow(base, num.ininverse());
	}

	public int needs(){
		return 3;
	}

    protected Expression expand(final Expression[] args){
		final Expression num = args[0];
		final Expression absa = new Abs(args[1]);
		final Expression absb = new Abs(args[2]);
		final Expression base = new Power(absa, num).add(new Power(absb, num));
		return new Power(base, num.inverse());
	}
}
