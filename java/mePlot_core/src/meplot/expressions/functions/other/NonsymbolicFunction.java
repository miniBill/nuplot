package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.Function;
import meplot.expressions.visitors.IExpressionFunctorVisitor;
import meplot.expressions.visitors.IExpressionNonsymbolicFunctionVisitor;

public abstract class NonsymbolicFunction extends Function{
	protected NonsymbolicFunction(final Expression[] values){
		super(values);
	}

	protected double fdvalue(final double[] arg, final char letter, final double value){
		return fdvalue(arg);
	}

	protected abstract double fdvalue(double[] arg);

	public final Expression accept(final IExpressionFunctorVisitor visitor){
		return visitor.visit(this);
	}

	public abstract Expression accept(IExpressionNonsymbolicFunctionVisitor visitor);
}
