package meplot.expressions.functions;

import meplot.expressions.Expression;
import meplot.expressions.visitors.IExpressionFunctorVisitor;

public abstract class ExpandFunction extends Function{
	protected ExpandFunction(final Expression[] values){
		super(values);
	}

	protected ExpandFunction(final Expression[] values, final boolean[] symbolic){
		super(values, symbolic);
	}

	public final Expression expand(){
		return expand(getArguments());
	}

	protected final Expression innerSimplify(final Expression[] vals){
		return expand(vals);
	}

	protected Expression innerStepSimplify(final Expression[] args){
		return expand(args);
	}

	protected abstract Expression expand(final Expression[] args);

	public final Expression accept(final IExpressionFunctorVisitor visitor){
		return visitor.visit(this);
	}
}
