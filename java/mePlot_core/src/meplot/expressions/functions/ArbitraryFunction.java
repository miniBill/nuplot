package meplot.expressions.functions;

import meplot.expressions.Expression;

public abstract class ArbitraryFunction extends Function{
	protected ArbitraryFunction(final Expression[] values, final boolean[] symbolic){
		super(values, symbolic);
	}

	public final int needs(){
		return -1;
	}
}
