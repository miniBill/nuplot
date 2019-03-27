package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.ExpandFunction;

abstract class NonsymbolicExpandFunction extends ExpandFunction{
	protected NonsymbolicExpandFunction(final Expression[] values){
		super(values);
	}

	protected double fdvalue(final double[] arg, final char letter, final double value){
		return fdvalue(arg);
	}

	protected abstract double fdvalue(double[] arg);
}
