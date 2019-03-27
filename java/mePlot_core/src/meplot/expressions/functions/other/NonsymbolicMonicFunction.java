package meplot.expressions.functions.other;

import meplot.expressions.Expression;
import meplot.expressions.functions.MonicFunction;

public abstract class NonsymbolicMonicFunction extends MonicFunction{
	protected NonsymbolicMonicFunction(final Expression value){
		super(value);
	}

	protected final double fdvalue(final double arg, final char letter, final double value){
		return fdvalue(arg);
	}

	protected abstract double fdvalue(double arg);
}
