package meplot.expressions.functions;

import meplot.expressions.Expression;
import platform.lists.IList;

public abstract class ArbitraryFunction extends Function{
	protected ArbitraryFunction(final IList<Expression> values, final boolean[] symbolic){
		super(values, symbolic);
	}

	public final int needs(){
		return -1;
	}
}
