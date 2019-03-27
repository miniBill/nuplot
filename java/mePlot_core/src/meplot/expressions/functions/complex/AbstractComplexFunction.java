package meplot.expressions.functions.complex;

import meplot.expressions.Expression;
import meplot.expressions.functions.other.NonsymbolicMonicFunction;

public abstract class AbstractComplexFunction extends NonsymbolicMonicFunction{
	private boolean checkDone;
	private boolean isFull;

	protected AbstractComplexFunction(final Expression value){
		super(value);
	}

	public final boolean isFullDouble(){
		if(!checkDone){
			isFull = getArgument().isFullDouble();
			checkDone = true;
		}
		return true;
	}

	public final double fdvalue(final char letter, final double value){
		if(isFull)
			return fdvalue(getArgument().dvalue(letter, value));
		return dvalue(letter, value);
	}
}
