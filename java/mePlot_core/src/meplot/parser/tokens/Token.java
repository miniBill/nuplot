package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;

public abstract class Token implements IToken{
	// Needed for broken real implementations
	public abstract Expression toExpression() throws ParserException;

	public String toCString(){
		return toString();
	}

	public String toSString(){
		return toString();
	}
}
