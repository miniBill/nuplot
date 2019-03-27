package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;
import platform.lists.IToString;

public interface IToken extends IToString{
	Expression toExpression() throws ParserException;

	String toCString();

	String toSString();
}
