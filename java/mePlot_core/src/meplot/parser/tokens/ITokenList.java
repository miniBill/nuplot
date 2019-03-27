package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;
import platform.lists.IEquatableIterable;

public interface ITokenList extends IEquatableIterable, IToken{
	void add(final IToken token);

	TokenIterator getIterator();

	Expression toExpression(final int index) throws ParserException;

	IToken elementAt(final int index);

	IToken getLast();
}
