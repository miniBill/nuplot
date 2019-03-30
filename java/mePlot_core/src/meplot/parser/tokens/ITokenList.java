package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;
import platform.lists.IIterable;

public interface ITokenList extends IIterable<IToken>, IToken {
	void add(final IToken token);

	TokenIterator tgetIterator();

	Expression toExpression(final int index) throws ParserException;

	IToken elementAt(final int index);

	IToken getLast();
}
