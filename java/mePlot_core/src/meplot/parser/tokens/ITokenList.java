package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;

public interface ITokenList extends Iterable<IToken>, IToken {
	void add(final IToken token);

	TokenIterator titerator();

	Expression toExpression() throws ParserException;

	IToken elementAt(final int index);

	IToken getLast();
}
