package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;
import platform.lists.IList;

public interface ITokenList extends Iterable<IToken>, IList<IToken>, IToken {
	Expression toExpression() throws ParserException;

	IToken getLast();
}
