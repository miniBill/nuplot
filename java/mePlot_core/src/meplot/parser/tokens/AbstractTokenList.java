package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Multiplication;
import meplot.parser.ParserException;
import platform.lists.ToStringList;

public abstract class AbstractTokenList extends ToStringList<IToken> implements ITokenList {
	public Expression toExpression() throws ParserException {
		if (isEmpty())
			return Int.ONE;
		if (isSingle())
			return getLast().toExpression();
		final IExpressionList result = new ExpressionList();
		for (IToken iToken : this)
			result.add(iToken.toExpression());
		return new Multiplication(result);
	}

	public final TokenIterator titerator() {
		return new TokenIterator(this, 0);
	}

	public final String toCString() {
		final StringBuffer toret = new StringBuffer();
		cString(toret);
		return toret.toString();
	}

	public final String toSString() {
		final StringBuilder toret = new StringBuilder("{");
		boolean first=true;
		for (IToken curr : this) {
			if (!first)
				toret.append(',');
			first = false;
			if (curr instanceof TokenList)
				toret.append(curr.toCString());
			else
				toret.append(curr);
		}
		toret.append('}');
		return toret.toString();
	}

	protected final void cString(final StringBuffer toret) {
		for (IToken iToken : this)
			toret.append(iToken);
	}
}
