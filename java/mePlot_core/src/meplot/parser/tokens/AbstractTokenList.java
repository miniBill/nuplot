package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.parser.ParserException;
import platform.lists.ToStringList;

import java.util.Iterator;

public abstract class AbstractTokenList extends ToStringList<IToken> implements ITokenList {
	public final Expression toExpression() throws ParserException {
		return toExpression(0);
	}

	public final TokenIterator titerator() {
		return titerator(0);
	}

	protected final TokenIterator titerator(final int index) {
		return new TokenIterator(this, index);
	}

	public final String toCString() {
		final StringBuffer toret = new StringBuffer();
		cString(toret);
		return toret.toString();
	}

	public final String toSString() {
		final StringBuilder toret = new StringBuilder("{");
		final Iterator<IToken> iterator = iterator();
		while (iterator.hasNext()) {
			final IToken curr = iterator.next();
			if (curr instanceof TokenList)
				toret.append(curr.toCString());
			else
				toret.append(curr);
			if (iterator.hasNext())
				toret.append(',');
		}
		toret.append('}');
		return toret.toString();
	}

	protected final void cString(final StringBuffer toret) {
		for (IToken iToken : this)
			toret.append(iToken);
	}

	// Needed for broken real implementations.
	public abstract Expression toExpression(final int index) throws ParserException;
}
