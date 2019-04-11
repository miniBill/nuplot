package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.Multiplication;
import meplot.parser.ParserException;

import java.util.Iterator;

public class TokenList extends AbstractTokenList {
	/**
	 * Creates an empty list.
	 */
	public TokenList() {
		// Creates an empty list.
	}

	public TokenList(Iterable<IToken> other) {
		addRange(other);
	}

	public TokenList(final IToken next) {
		add(next);
	}

	public final String toString() {
		final StringBuffer toret = new StringBuffer("{");
		cString(toret);
		toret.append('}');
		return toret.toString();
	}

	public final void addRange(final Iterator<IToken> iterator) {
		while (iterator.hasNext())
			add(iterator.next());
	}

	public final IToken pop() {
		final IToken toret = getLast();
		super.removeAt(length() - 1);
		return toret;
	}

	public final IToken[] toArray() {
		final IToken[] toret = new IToken[length()];
		for (int i = 0; i < toret.length; i++)
			toret[i] = elementAt(i);
		return toret;
	}

	public Expression toExpression(final int index) throws ParserException {
		if (isEmpty())
			return Int.ONE;
		if (isSingle()) {
			if (index == 0)
				return getLast().toExpression();
			throw new ParserException("index out of bound in TokenList.toExpression(I)",
					new ArrayIndexOutOfBoundsException(index));
		}
		final IExpressionList toret = new ExpressionList();
		final TokenIterator iterator = titerator(index);
		while (iterator.hasNext())
			toret.add(iterator.next().toExpression());
		return new Multiplication(toret);
	}
}
