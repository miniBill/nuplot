package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.operations.Multiplication;
import meplot.parser.ParserException;

import java.util.Iterator;

public final class CharList extends AbstractTokenList {
	/**
	 * Creates an empty list.
	 */
	public CharList() {
		// Creates an empty list.
	}

	public CharList(final String substring) {
		for (int c = 0; c < substring.length(); c++)
			add(new CharToken(substring.charAt(c)));
	}

	public String toString() {
		final StringBuffer toret = new StringBuffer("{C");
		cString(toret);
		toret.append('}');
		return toret.toString();
	}

	private String toSmallString() {
		final StringBuilder toret = new StringBuilder();
		for (IToken iToken : this)
			toret.append(iToken);
		return toret.toString();
	}

	public ITokenList aggregate() {
		final String small = toSmallString();
		return FunctionToken.parse(small);
	}

	public Expression toExpression(final int index) throws ParserException {
		CharList list = this;
		if (list.isSingle()) {
			if (index == 0)
				return list.getLast().toExpression();
			throw new ParserException("index out of bound in CharList.toExpression(I)",
					new ArrayIndexOutOfBoundsException(index));
		}
		final IExpressionList result = new ExpressionList();
		final Iterator<IToken> iterator = list.titerator(index);
		while (iterator.hasNext())
			result.add(iterator.next().toExpression());
		return new Multiplication(result);
	}
}
