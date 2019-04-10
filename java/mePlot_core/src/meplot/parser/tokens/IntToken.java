package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.expressions.numbers.Int;
import meplot.parser.ParserException;

public final class IntToken extends Token {
	private final int val;

	public IntToken(final int value) {
		val = value;
	}

	public IntToken(final ITokenList temp) throws ParserException {
		final StringBuilder toret = new StringBuilder();
		final TokenIterator iterator = temp.titerator();
		while (iterator.hasNext())
			toret.append(iterator.next());
		try {
			val = Integer.parseInt(toret.toString());
		} catch (final NumberFormatException e) {
			throw new ParserException("Parsing " + toret + " as a number failed.", e);
		}
	}

	public String toString() {
		return Integer.toString(val);
	}

	public Expression toExpression() {
		return new Int(val);
	}

	public void toString(final StringBuffer buffer) {
		buffer.append(val);
	}
}
