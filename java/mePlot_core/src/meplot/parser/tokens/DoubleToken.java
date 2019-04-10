package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.expressions.numbers.Dou;
import meplot.parser.ParserException;

public final class DoubleToken extends Token {
	private final double val;

	public DoubleToken(final ITokenList temp) throws ParserException {
		final StringBuilder buffer = new StringBuilder();
		final TokenIterator iterator = temp.titerator();
		while (iterator.hasNext())
			buffer.append(iterator.next());
		final String res = buffer.toString();
		if (res.length() > 0 && !".".equals(res))
			try {
				val = Double.parseDouble(res);
			} catch (final NumberFormatException e) {
				throw new ParserException("Failed parsing " + res, e);
			}
		else
			val = 0;
	}

	public String toString() {
		return Double.toString(val);
	}

	public Expression toExpression() {
		return new Dou(val);
	}

	public void toString(final StringBuffer buffer) {
		buffer.append(val);
	}
}
