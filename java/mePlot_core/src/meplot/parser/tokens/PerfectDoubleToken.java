package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.expressions.numbers.Fou;
import meplot.parser.ParserException;

public final class PerfectDoubleToken extends Token {
	private final int intPart;
	private final int decPart;
	private final int decLen;
	private final int infPart;
	private final int infLen;
	private final String input;

	public PerfectDoubleToken(final ITokenList temp) throws ParserException {
		final StringBuilder buffer = new StringBuilder();
		final TokenIterator iterator = temp.tgetIterator();
		while (iterator.hasNext())
			buffer.append(iterator.next());
		input = buffer.toString();

		final int coma = input.indexOf('.');
		final int bang = input.indexOf('@');
		if (coma < 0) {
			if (bang < 0)
				throw new ParserException();
			if (bang == 0)
				intPart = 0;
			else {
				final String intString = input.substring(0, bang);
				intPart = tryParse(intString);
			}

			decPart = 0;
			decLen = 0;

			final String infString = input.substring(bang + 1);
			if (infString.length() > 0) {
				infPart = tryParse(infString);
				infLen = infString.length();
			} else {
				infPart = 0;
				infLen = 0;
			}
		} else {
			if (coma == 0)
				intPart = 0;
			else {
				final String intString = input.substring(0, coma);
				intPart = tryParse(intString);
			}
			if (bang >= 0) {
				if (coma + 1 < bang) {
					final String decString = input.substring(coma + 1, bang);
					decPart = tryParse(decString);
					decLen = decString.length();
				} else {
					decPart = 0;
					decLen = 0;
				}

				final String infString = input.substring(bang + 1);
				infPart = tryParse(infString);
				infLen = infString.length();
			} else {
				final String decString = input.substring(coma + 1);
				if (decString.length() > 0) {
					decPart = tryParse(decString);
					decLen = decString.length();
				} else {
					decPart = 0;
					decLen = 0;
				}
				infPart = 0;
				infLen = 0;
			}
		}
	}

	private int tryParse(final String infString) throws ParserException {
		try {
			return Integer.parseInt(infString);
		} catch (final NumberFormatException e) {
			throw new ParserException("Failed parsing " + input, e);
		}
	}

	public String toString() {
		return Integer.toString(intPart) + '.' + decPart + '@' + infPart;
	}

	public Expression toExpression() {
		return new Fou(intPart, decPart, decLen, infPart, infLen, input);
	}

	public void toString(final StringBuffer buffer) {
		buffer.append(intPart);
		buffer.append('.');
		buffer.append(decPart);
		buffer.append('@');
		buffer.append(infPart);
	}
}
