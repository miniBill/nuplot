package meplot.parser.utils;

import meplot.expressions.Expression;
import meplot.parser.Divided;
import meplot.parser.ParserException;
import meplot.parser.tokens.CharToken;
import meplot.parser.tokens.IToken;
import meplot.parser.tokens.ITokenList;
import meplot.parser.tokens.MatrixTokenList;
import meplot.parser.tokens.TokenIterator;
import meplot.parser.tokens.TokenList;
import platform.lists.IterableExtensions;

import java.util.Iterator;

public final class MatrixDivider {
	private static final String CLOSED_PAR = "}";
	private static final String OPEN_PAR = "{";

	private MatrixDivider() {
	}

	public static Divided divideMatrices(final ITokenList root) throws ParserException {
		final Divided toret = new Divided();
		final TokenIterator iterator = root.titerator();
		char name = 'A';
		while (iterator.hasNext()) {
			IToken curr = iterator.next();
			if (OPEN_PAR.equals(curr.toString())) {
				if (!iterator.hasNext())
					throw new ParserException("Error in matrix parse: wrong size!");
				if (OPEN_PAR.equals(iterator.peek().toString())) {
					curr = processColumn(iterator);
					try {
						final Expression expr = curr.toExpression();
						toret.add(name, expr);
					} catch (final ArrayIndexOutOfBoundsException e) {
						throw new ParserException("Error in matrix parse: wrong size?", e);
					}
					toret.rest().add(new CharToken(name++));
				} else {
					curr = processRow(iterator);
					final Expression expr = curr.toExpression();
					toret.add(name, expr);
					toret.rest().add(new CharToken(name++));
				}
			} else
				toret.rest().add(curr);
		}
		return toret;
	}

	private static MatrixTokenList processColumn(final TokenIterator iterator) {
		final MatrixTokenList toret = new MatrixTokenList();
		while (iterator.hasNext()) {
			iterator.next(); // skip "{"
			final IToken curr = processRow(iterator);
			toret.add(curr);
			if (iterator.hasNext()) {
				if (CLOSED_PAR.equals(iterator.peek().toString())) {
					iterator.next(); // drop }
					return toret;
				}
				if (iterator.hasNext())
					iterator.next(); // jump comma
			}
		}
		return toret;
	}

	private static MatrixTokenList processRow(final Iterator<IToken> iterator) {
		final MatrixTokenList toret = new MatrixTokenList();
		TokenList temp = new TokenList();

		for (IToken curr : IterableExtensions.wrap(iterator)) {
			if (curr.toString().equals(CLOSED_PAR)) {
				toret.add(temp);
				return toret;
			}
			if (",".equals(curr.toString())) {
				toret.add(temp);
				temp = new TokenList();
			} else
				temp.add(curr);
		}
		toret.add(temp);
		return toret;
	}
}
