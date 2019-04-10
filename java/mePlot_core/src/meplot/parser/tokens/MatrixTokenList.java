package meplot.parser.tokens;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.numbers.Int;
import meplot.parser.Parser;
import meplot.parser.ParserException;
import platform.lists.IIterator;

public final class MatrixTokenList extends TokenList {
	public Expression toExpression(final int index) throws ParserException {
		if (isEmpty())
			return Int.ONE;
		if (isSingle()) {
			if (index == 0) {
				final IToken singles = getLast();

				if (singles instanceof MatrixTokenList)
					return Parser.parse(singles.toSString());
				final String toParse;
				if (singles instanceof AbstractTokenList)
					toParse = singles.toCString();
				else
					toParse = singles.toString();
				if (toParse.length() > 0)
					return new Matrix(Parser.parse(toParse));
				return new Matrix(new Expression[0]);
			}
			throw new ParserException("index out of bound in MatrixTokenList.toExpression(I)",
					new ArrayIndexOutOfBoundsException(index));
		}
		final IExpressionList list = new ExpressionList();
		final IIterator<IToken> iterator = iterator(index);
		boolean vector = false;
		while (iterator.hasNext()) {
			final IToken curr = iterator.next();
			if (curr instanceof MatrixTokenList)
				list.add(curr.toExpression());
			else {
				vector = true;
				if (curr instanceof AbstractTokenList) {
					final String currString = curr.toCString();
					list.add(Parser.parse(currString));
				} else
					list.add(Parser.parse(curr.toCString()));
			}
		}
		return new Matrix(list, vector);
	}
}
