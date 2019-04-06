package meplot.expressions.list;

import meplot.expressions.Expression;
import meplot.parser.utils.Cleaner;
import org.jetbrains.annotations.NotNull;
import platform.lists.IIterator;
import platform.lists.ToStringList;

public final class ExpressionList extends ToStringList<Expression> implements IExpressionList {
	private static final IExpressionList EMPTY = new ExpressionList();

	public static IExpressionList getEmpty() {
		return EMPTY;
	}

	public ExpressionList(final Expression expr) {
		add(expr);
	}

	/**
	 * Creates empty list.
	 */
	public ExpressionList() {
		// Creates empty list
	}

	public ExpressionList(final Expression expr, final IIterator<Expression> expressionList) {
		add(expr);
		addRange(expressionList);
	}

	public ExpressionList(final Expression expr1, final Expression expr2) {
		add(expr1);
		add(expr2);
	}

	public ExpressionList(final Expression[] expressionList) {
		for (Expression expression : expressionList)
			add(expression);
	}

	public ExpressionList(final IIterator<Expression> iterator1, final IIterator<Expression> iterator2) {
		addRange(iterator1);
		addRange(iterator2);
	}

	public ExpressionList(final IExpressionList left, final IExpressionList right) {
		addRange(left);
		addRange(right);
	}

	public ExpressionList(final IExpressionList left) {
		addRange(left);
	}

	public ExpressionList(final IIterator<Expression> iterator) {
		addRange(iterator);
	}

	public void addRange(final IExpressionList range) {
		addRange(range.getIterator());
	}

	public void addRange(final IIterator<Expression> toAdd) {
		while (toAdd.hasNext())
			add(toAdd.next());
	}

	public boolean hasLetter(final char letter) {
		for (int i = 0; i < length(); i++)
			if (elementAt(i).hasLetter(letter))
				return true;
		return false;
	}

	public Expression[] toArray() {
		final Expression[] toret = new Expression[length()];
		final IIterator<Expression> iterator = getIterator();
		for (int c = 0; iterator.hasNext(); c++)
			toret[c] = iterator.next();
		return toret;
	}

	/**
	 * Returns a new list with duplicates squashed.
	 * 
	 * @return The list with no duplicates
	 */
	public IExpressionList fold() {
		final IExpressionList toret = new ExpressionList();
		for (Expression current : this) {
			if (!toret.contains(current))
				toret.add(current);
		}
		return toret;
	}

	public void toCleanString(final char separator, final StringBuffer buffer) {
		toCleanString(this, separator, buffer);
	}

	public static void toCleanString(Iterable<Expression> expressions, char separator, StringBuffer buffer) {
		boolean first = true;
		for (Expression curr : expressions) {
			if (first)
				first = false;
			else
				buffer.append(separator);
			buffer.append(Cleaner.dematrix(curr.toCleanString()));
		}
	}

	public String toCleanString(final char separator) {
		return toCleanString(this, separator);
	}

	@NotNull
	public static String toCleanString(Iterable<Expression> expressions, char separator) {
		final StringBuffer toret = new StringBuffer();
		toCleanString(expressions, separator, toret);
		return toret.toString();
	}
}
