package meplot.expressions.list;

import meplot.expressions.Expression;
import meplot.parser.utils.Cleaner;
import org.jetbrains.annotations.NotNull;
import platform.lists.IList;
import platform.lists.IterableExtensions;
import platform.lists.List;
import platform.lists.ToStringList;

import java.util.Iterator;

@Deprecated
public final class ExpressionList extends ToStringList<Expression> implements IExpressionIterable, IList<Expression> {
	private static final ExpressionList EMPTY = new ExpressionList();

	public static ExpressionList getEmpty() {
		return EMPTY;
	}

	@Deprecated
	public ExpressionList(final Expression expr) {
		add(expr);
	}

	/**
	 * Creates empty list.
	 */
	@Deprecated
	public ExpressionList() {
		// Creates empty list
	}

	@Deprecated
	public void addRange(final Iterator<Expression> toAdd) {
		while (toAdd.hasNext())
			add(toAdd.next());
	}

	public static boolean hasLetter(Iterable<Expression> list, char letter) {
		for (Expression expr : list)
			if (expr.hasLetter(letter))
				return true;
		return false;
	}

	@NotNull
	public static IList<Expression> unique(Iterable<Expression> list) {
		final IList<Expression> toret = new List<>();
		for (Expression current : list) {
			if (!IterableExtensions.contains(toret, current))
				toret.add(current);
		}
		return toret;
	}

	public void toCleanString(final char separator, final StringBuilder buffer) {
		toCleanString(this, separator, buffer);
	}

	public static void toCleanString(Iterable<Expression> expressions, char separator, StringBuilder buffer) {
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
		final StringBuilder toret = new StringBuilder();
		toCleanString(expressions, separator, toret);
		return toret.toString();
	}
}
