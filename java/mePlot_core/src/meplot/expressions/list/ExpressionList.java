package meplot.expressions.list;

import meplot.expressions.Expression;
import meplot.parser.utils.Cleaner;
import org.jetbrains.annotations.NotNull;

@Deprecated
public final class ExpressionList  {
	public static boolean hasLetter(Iterable<Expression> list, char letter) {
		for (Expression expr : list)
			if (expr.hasLetter(letter))
				return true;
		return false;
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

	@NotNull
	public static String toCleanString(Iterable<Expression> expressions, char separator) {
		final StringBuilder toret = new StringBuilder();
		toCleanString(expressions, separator, toret);
		return toret.toString();
	}
}
