package meplot.expressions.visitors.simplification;

import meplot.expressions.Expression;
import meplot.expressions.IOutputtable;
import meplot.expressions.list.ExpressionList;
import platform.lists.IIterator;
import meplot.expressions.list.SimplifyList;
import meplot.expressions.numbers.Int;
import platform.log.Log;
import platform.log.LogLevel;

public final class SimplificationHelper {
	private static final int MAX_SIMPLIFY_ITERATIONS = 99;

	private static final SimplifyList SIMPLIFY_CHAIN = new SimplifyList();

	public static Expression simplify(final Expression arg) {
		if (arg.isSimplified())
			return arg;
		if ("0".equals(arg.toString()))// otherwise it could FAIL
			return Int.ZERO;
		Expression current = Int.ZERO;
		Expression next = arg;
		final int max = arg.toString().length() > 25 ? 10 : MAX_SIMPLIFY_ITERATIONS;
		for (int safety = 0; safety < max; safety++) {
			current = next;
			if (next == null) {
				Log.log(LogLevel.ERROR, "Next null in simplify()");
				break;
			}
			next = next.innerSimplify();
			next = SIMPLIFY_CHAIN.simplify(next);
			if (safety == MAX_SIMPLIFY_ITERATIONS - 1) {
				final String nextString = next == null ? "null" : next.toString();
				current.isIdentical(next);
				Log.log(LogLevel.WARNING,
						"Exceeded " + MAX_SIMPLIFY_ITERATIONS + " iterations in Simplify(), last was " + nextString);
			}
			if (current.isIdentical(next))
				break;
		}
		checkIdentical(current, next, "Next is null in simplify()");
		return next;
	}

	public static IIterator<Expression> stepSimplify(final Expression arg) {
		if (arg.isSimplified())
			return new ExpressionList(arg).getIterator();
		String argstring = arg.toString();
		if ("0".equals(argstring))// otherwise it could FAIL
			return new ExpressionList(Int.ZERO).getIterator();
		Expression current = Int.ZERO;
		Expression next = arg;
		final int max = argstring.length() > 25 ? 10 : 100;
		final ExpressionList toret = new ExpressionList(next);
		for (int safety = 0; safety < max; safety++) {
			current = next;
			if (next == null) {
				Log.log(LogLevel.ERROR, "Next null in simplify()");
				break;
			}
			next = next.innerStepSimplify();
			toret.add(next);
			if (safety == MAX_SIMPLIFY_ITERATIONS - 1) {
				final int togot = 10;
				final StringBuffer lastString = new StringBuffer();
				for (int i = 1; i <= togot; i++) {
					lastString.append('\n');
					final Expression curr = toret.elementAt(toret.length() - i);
					curr.toString(lastString);
					lastString.append(':');
					curr.toFullString(lastString);
				}
				Log.log(LogLevel.WARNING, "Exceeded " + MAX_SIMPLIFY_ITERATIONS + " iterations in Simplify(), last "
						+ togot + " where:" + lastString.toString());
			}
			if (current.isIdentical(next))
				break;
		}
		checkIdentical(current, next, "Next is null in stepSimplify()");
		return toret.getIterator();
	}

	private static void checkIdentical(final IOutputtable current, final IOutputtable next, final String error) {
		if (current.equals(next))
			if (next == null)
				Log.log(LogLevel.ERROR, error);
			else {
				final String currString = current.toString();
				final String currFullString = current.toFullString();
				final String nextString = next.toString();
				final String nextFullString = next.toFullString();
				if (nextString.equals(currString) && !currFullString.equals(nextFullString))
					Log.log(LogLevel.DEBUG, "Identical different objects", currString, currFullString, nextFullString);
			}
	}

	private SimplificationHelper() {
	}
}
