package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import meplot.expressions.Expression;
import meplot.expressions.IOutputtable;
import meplot.expressions.exceptions.DerivationException;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.list.ExpressionList;
import org.jetbrains.annotations.NotNull;
import org.junit.Assert;
import platform.lists.IList;
import platform.lists.IterableExtensions;
import meplot.expressions.numbers.Int;
import meplot.expressions.other.PolynomialMath;
import meplot.expressions.visitors.derivative.DerivativeHelper;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.parser.Parser;
import meplot.parser.ParserException;
import meplot.parser.utils.Cleaner;
import platform.lists.List;
import platform.log.FilterLogger;
import platform.log.Log;
import platform.log.LogLevel;

public abstract class TestUtils {
	@NotNull
	public static Expression parseOrFail(final String arg) {
		try {
			return Parser.parse(arg);
		} catch (final ParserException e) {
			Assert.fail("Failed parsing " + arg);
			return null;
		}
	}

	protected static Expression justParse(final String arg, final String target) {
		final Expression expr = parseOrFail(arg);
		reparse(expr);
		final Object simplified = SimplificationHelper.simplify(expr);
		final String simplifiedString = simplified.toString();
		assertEquals("justParse error", target, Cleaner.clean(simplifiedString));
		return expr;
	}

	protected static void checkFullSimplify(final Expression[] expressions, final String... strings) {
		for (int i = 0; i < expressions.length; i++)
			checkFullSimplify(expressions[i], strings[i]);
	}

	protected static void checkFullSimplify(final Expression expr, final String target) {
		final Expression eSimplified = SimplificationHelper.simplify(expr);
		final String simplified = eSimplified.toFullString();
		assertEquals("In checkFullSimplify:", target, simplified);
	}

	protected static Expression checkSimplify(final Expression expr, final String target) {
		final Expression eSimplified = SimplificationHelper.simplify(expr);
		final String simplified = eSimplified.toString();
		final String simplifiedClean = Cleaner.clean(simplified);
		assertEquals("In checkSimplify:", target, simplifiedClean);
		reparse(eSimplified);
		return eSimplified;
	}

	protected static void checkWithDerivatives(final String function, final String derivative, final String dderivative,
			final char var) {
		checkWithDerivatives(function, function, derivative, dderivative, var);
	}

	protected static void checkWithDerivatives(final String input, final String function, final String derivative,
			final String dderivative, final char var) {
		final Expression parsed = parseOrFail(input);

		final Expression parsedSimplified = checkSimplify(parsed, function);
		if ("?".equals(derivative))
			return;

		final Expression eDerivative = derivativeOrFail(parsedSimplified, var);
		final Expression eDerSimplified = checkSimplify(eDerivative, derivative);

		if ("?".equals(dderivative))
			return;
		final Expression eDDerivative = derivativeOrFail(eDerSimplified, var);
		checkSimplify(eDDerivative, dderivative);
	}

	protected static void reparse(final Expression arg) {
		final Object aSimplified = SimplificationHelper.simplify(arg);
		final String aSimplifiedString = aSimplified.toString();

		final String aString = arg.toString();
		final String aStringClean = Cleaner.clean(aString);

		final Expression reparsed = parseOrFail(aStringClean);

		final Object reparsedSim = SimplificationHelper.simplify(reparsed);
		final String reparsedSimString = reparsedSim.toString();

		assertEquals("Reparsing:", aSimplifiedString, reparsedSimString);
	}

	protected static Expression derivativeOrFail(final Expression expr, final char var) {
		try {
			return DerivativeHelper.derivative(expr, var);
		} catch (final DerivationException e) {
			fail("Derivation of " + expr + " with respect to " + var + " failed");
			return null;
		}
	}

	protected static Expression justParse(final String arg) {
		return justParse(arg, arg);
	}

	protected final void assertSimplify(final Expression input, final String expected) {
		assertSimplify(input, expected, true);
	}

	protected final void assertSimplify(final Expression input, final String expected, final boolean doManual) {
		final IOutputtable simplified = SimplificationHelper.simplify(input);
		final String simplifiedClean = simplified.toCleanString();

		final Iterable<Expression> step = SimplificationHelper.stepSimplify(input);
		final String stepClean = IterableExtensions.getLast(step).toCleanString();

		final Expression expectedParsed = parseOrFail(expected);
		final IOutputtable expectedSimplified = SimplificationHelper.simplify(expectedParsed);
		final String expectedClean = expectedSimplified.toCleanString();

		assertEquals("dir/step", simplifiedClean, stepClean);

		assertEquals("Simplification gone wrong", expectedClean, simplifiedClean);
		assertEquals("Step gone wrong", expectedClean, stepClean);

		if (doManual) {
			final IList<Expression> inputManual = manualSimplify(input);
			final String manualClean = IterableExtensions.getLast(inputManual).toCleanString();
			assertEquals("step/man", stepClean, manualClean);
			assertEquals("dir/man", simplifiedClean, manualClean);
			assertEquals("Manual gone wrong", expectedClean, manualClean);
		}
	}

	private IList<Expression> manualSimplify(final Expression input) {
		if (input.isSimplified())
			return new List<>(input);
		if ("0".equals(input.toString()))// otherwise it could FAIL
			return new List<>(Int.ZERO);
		Expression current = Int.ZERO;
		Expression next = input;
		final int max = toString().length() > 25 ? 20 : 100;
		final List<Expression> toret = new List<>(next);
		for (int safety = 0; safety < max && next != null && !current.isIdentical(next); safety++) {
			current = parseOrFail(next.toCleanString());
			next = current;
			do {
				next = next.innerStepSimplify();
				safety++;
			} while (safety < max && next != null && parseOrFail(next.toCleanString()).isIdentical(current));

			toret.add(next);
			if (safety == 99)
				// ESCA-JAVA0285:
				Log.log(LogLevel.WARNING,
						"Exceeded 100 iterations in stepSimplify(), last was " + (next == null ? "" : next.toString()));
			if (current.isIdentical(next) || safety == max || next == null)
				safety += 0;
		}
		// ESCA-JAVA0285:
		if (current.equals(next) && next != null && next.toString().equals(current.toString())
				&& !current.toFullString().equals(next.toFullString())) {
			final String currString = current.toString();
			final String currFullString = current.toFullString();
			final String nextFullString = next.toFullString();
			Log.log(LogLevel.DEBUG, "Identical different objects", currString, currFullString, nextFullString);
		}
		return toret;
	}

	protected final void assertSimplify(final String input, final String expected) {
		assertSimplify(input, expected, true);
	}

	protected final void assertSimplify(final String input, final String expected, final boolean doManual) {
		final Expression expr = parseOrFail(input);
		assertSimplify(expr, expected, doManual);
	}

	protected TestUtils() {
		setLogToFail();
	}

	protected static void setLogToFail() {
		Log.setLogger(new FilterLogger(LogLevel.INFO, new FailLog()));
	}

	protected static void setLogToNormal() {
		Log.setLogger(new FilterLogger(LogLevel.MAX, new FailLog()));
	}

	protected static void checkRoots(final Expression e, final char x, final String out) {
		final IList<Expression> roots = PolynomialMath.getRoots(e, x);
		assertEquals(out, ExpressionList.toString(roots));
	}

	protected static void checkPrecision(final double val, final double target, final double percent) {
		final double delta = val - target;
		if (percent == 0)
			assertEquals("Too low precision, expected " + target + " got " + val + ", so error was " + delta
					+ " but tolerance was 0", delta, 0, Double.MIN_VALUE);
		assertTrue(getPrecisionErrorMessage(target, val, percent, delta),
				Math.abs(delta) <= percent / 100.0 * Math.abs(target));
	}

	private static String getPrecisionErrorMessage(final double target, final double val, final double percent,
			final double delta) {
		return "Too low precision, expected " + target + " got " + val + ", so error was "
				+ Math.abs(delta / target) * 100.0 + "% that is" + delta + " but tolerance was " + percent
				+ "%, that is " + percent / 100.0 * Math.abs(target);
	}

	@NotNull
	protected static Matrix matrixOrFail(final String string) {
		final Object candidate = parseOrFail(string);
		if (candidate instanceof Matrix)
			return (Matrix) candidate;
		fail(string + " did not parse to Matrix but to " + candidate);
		return null;
	}
}
