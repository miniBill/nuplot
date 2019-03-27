package test.solver;

import static org.junit.Assert.assertEquals;
import meplot.solver.PaidSolver;
import meplot.expressions.Expression;
import meplot.parser.utils.Cleaner;
import meplot.solver.ISolver;
import meplot.solver.Solution;
import meplot.solver.Solver;
import test.TestUtils;

public abstract class SolverUtils extends TestUtils {
	private static final ISolver paidsolver = new PaidSolver();
	private static final ISolver solver = new Solver();

	protected static void checkHtmlSolve(final Expression input, final String expected, final boolean paid) {
		final Solution sol;
		if (paid)
			sol = paidsolver.solve(input);
		else
			sol = solver.solve(input);
		StringBuffer buffer = new StringBuffer();
		sol.toHtml(buffer);
		assertEquals(expected, buffer.toString());
	}

	protected static void checkHtmlSolve(final String input, final String expected) {
		checkHtmlSolve(input, expected, false);
	}

	protected static void checkHtmlSolve(final String input, final String expected, final boolean paid) {
		final Expression eq = parseOrFail("'" + input);
		checkHtmlSolve(eq, expected, paid);
	}

	protected static void checkLeaves(final String input, final String expected) {
		final Expression eq = parseOrFail("'" + input);
		final Solution sol = solver.solve(eq);
		assertEquals(expected, Cleaner.dematrix(Cleaner.clean(sol.getSteps().getLeaves().toCString())));
	}

	public static void checkSolve(final Expression input, final String expected) {
		checkSolve(input, expected, false);
	}

	protected static void checkSolve(final Expression input, final String expected, final boolean paid) {
		final Solution sol;
		if (paid)
			sol = paidsolver.solve(input);
		else
			sol = solver.solve(input);
		assertEquals(expected, sol.toString());
	}

	protected static void checkSolve(final String input, final String expected) {
		checkSolve(input, expected, false);
	}

	protected static void checkSolve(final String input, final String expected, final boolean paid) {
		final Expression eq = parseOrFail("'" + input);
		checkSolve(eq, expected, paid);
	}
}
