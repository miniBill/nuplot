package meplot.android.paid.gui;

import meplot.android.gui.activities.Solver;
import meplot.expressions.Expression;
import meplot.solver.ISolver;
import meplot.solver.Solution;

public class PaidSolver extends Solver{
	private final ISolver solver = new meplot.android.paid.solver.PaidSolver();

	@Override
	protected final Solution solve(final Expression input){
		return solver.solve(input);
	}
}
