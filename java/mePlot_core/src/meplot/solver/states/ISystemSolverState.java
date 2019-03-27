package meplot.solver.states;

import meplot.expressions.tree.ExpressionTree;

public interface ISystemSolverState{
	void solve();

	ISystemSolverState fill(final ExpressionTree child);
}
