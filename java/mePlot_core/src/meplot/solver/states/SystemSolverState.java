package meplot.solver.states;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.tree.ExpressionTree;
import meplot.solver.ISolver;
import platform.lists.IList;
import platform.lists.IterableExtensions;
import platform.lists.List;

abstract class SystemSolverState implements ISystemSolverState {
	private final ExpressionTree leave;
	private final ISolver solver;

	protected final ExpressionTree getLeaf() {
		return leave;
	}

	// TODO: nonstatic
	protected static final List<ISystemSolverState> QUEUE = new List<>();

	protected SystemSolverState(final ExpressionTree leave, final ISolver solver) {
		this.leave = leave;
		this.solver = solver;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see meplot.solver.ISystemSolverState#solve()
	 */
	public final void solve() {
		execute();
		while (!QUEUE.isEmpty()) {
			final ISystemSolverState curr = QUEUE.pop();
			curr.execute();
		}
	}

	public abstract void execute();

	protected static char getFirstVar(final char firstVar, final Expression curr) {
		char var = firstVar;
		while (var <= 'z' && !curr.hasLetter(var))
			var++;
		if (var > 'z')
			return '?';
		return var;
	}

	protected static IList<Expression> removeNullAndZero(final Iterable<Expression> equations) {
		final IList<Expression> toret = new List<>();
		for (Expression expression : equations) {
			if (expression != null)
				if (expression instanceof Letter) {
					if (expression.isIdentical(Letter.NOTEXISTS))
						return new List<>(Letter.NOTEXISTS);
				} else if (!expression.isZero() && !expression.equals(Letter.FORALL))
					toret.add(expression);
		}
        if (IterableExtensions.isEmpty(toret))
			return new List<>(Letter.FORALL);
		return toret;
	}

	public ISolver getSolver() {
		return solver;
	}
}
