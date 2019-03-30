package meplot.solver.states;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionIterable;
import platform.lists.IIterator;
import platform.lists.List;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.tree.ExpressionTree;
import meplot.solver.ISolver;

abstract class SystemSolverState implements ISystemSolverState {
	private final ExpressionTree leave;
	private final ISolver solver;

	protected final ExpressionTree getLeaf() {
		return leave;
	}

	// TODO: nonstatic
	protected static final List<ISystemSolverState> QUEUE = new List<ISystemSolverState>();

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

	protected static IExpressionIterable removeNullAndZero(final IExpressionIterable equations) {
		final IExpressionList toret = new ExpressionList();
		final IIterator<Expression> iterator = equations.getIterator();
		while (iterator.hasNext()) {
			final Expression expression = iterator.next();
			if (expression != null)
				if (expression instanceof Letter) {
					if (expression.isIdentical(Letter.NOTEXISTS))
						return new ExpressionList(Letter.NOTEXISTS);
				} else if (!expression.isZero() && !expression.equals(Letter.FORALL))
					toret.add(expression);
		}
		if (toret.isEmpty())
			return new ExpressionList(Letter.FORALL);
		return toret;
	}

	public ISolver getSolver() {
		return solver;
	}
}
