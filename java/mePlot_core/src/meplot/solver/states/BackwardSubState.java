package meplot.solver.states;

import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.list.IExpressionIterable;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.operations.Operation;
import meplot.expressions.tree.ExpressionTree;
import meplot.solver.ISolver;
import platform.lists.IList;
import platform.lists.IterableExtensions;

class BackwardSubState extends SystemSolverState {
	private final int index;

	BackwardSubState(final ExpressionTree leaf, final ISolver solver) {
		super(leaf, solver);
		if (leaf == null)
			index = -1;
		else
            index = IterableExtensions.length(leaf.getValue()) - 1;
	}

	private BackwardSubState(final ExpressionTree leaf, final int index, final ISolver solver) {
		super(leaf, solver);
		this.index = index;
	}

	public void execute() {
		if (index == -1) {
			nextStep();
			return;
		}
        Iterable<Expression> expressions = getLeaf().getValue();
        final Expression[] equations = IterableExtensions.toArray(Expression.class, expressions);
		if (index >= equations.length) {
			QUEUE.add(new BackwardSubState(getLeaf(), equations.length - 1, getSolver()));
			return;
		}
		final Expression curr = equations[index];
		ExpressionTree leaf = getLeaf();
		if (curr instanceof BooleanOp) {
			final BooleanOp bcurr = (BooleanOp) curr;
			if (bcurr.getLeft() instanceof Letter && bcurr.getBool() == Operation.EQUALS) {
				final Letter lvar = (Letter) bcurr.getLeft();
				final char var = lvar.getLetter();
				final Expression tosub = bcurr.getRight();
				for (int d = index - 1; d >= 0; d--) {
					equations[d] = equations[d].partialSubstitute(var, tosub);
					leaf = leaf.addChild(equations);
				}
			}
		}
		QUEUE.add(new BackwardSubState(leaf, index - 1, getSolver()));
	}

	private void nextStep() {
		final IList<Expression> equations = removeNullAndZero(getLeaf().getValue());
		final ExpressionTree child = getLeaf().addChild(equations);
		QUEUE.add(new StepSimplifyState(child, 0, getSolver()));
	}

	public ISystemSolverState fill(final ExpressionTree child) {
		return new BackwardSubState(child, index, getSolver());
	}
}
