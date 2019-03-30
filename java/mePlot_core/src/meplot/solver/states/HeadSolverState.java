package meplot.solver.states;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionIterable;
import platform.lists.IIterator;
import meplot.expressions.list.IExpressionList;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.operations.Operation;
import meplot.expressions.tree.ExpressionTree;
import meplot.solver.ISolver;

public class HeadSolverState extends SystemSolverState {
	public HeadSolverState(final ExpressionTree tree, final ISolver solver) {
		super(tree, solver);
	}

	public void execute() {
		final IExpressionIterable last = getLeaf().getValue();
		final ExpressionList toPass = new ExpressionList();
		final IIterator<Expression> iterator = last.getIterator();
		while (iterator.hasNext()) {
			final Expression value = iterator.next();
			if (value instanceof BooleanOp) {
				final BooleanOp boo = (BooleanOp) value;
				if (boo.getLeft() instanceof Matrix && boo.getRight().isZero()) {
					final Matrix m = (Matrix) boo.getLeft();
					addMatrix(toPass, m);
				} else
					toPass.add(value);
			} else if (value instanceof Matrix)
				addMatrix(toPass, (Matrix) value);
			else
				toPass.add(value);
		}

		final ExpressionTree leaf = getLeaf().addChild(toPass);
		final ForwardSubState nextState = new ForwardSubState(null, getSolver());
		final StepSimplifyState stepState = new StepSimplifyState(leaf, 0, getSolver(), nextState);
		QUEUE.add(stepState);
	}

	private static void addMatrix(final IExpressionList list, final Matrix m) {
		final IIterator<Expression> elements = m.getElements();
		while (elements.hasNext()) {
			final Expression curr = elements.next();
			if (curr instanceof BooleanOp)
				list.add(curr);
			else
				list.add(new BooleanOp(curr, Operation.EQUALS, Int.ZERO));
		}
	}

	public ISystemSolverState fill(final ExpressionTree child) {
		return new HeadSolverState(child, getSolver());
	}
}
