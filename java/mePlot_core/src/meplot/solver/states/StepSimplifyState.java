package meplot.solver.states;

import meplot.expressions.Expression;
import meplot.expressions.tree.ExpressionTree;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.solver.ISolver;
import org.jetbrains.annotations.Nullable;
import platform.lists.IList;
import platform.lists.IterableExtensions;

public class StepSimplifyState extends SystemSolverState {
	private final int index;
	@Nullable
	private final ISystemSolverState nextState;

	protected StepSimplifyState(final ExpressionTree tree, final int index, final ISolver solver,
								@Nullable final ISystemSolverState nextState) {
		super(tree, solver);
		this.index = index;
		this.nextState = nextState;
	}

	public StepSimplifyState(final ExpressionTree tree, final int index, final ISolver solver) {
		super(tree, solver);
		this.index = index;
		nextState = null;
	}

	public void execute() {
		if (index == IterableExtensions.length(getLeaf().getValue())) {
			final IList<Expression> equations = removeNullAndZero(getLeaf().getValue());
			final ExpressionTree child = getLeaf().addChild(equations);
			if (nextState != null)
				QUEUE.add(nextState.fill(child));
			return;
		}
        Iterable<Expression> expressions = getLeaf().getValue();
        final Expression[] equations = IterableExtensions.toArray(Expression.class, expressions);
		final Expression curr = equations[index];
		ExpressionTree leaf = getLeaf();
		for (Expression ce : SimplificationHelper.stepSimplify(curr)) {
			equations[index] = ce;
			leaf = leaf.addChild(equations);
		}
		equations[index] = SimplificationHelper.simplify(equations[index]);
		leaf = leaf.addChild(equations);
		final StepSimplifyState state = new StepSimplifyState(leaf, index + 1, getSolver(), nextState);
		QUEUE.insert(state, 0);
	}

	public ISystemSolverState fill(final ExpressionTree child) {
		return new StepSimplifyState(child, index, getSolver(), nextState);
	}
}
