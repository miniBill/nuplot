package meplot.solver.states;

import meplot.expressions.Expression;
import meplot.expressions.list.IExpressionIterable;
import meplot.expressions.list.IExpressionIterator;
import meplot.expressions.tree.ExpressionTree;
import meplot.expressions.visitors.simplification.SimplificationHelper;
import meplot.solver.ISolver;

public class StepSimplifyState extends SystemSolverState{
	private final int index;
	private final ISystemSolverState nextState;

	protected StepSimplifyState(final ExpressionTree tree, final int index, final ISolver solver,
			final ISystemSolverState nextState){
		super(tree, solver);
		this.index = index;
		this.nextState = nextState;
	}

	public StepSimplifyState(final ExpressionTree tree, final int index, final ISolver solver){
		super(tree, solver);
		this.index = index;
		nextState = null;
	}

	protected void execute(){
		if(index == getLeaf().getValue().length()){
			final IExpressionIterable equations = removeNullAndZero(getLeaf().getValue());
			final ExpressionTree child = getLeaf().addChild(equations);
			if(nextState != null)
				QUEUE.add(nextState.fill(child));
			return;
		}
		final Expression[] equations = getLeaf().getValue().toArray();
		final Expression curr = equations[index];
		final IExpressionIterator iterator = SimplificationHelper.stepSimplify(curr);
		ExpressionTree leaf = getLeaf();
		while(iterator.hasNext()){
			equations[index] = iterator.next();
			leaf = leaf.addChild(equations);
		}
		equations[index] = SimplificationHelper.simplify(equations[index]);
		leaf = leaf.addChild(equations);
		final StepSimplifyState state = new StepSimplifyState(leaf, index + 1, getSolver(), nextState);
		QUEUE.insert(state, 0);
	}

	public ISystemSolverState fill(final ExpressionTree child){
		return new StepSimplifyState(child, index, getSolver(), nextState);
	}
}
