package meplot.solver.states;

import meplot.expressions.Expression;
import meplot.expressions.ISubstitutible;
import meplot.expressions.Letter;
import meplot.expressions.exceptions.CalcException;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.operations.BooleanOp;
import meplot.expressions.operations.Operation;
import meplot.expressions.tree.ExpressionTree;
import meplot.expressions.tree.ExpressionTreeIterator;
import meplot.solver.AbstractSolver;
import meplot.solver.ISolver;
import meplot.solver.SolveException;
import platform.lists.IList;
import platform.lists.IterableExtensions;
import platform.lists.List;
import platform.log.Log;
import platform.log.LogLevel;

import java.util.Iterator;

class ForwardSubState extends SystemSolverState {
	private final int index;
	private char firstVar = '?';

	ForwardSubState(final ExpressionTree leave, final ISolver solver) {
		super(leave, solver);
		index = 0;
	}

	private ForwardSubState(final ExpressionTree leave, final int index, final char firstVar, final ISolver solver) {
		super(leave, solver);
		this.index = index;
		this.firstVar = firstVar;
	}

	public void execute() {
		if (firstVar == '?')
			firstVar = AbstractSolver.getFirstVar(getLeaf().getValue());
		ExpressionTree leaf = getLeaf();
		if (index >= IterableExtensions.length(getLeaf().getValue())) {
			final IList<Expression> equations = removeNullAndZero(getLeaf().getValue());
			leaf = leaf.addChild(equations);
			// TODO: leaf is incorrect
			final BackwardSubState nextState = new BackwardSubState(leaf, getSolver());
			final StepSimplifyState simplifyStep = new StepSimplifyState(leaf, 0, getSolver(), nextState);
			QUEUE.add(simplifyStep);
			return;
		}
        Iterable<Expression> expressions = getLeaf().getValue();
        final Expression[] equations = IterableExtensions.toArray(Expression.class, expressions);
		final Expression curr = equations[index];
		if (curr == null) {
			Log.log(LogLevel.DEBUG, "Null equation");
			return;
		}
		if (curr.equals(Letter.NOTEXISTS)) {
			leaf.addChild(curr);
			return;
		}
		if (curr.isZero()) {
			QUEUE.add(new ForwardSubState(leaf, index + 1, firstVar, getSolver()));
			return;
		}
		final char first = getFirstVar(firstVar, curr);
		if (first == '?') {
			leaf.addChild(equations);
			QUEUE.add(new ForwardSubState(leaf, index + 1, firstVar, getSolver()));
			return;
		}

		try {
			final ExpressionTree local = getSolver().explicate(curr, first);
			addSteps(leaf, local, index, index);
		} catch (CalcException ce) {
			leaf.addChild(Letter.UNKNOWN);
		}

		doSubstitution(first);
	}

	private static void addSteps(final ExpressionTree leaf, final ExpressionTree steps, final int startindex,
			final int endindex) {
		final Iterable<Expression> value = leaf.getValue();
		final Iterator<Expression> iterator = value.iterator();
		final List<Expression> current = new List<Expression>();
		final Iterable<Expression> stepValue = steps.getValue();
		int newlength = 0;
		for (int i = 0; i < IterableExtensions.length(value); i++)
			if (i < startindex || i > endindex)
				current.add(iterator.next());
			else {
				iterator.next(); // Ignores it
				if (i == startindex) {
					newlength = IterableExtensions.length(stepValue);
					IterableExtensions.addRange(current, stepValue);
				}
			}
		final ExpressionTree child = leaf.addChild(current);
		if (steps.hasChild())
			addSteps(child, steps.getChild(), startindex, startindex + newlength - 1);
		if (steps.hasBrother())
			addSteps(leaf, steps.getBrother(), startindex, startindex + newlength - 1);
	}

	private void doSubstitution(final char first) {
		final ExpressionTreeIterator iterator = getLeaf().getLeaves();
		while (iterator.hasNext()) {
			ExpressionTree leaf = iterator.next();

            Iterable<Expression> expressions = leaf.getValue();
            final Expression[] equations = IterableExtensions.toArray(Expression.class, expressions);
			if (index >= equations.length)
				continue;
			final Expression boo = equations[index];
			if (!(boo instanceof BooleanOp)) {
				if (!boo.equals(Letter.FORALL) && !boo.equals(Letter.NOTEXISTS) && !boo.equals(Letter.UNKNOWN)) {
					throw new SolveException(
							"boo is not a BooleanOp, nor Letter.FORALL, nor Letter.NOTEXISTS, but " + boo);
				}
			} else {
				final BooleanOp sub = (BooleanOp) boo;
				if (sub.getBool() == Operation.EQUALS) {
					final Expression tosub = sub.getRight();

					final char var = getFirstVar(first, sub.getLeft());

					for (int d = index + 1; d < equations.length; d++) {
						final ISubstitutible currEq = equations[d];
						if (currEq == null) {
							Log.log(LogLevel.WARNING, "NULLINNER");
							leaf.addChild(Letter.UNKNOWN);
							return;
						}
						final Expression subbed = currEq.partialSubstitute(var, tosub);
						if (subbed == null) {
							Log.log(LogLevel.WARNING, "NULLSUBBED");
							leaf.addChild(Letter.UNKNOWN);
							return;
						}

						equations[d] = subbed;
						leaf = leaf.addChild(equations);
					}
				}
			}
			final ForwardSubState nextState = new ForwardSubState(leaf, index + 1, firstVar, getSolver());
			QUEUE.add(new StepSimplifyState(leaf, index + 1, getSolver(), nextState));
		}
	}

	public ISystemSolverState fill(final ExpressionTree child) {
		return new ForwardSubState(child, index, firstVar, getSolver());
	}
}
