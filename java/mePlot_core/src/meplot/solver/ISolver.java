package meplot.solver;

import meplot.expressions.Expression;
import meplot.expressions.tree.ExpressionTree;

public interface ISolver{
	Solution solve(final Expression input);

	Solution solve(final Expression input, final char var);

	ExpressionTree explicate(Expression curr, final char var);
}
