package meplot.expressions.list;

import meplot.expressions.Expression;
import meplot.expressions.ISimplifyNode;
import platform.lists.List;

public final class SimplifyList extends List<ISimplifyNode> {
	public Expression simplify(final Expression next) {
		Expression toret = next;
		for (ISimplifyNode curr : this)
			toret = curr.simplify(toret);
		return toret;
	}
}
