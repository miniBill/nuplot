package meplot.expressions.list;

import meplot.expressions.Expression;
import meplot.expressions.ISimplifyNode;
import platform.lists.IIterator;
import platform.lists.List;

public final class SimplifyList extends List<ISimplifyNode> {
	public Expression simplify(final Expression next) {
		final IIterator<ISimplifyNode> iterator = getIterator();
		Expression toret = next;
		while (iterator.hasNext())
			toret = iterator.next().simplify(toret);
		return toret;
	}
}
