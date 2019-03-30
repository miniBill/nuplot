package meplot.parser;

import meplot.expressions.Expression;
import platform.lists.List;

final class DividedNodeList extends List<DividedNode> {
	public void add(final Expression expr, final char name) {
		super.add(new DividedNode(expr, name));
	}
}
