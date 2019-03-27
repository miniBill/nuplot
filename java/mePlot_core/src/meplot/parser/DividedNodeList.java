package meplot.parser;

import meplot.expressions.Expression;
import platform.lists.List;

final class DividedNodeList extends List{
	public void add(final Expression expr, final char name){
		super.add(new DividedNode(expr, name));
	}

	public DividedNode elementAt(final int index){
		return (DividedNode)gelementAt(index);
	}
}
