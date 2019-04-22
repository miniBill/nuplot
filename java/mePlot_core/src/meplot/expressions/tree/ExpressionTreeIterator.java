package meplot.expressions.tree;

import platform.lists.IterableExtensions;
import platform.lists.ToStringList;

public abstract class ExpressionTreeIterator implements java.util.Iterator<ExpressionTree> {
	public abstract ExpressionTree peek();

	public abstract ExpressionTreeIterator subIterator();

	public final String toCString() {
		final StringBuilder buffer = new StringBuilder();
		boolean first = true;
		for (ExpressionTree expressionTree : IterableExtensions.wrap(subIterator())) {
			// HACK: was .toCleanString
			if (!first)
				buffer.append(',');
			first = false;
			ToStringList.toString(expressionTree.getValue(), buffer);
		}
		return buffer.toString();
	}
}
