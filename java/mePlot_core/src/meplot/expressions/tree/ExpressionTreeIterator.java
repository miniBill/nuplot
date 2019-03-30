package meplot.expressions.tree;

public abstract class ExpressionTreeIterator implements java.util.Iterator<ExpressionTree> {
	public abstract boolean hasNext();

	public abstract ExpressionTree next();

	public abstract ExpressionTree peek();

	public abstract ExpressionTreeIterator subIterator();

	public final String toCString() {
		final ExpressionTreeIterator clone = subIterator();
		final StringBuffer buffer = new StringBuffer();
		while (clone.hasNext()) {
			// HACK: was .toCleanString
			buffer.append(clone.next().getValue().toString());
			if (clone.hasNext())
				buffer.append(',');
		}
		return buffer.toString();
	}
}
