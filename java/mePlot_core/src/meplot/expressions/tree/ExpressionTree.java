package meplot.expressions.tree;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.operations.BooleanOp;
import platform.lists.IList;
import platform.lists.IterableExtensions;
import platform.lists.List;
import platform.lists.ToStringList;

import java.util.Iterator;

public final class ExpressionTree {
	private ExpressionTree child;
	private final IList<Expression> value;
	private ExpressionTree brother;

	private ExpressionTree(final IList<Expression> list) {
		value = list;
	}

	public ExpressionTree(final Expression val) {
		value = new List<>(val);
	}

	public String toString() {
		final StringBuilder buffer = new StringBuilder();
		toString(buffer);
		return buffer.toString();
	}

	private void toString(final StringBuilder buffer) {
		if (hasBrother()) {
			buffer.append('(');
			ToStringList.toString(value, buffer);
			if (hasChild()) {
				buffer.append('[');
				child.toString(buffer);
				buffer.append(']');
			}
			buffer.append(" o ");
			brother.toString(buffer);
			buffer.append(')');
			return;
		}
		ToStringList.toString(value, buffer);
		if (hasChild()) {
			buffer.append(',');
			child.toString(buffer);
		}
	}

	public Iterable<Expression> getValue() {
		return value;
	}

	private ExpressionTree addBrother(final IList<Expression> equations) {
		return addBrother(new ExpressionTree(equations));
	}

	public ExpressionTree stringFold() {
		if (child != null)
			child.stringFold();
		if (brother != null)
			brother.stringFold();

		final String cString = ExpressionList.toCleanString(value, ',');
		Iterable<ExpressionTree> iterable = this::getAllChildren;
		for (ExpressionTree curr : iterable) {
			if (curr != this && cString.equals(ExpressionList.toCleanString(curr.value, ','))) {
				child = curr.child;
				break;
			}
		}
		return this;
	}

	private ExpressionTree addBrother(final ExpressionTree bros) {
		if (brother == null)
			brother = bros;
		else
			brother.addBrother(bros);
		return brother;
	}

	private ExpressionTreeIterator getAllChildren() {
		return new ExpressionTreeIteratorImpl(this);
	}

	public ExpressionTree addChild(final IList<Expression> equations) {
		if (child == null) {
			child = new ExpressionTree(equations);
			return child;
		}
		return child.addBrother(equations);
	}

	public ExpressionTree addBrother(final Expression expr) {
		return addBrother(new ExpressionTree(expr));
	}

	public ExpressionTree addChild(final Expression expr) {
		if (child == null) {
			child = new ExpressionTree(expr);
			return child;
		}
		return child.addChild(expr);
	}

	public ExpressionTreeIterator getLeaves() {
		return new ExpressionTreeLeafIterator(getAllChildren());
	}

	public ExpressionTree getBrother() {
		return brother;
	}

	public ExpressionTree getChild() {
		return child;
	}

	public boolean hasChild() {
		return child != null;
	}

	public boolean hasBrother() {
		return brother != null;
	}

	public String toCleanString(final char separator) {
		final StringBuilder buffer = new StringBuilder();
		toCleanString(separator, buffer);
		return buffer.toString();
	}

	private void toCleanString(final char separator, final StringBuilder buffer) {
		toTreeString(separator, buffer, 0);
	}

	private void toTreeString(final char separator, final StringBuilder buffer, final int index) {
		if (hasBrother()) {
			appendTree(buffer, index);
			buffer.append('°');
			ExpressionList.toCleanString(value, ',', buffer);
			if (hasChild()) {
				buffer.append(separator);
				child.toTreeString(separator, buffer, index + 1);
			}
			buffer.append(separator);
			appendTree(buffer, index);
			buffer.append('°');
			ExpressionList.toCleanString(brother.value, ',', buffer);
			if (brother.hasChild()) {
				buffer.append(separator);
				brother.child.toTreeString(separator, buffer, index + 1);
			}
			return;
		}
		appendTree(buffer, index);
		ExpressionList.toCleanString(value, ',', buffer);
		if (hasChild()) {
			buffer.append(separator);
			child.toTreeString(separator, buffer, index);
		}
	}

	private static void appendTree(final StringBuilder buffer, final int nested) {
		for (int a = 0; a < nested - 1; a++)
			buffer.append(' ');
		if (nested != 0)
			buffer.append('|');
	}

	public void toHtml(final StringBuilder buffer) {
		if (hasBrother())
			buffer.append("<table><tr><td><div class=\"border\">\n");
		outputValue(buffer);
		buffer.append("<br/>\n");
		if (hasChild())
			child.toHtml(buffer);
		if (hasBrother()) {
			buffer.append("</div>\n</td><td><div class=\"border\">");
			brother.toHtml(buffer);
			buffer.append("</div>\n</td></tr></table>");
		}
	}

	private void outputValue(final StringBuilder buffer) {
		if (IterableExtensions.length(value) > 1) {
			appendTable(buffer, value.iterator());
			return;
		}
		if (IterableExtensions.isSingle(value) && IterableExtensions.getFirst(value) instanceof Matrix) {
			Matrix mat = (Matrix) IterableExtensions.getFirst(value);
			if (mat.getRows() * mat.getCols() > 0 && mat.get(0, 0) instanceof BooleanOp) {
				appendTable(buffer, mat.iterator());
				return;
			}
		}
		buffer.append('$');
		if (IterableExtensions.length(value) > 0)
			IterableExtensions.getFirst(value).toHtml(buffer);
		/*
		 * else buffer.append("∀");
		 */
		buffer.append('$');
	}

	private static void appendTable(final StringBuilder buffer, final Iterator<Expression> iterator) {
		buffer.append("$\\{\\table ");
		while (iterator.hasNext()) {
			iterator.next().toWrappedHtml(buffer);
			if (iterator.hasNext())
				buffer.append("; ");
			else
				buffer.append('$');
		}
	}

	public void addChild(final ExpressionTree node) {
		if (child == null)
			child = node;
		else
			child.addBrother(node);
	}

	@Deprecated
	public ExpressionTree addChild(final Expression[] expressions) {
		return addChild(new List<Expression>(expressions));
	}
}
