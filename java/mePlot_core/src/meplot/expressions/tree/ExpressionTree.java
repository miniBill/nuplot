package meplot.expressions.tree;

import java.util.Iterator;

import meplot.expressions.Expression;
import meplot.expressions.geometry.Matrix;
import meplot.expressions.list.ExpressionList;
import meplot.expressions.list.IExpressionIterable;
import platform.lists.IIterator;
import meplot.expressions.operations.BooleanOp;

public final class ExpressionTree {
	private ExpressionTree child;
	private final IExpressionIterable value;
	private ExpressionTree brother;

	private ExpressionTree(final IExpressionIterable list) {
		value = list;
	}

	public ExpressionTree(final Expression val) {
		value = new ExpressionList(val);
	}

	public String toString() {
		return toString(',');
	}

	private String toString(final char separator) {
		final StringBuffer buffer = new StringBuffer();
		toString(separator, buffer);
		return buffer.toString();
	}

	private void toString(final char separator, final StringBuffer buffer) {
		if (hasBrother()) {
			buffer.append('(');
			buffer.append(value);
			if (hasChild()) {
				buffer.append('[');
				child.toString(separator, buffer);
				buffer.append(']');
			}
			buffer.append(" o ");
			brother.toString(separator, buffer);
			buffer.append(')');
			return;
		}
		buffer.append(value);
		if (hasChild()) {
			buffer.append(separator);
			child.toString(separator, buffer);
		}
	}

	public IExpressionIterable getValue() {
		return value;
	}

	private ExpressionTree addBrother(final IExpressionIterable equations) {
		return addBrother(new ExpressionTree(equations));
	}

	public ExpressionTree stringFold() {
		if (child != null)
			child.stringFold();
		if (brother != null)
			brother.stringFold();

		final String cString = value.toCleanString(',');
		Iterable<ExpressionTree> iterable = new Iterable<ExpressionTree>() {
			@Override
			public Iterator<ExpressionTree> iterator() {
				return getAllChildren();
			}
		};
		for (ExpressionTree curr : iterable) {
			if (curr != this && cString.equals(curr.value.toCleanString(','))) {
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

	public ExpressionTree addChild(final IExpressionIterable equations) {
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
		final StringBuffer buffer = new StringBuffer();
		toCleanString(separator, buffer);
		return buffer.toString();
	}

	private void toCleanString(final char separator, final StringBuffer buffer) {
		toTreeString(separator, buffer, 0);
	}

	private void toTreeString(final char separator, final StringBuffer buffer, final int index) {
		if (hasBrother()) {
			appendTree(buffer, index);
			buffer.append('°');
			value.toCleanString(',', buffer);
			if (hasChild()) {
				buffer.append(separator);
				child.toTreeString(separator, buffer, index + 1);
			}
			buffer.append(separator);
			appendTree(buffer, index);
			buffer.append('°');
			brother.value.toCleanString(',', buffer);
			if (brother.hasChild()) {
				buffer.append(separator);
				brother.child.toTreeString(separator, buffer, index + 1);
			}
			return;
		}
		appendTree(buffer, index);
		value.toCleanString(',', buffer);
		if (hasChild()) {
			buffer.append(separator);
			child.toTreeString(separator, buffer, index);
		}
	}

	private static void appendTree(final StringBuffer buffer, final int nested) {
		for (int a = 0; a < nested - 1; a++)
			buffer.append(' ');
		if (nested != 0)
			buffer.append('|');
	}

	public void toHtml(final StringBuffer buffer) {
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

	private void outputValue(final StringBuffer buffer) {
		if (value.length() > 1) {
			appendTable(buffer, value.getIterator());
			return;
		}
		if (value.isSingle() && value.getFirst() instanceof Matrix) {
			Matrix mat = (Matrix) value.getFirst();
			if (mat.getRows() * mat.getCols() > 0 && mat.get(0, 0) instanceof BooleanOp) {
				appendTable(buffer, mat.getElements());
				return;
			}
		}
		buffer.append('$');
		if (value.length() > 0)
			value.getFirst().toHtml(buffer);
		/*
		 * else buffer.append("∀");
		 */
		buffer.append('$');
	}

	private static void appendTable(final StringBuffer buffer, final IIterator<Expression> iterator) {
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

	public ExpressionTree addChild(final Expression[] expressions) {
		return addChild(new ExpressionList(expressions));
	}
}
