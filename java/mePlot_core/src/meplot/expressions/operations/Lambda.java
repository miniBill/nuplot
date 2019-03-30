package meplot.expressions.operations;

import meplot.expressions.AbstractExpression;
import meplot.expressions.Expression;
import meplot.expressions.Letter;
import meplot.expressions.list.IValueList;
import meplot.expressions.list.IValueNode;
import meplot.expressions.list.ValueList;
import meplot.expressions.numbers.INumber;
import meplot.expressions.visitors.IExpressionVisitor;
import platform.NotImplementedException;
import platform.lists.IIterator;

public final class Lambda extends AbstractExpression {
	private final Expression right;
	private final char left;

	public Expression partialSubstitute(final char letter, final double value) {
		if (letter == left)
			return this;
		return new Lambda(left, right.partialSubstitute(letter, value));
	}

	public Expression partialSubstitute(final char letter, final Expression value) {
		if (letter == left)
			return this;
		return new Lambda(left, right.partialSubstitute(letter, value));
	}

	public Expression partialSubstitute(final IValueList valueList) {
		final IValueList newList = new ValueList();
		final IIterator<IValueNode> iterator = valueList.getIterator();
		while (iterator.hasNext()) {
			final IValueNode curr = iterator.next();
			if (curr.getLetter() != left)
				newList.add(curr);
		}
		if (newList.length() > 0)
			return new Lambda(left, right.partialSubstitute(newList));
		return this;
	}

	public Lambda(final Expression left, final Expression right) {
		this.left = left.toString().charAt(0);
		this.right = right;
	}

	public Lambda(final char left, final Expression right) {
		this.left = left;
		this.right = right;
	}

	public boolean hasLetter(final char letter) {
		return left == letter || right.hasLetter(letter);
	}

	public void toFullString(final StringBuffer buffer) {
		buffer.append("=>(");
		buffer.append(left);
		buffer.append(',');
		right.toFullString(buffer);
		buffer.append(')');
	}

	public void toString(final StringBuffer buffer) {
		buffer.append(left);
		buffer.append("=>");
		right.toPString(buffer);
	}

	public INumber value(final IValueList letters) {
		return right.value(letters);
	}

	public boolean compatible(final Expression elementAt, final char operation) {
		if (operation == Operation.MULTIPLICATION)
			return true;
		if (elementAt instanceof Lambda)
			return true;
		return false;
	}

	public Expression multiply(final Expression other) {
		if (other instanceof Lambda)
			return multiply((Lambda) other);
		return right.partialSubstitute(left, other);
	}

	private Expression multiply(final Lambda other) {
		return new Lambda(left, right.partialSubstitute(left, other.multiply(new Letter(left))));
	}

	public Expression add(final Expression other) {
		if (other instanceof Lambda) {
			final Lambda lother = (Lambda) other;
			if (lother.right.hasLetter(left))
				return new Lambda(lother.left,
						lother.right.add(right.partialSubstitute(left, new Letter(lother.left))));
			return new Lambda(left, right.add(lother.right.partialSubstitute(lother.left, new Letter(left))));
		}
		return super.add(other);
	}

	public Expression innerSimplify() {
		return new Lambda(left, right.innerSimplify());
	}

	public Expression innerStepSimplify() {
		return new Lambda(left, right.innerStepSimplify());
	}

	public boolean equals(final Object obj) {
		if (obj instanceof Lambda) {
			final Lambda other = (Lambda) obj;
			final Expression oright = other.right.partialSubstitute(other.left, new Letter(left));
			return right.equals(oright);
		}
		return super.equals(obj);
	}

	public double fdvalue(final char letter, final double value) {
		return right.fdvalue(letter, value);
	}

	public boolean isFullDouble() {
		return right.isFullDouble();
	}

	public boolean isIdentical(final Expression other) {
		if (!(other instanceof Lambda))
			return false;
		final Lambda oth = (Lambda) other;
		return left == oth.left && right.isIdentical(oth.right);
	}

	public Expression accept(final IExpressionVisitor visitor) {
		return visitor.visit(this);
	}

	public char getLeft() {
		return left;
	}

	public Expression getRight() {
		return right;
	}

	public void toHtml(final StringBuffer buffer) {
		throw new NotImplementedException();
	}
}
