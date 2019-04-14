package meplot.expressions;

import meplot.expressions.geometry.ITensor;
import meplot.expressions.list.IValueList;
import meplot.expressions.list.ValueList;
import meplot.expressions.numbers.Dou;
import meplot.expressions.numbers.IDou;
import meplot.expressions.numbers.INumber;
import meplot.expressions.numbers.Int;
import meplot.expressions.operations.*;
import meplot.parser.utils.Cleaner;

import java.util.Iterator;

public abstract class AbstractExpression implements Expression {
	public boolean toStringStartsWith(char prefix) {
		// TODO: Fix
		return toString().indexOf(prefix) == 0;
	}

	private String fullString;

	protected boolean simplified;

	private String string;

	public final void toWrappedHtml(final StringBuilder buffer) {
		buffer.append('{');
		toHtml(buffer);
		buffer.append('}');
	}

	protected static boolean areIdentical(final Expression[] arg0, final Expression[] arg1) {
		if (arg0.length != arg1.length)
			return false;
		for (int i = 0; i < arg0.length; i++)
			if (!arg0[i].isIdentical(arg1[i]))
				return false;
		return true;
	}

	public Expression add(final Expression other) {
		if (isZero())
			return other;
		if (other.isZero())
			return this;

		return new Sum(this, other);
	}

    public boolean compatible(final Expression elementAt, final char operation) {
		if (operation != Operation.POWER && equals(elementAt))
			return true;

		if (operation == Operation.ADDITION || operation == Operation.DIVISION)
			return isOpposite(elementAt);

		return false;
	}

	public boolean containsMatrix() {
		return false;
	}

	public Expression divide(final Expression divisor) {
		if (divisor.isOne())
			return this;
		if (equals(divisor))
			return Int.ONE;
		if (isOpposite(divisor))
			return Int.MINUSONE;
		return new Division(this, divisor);
	}

	public final IDou idouvalue(final IValueList valueList) {
		return new Dou(dvalue(valueList));
	}

	public final IDou idouvalue(final char letter, final double value) {
		return new Dou(dvalue(letter, value));
	}

	public final double dvalue() {
		return dvalue('.', 0);
	}

	public double dvalue(final char var, final double value) {
		return value(var, value).toDouble();
	}

	private double dvalue(final IValueList valueList) {
		return value(valueList).toDouble();
	}

	public boolean equals(final Object obj) {
		if (obj == null)
			return false;
		return toString().equals(obj.toString());
	}

	public Expression expand() {
		return this;
	}

	public double fdvalue(final char letter, final double value) {
		return dvalue(letter, value);
	}

	public int hashCode() {
		return toFullString().hashCode();
	}

	public abstract boolean hasLetter(char letter);

	public Expression innerSimplify() {
		return this;
	}

	public Expression innerStepSimplify() {
		return innerSimplify();
	}

	public Expression inverse() {
		return new Division(Int.ONE, this);
	}

	public boolean isFullDouble() {
		return false;
	}

	public boolean isOne() {
		return false;
	}

	public final boolean isOpposite(final Expression other) {
		final Expression othop = other.opposite();
		final Expression opp = opposite();
		return equals(othop) || other.equals(opp);
	}

	public final boolean isSimplified() {
		return simplified;
	}

	public boolean isZero() {
		return false;
	}

	public ITensor matrixDvalue(final IValueList valueList) {
		return new Dou(dvalue(valueList));
	}

	public ITensor matrixDvalue(final char letter, final double value) {
		return matrixDvalue(new ValueList(letter, new Dou(value)));
	}

	public Expression multiply(final Expression other) {
		if (isOne())
			return other;
		if (other.isOne())
			return this;

		// a*0=0;
		if (isZero())
			return this;
		if (other.isZero())
			return other;

		if (equals(other))
			return new Power(this, Int.TWO);

		if (other.compatible(this, Operation.MULTIPLICATION))
			return other.multiply(this);

		return new Multiplication(this, other);
	}

	/**
	 * Return whether the expression requires to be wrapped in parenthesis for
	 * operator precedence.
	 * 
	 * @return Whether the expression needs parenthesis.
	 */
	public boolean needParenthesis() {
		return false;
	}

	public Expression opposite() {
		return Int.MINUSONE.multiply(this);
	}

	public final Expression partialSimplify() {
		if (simplified)
			return this;
		final Expression inner = innerSimplify();
		if (isIdentical(inner)) {
			simplified = true;
			return this;
		}
		return inner;
	}

	public Expression square() {
		return multiply(this);
	}

	public final String toCleanString() {
		return Cleaner.clean(toString());
	}

	/**
	 * Returns a String that describes the object in a completely unambiguous way.
	 * Possibly use RPN
	 * 
	 * @return A String that describes the object in a completely unambiguous way.
	 */
	public final String toFullString() {
		if (fullString == null) {
			final StringBuilder buffer = new StringBuilder();
			toFullString(buffer);
			fullString = buffer.toString();
		}
		return fullString;
	}

	public abstract void toFullString(final StringBuilder buffer);

	public void toPString(final StringBuilder buffer) {
		if (needParenthesis())
			buffer.append('(');
		toString(buffer);
		if (needParenthesis())
			buffer.append(')');
	}

	public final String toString() {
		if (string == null) {
			final StringBuilder buffer = new StringBuilder();
			toString(buffer);
			string = buffer.toString();
		}
		return string;
	}

	public abstract void toString(final StringBuilder buffer);

	public final INumber value() {
		return value(ValueList.EMPTY);
	}

	public INumber value(final char var, final double value) {
		return value(new ValueList(var, new Dou(value)));
	}

	public abstract INumber value(IValueList letters);

	protected static boolean areIdentical(final Iterator<Expression> arg0, final Iterator<Expression> arg1) {
		while (arg0.hasNext() && arg1.hasNext())
			if (!arg0.next().isIdentical(arg1.next()))
				return false;
		return !arg0.hasNext() && !arg1.hasNext();
	}
}
