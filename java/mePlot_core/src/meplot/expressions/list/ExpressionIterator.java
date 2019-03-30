package meplot.expressions.list;

import meplot.expressions.Expression;
import platform.lists.IIterable;
import platform.lists.IIterator;
import platform.lists.IToString;
import platform.lists.ToStringIterator;

public class ExpressionIterator extends ToStringIterator<Expression> implements IIterator<Expression> {
	private final IExpressionIterable inner;

	public ExpressionIterator(final IExpressionIterable iterable) {
		inner = iterable;
	}

	public ExpressionIterator(final IExpressionIterable iterable, final int start) {
		super(start);
		inner = iterable;
	}

	public Expression next() {
		return inner.elementAt(index++);
	}

	public IIterator<Expression> subIterator() {
		return new ExpressionIterator(inner, index);
	}

	public IToString tnext() {
		return next();
	}

	public Expression[] toArray() {
		final Expression[] toret = new Expression[length()];
		for (int i = index; i < inner.length(); i++)
			toret[i] = inner.elementAt(i);
		return toret;
	}

	public Expression getCurrent() {
		return inner.elementAt(index);
	}

	public IIterable<Expression> getInner() {
		return inner;
	}

	public Expression getLast() {
		return inner.elementAt(inner.length() - 1);
	}
}
