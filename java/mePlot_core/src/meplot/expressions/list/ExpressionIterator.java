package meplot.expressions.list;

import meplot.expressions.Expression;
import platform.lists.IEquatableIterable;
import platform.lists.IToString;
import platform.lists.ToStringIterator;

public class ExpressionIterator extends ToStringIterator implements IExpressionIterator{
	private final IExpressionIterable inner;

	public ExpressionIterator(final IExpressionIterable iterable){
		inner = iterable;
	}

	public ExpressionIterator(final IExpressionIterable iterable, final int start){
		super(start);
		inner = iterable;
	}

	public Expression next(){
		return inner.elementAt(index++);
	}

	public IExpressionIterator subIterator(){
		return new ExpressionIterator(inner, index);
	}

	public IToString tnext(){
		return next();
	}

	public Expression[] toArray(){
		final Expression[] toret = new Expression[length()];
		for(int i = index; i < inner.length(); i++)
			toret[i] = inner.elementAt(i);
		return toret;
	}

	public Expression getCurrent(){
		return inner.elementAt(index);
	}

	protected IEquatableIterable egetInner(){
		return inner;
	}

	public Expression getLast(){
		return inner.elementAt(inner.length() - 1);
	}
}
