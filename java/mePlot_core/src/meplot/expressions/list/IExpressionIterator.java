package meplot.expressions.list;

import meplot.expressions.Expression;
import platform.lists.IToStringIterator;

public interface IExpressionIterator extends IToStringIterator{
	Expression next();

	IExpressionIterator subIterator();

	Expression getCurrent();

	Expression getLast();
}
