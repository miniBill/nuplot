package meplot.expressions.list;

import meplot.expressions.Expression;
import platform.lists.IList;

import java.util.Iterator;

@Deprecated
public interface IExpressionList extends IExpressionIterable, IList<Expression> {
	void addRange(Iterator<Expression> iterator);

	IExpressionList fold();

	Expression[] toArray();
}
