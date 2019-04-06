package meplot.expressions.list;

import meplot.expressions.Expression;

public interface IExpressionIterable extends IToStringIterable<Expression> {
	Expression elementAt(int index);

	Expression getFirst();

	void toCleanString(char separator, StringBuffer buffer);

	String toCleanString(char separator);
}
