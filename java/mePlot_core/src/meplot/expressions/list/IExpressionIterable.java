package meplot.expressions.list;

import meplot.expressions.Expression;

@Deprecated
public interface IExpressionIterable extends IToStringIterable<Expression> {
	Expression elementAt(int index);

	Expression getFirst();

	void toCleanString(char separator, StringBuilder buffer);

	String toCleanString(char separator);
}
