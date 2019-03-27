package meplot.expressions.list;

import meplot.expressions.Expression;

public interface IValueNode{
	char getLetter();

	void setValue(Expression val);

	Expression getValue();
}
