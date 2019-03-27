package meplot.expressions.list;

import meplot.expressions.Expression;
import platform.lists.IIterable;

public interface IValueList extends IIterable{
	void add(char letter, Expression value);

	void add(IValueNode value);

	IValueListIterator getIterator();

	void set(char letter, Expression value);

	Expression value(char letter);

	boolean contains(char value);

	IValueNode elementAt(int index);
}
