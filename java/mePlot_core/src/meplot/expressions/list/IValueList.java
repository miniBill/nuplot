package meplot.expressions.list;

import meplot.expressions.Expression;
import platform.lists.IIterable;
import platform.lists.IIterator;

public interface IValueList extends IIterable<IValueNode> {
	void add(char letter, Expression value);

	void add(IValueNode value);

	IIterator<IValueNode> getIterator();

	void set(char letter, Expression value);

	Expression value(char letter);

	boolean contains(char value);

	IValueNode elementAt(int index);
}
