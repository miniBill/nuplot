package meplot.expressions.list;

import meplot.expressions.Expression;
import platform.lists.IIterable;
import platform.lists.IIterator;

import java.util.Iterator;

public interface IValueList extends Iterable<IValueNode> {
	void add(char letter, Expression value);

	void add(IValueNode value);

	void set(char letter, Expression value);

	Expression value(char letter);

	boolean contains(char value);

	IValueNode elementAt(int index);
}
