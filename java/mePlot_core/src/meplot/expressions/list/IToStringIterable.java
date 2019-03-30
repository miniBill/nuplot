package meplot.expressions.list;

import platform.lists.IIterable;

public interface IToStringIterable<T> extends IIterable<T> {
	void toString(StringBuffer buffer);
}
