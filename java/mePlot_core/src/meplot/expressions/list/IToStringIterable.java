package meplot.expressions.list;

import platform.lists.IEquatableIterable;

public interface IToStringIterable extends IEquatableIterable{
	void toString(StringBuffer buffer);
}
