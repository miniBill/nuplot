package meplot.expressions.list;

public interface IToStringIterable<T> extends Iterable<T> {
	void toString(StringBuffer buffer);
}
