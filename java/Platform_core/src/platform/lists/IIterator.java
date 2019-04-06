package platform.lists;

public interface IIterator<T> extends java.util.Iterator<T>, java.lang.Iterable<T> {
	int length();

	boolean isSecond();

	boolean contains(T value);

	T getCurrent();

	List<T> until(String string);
}
