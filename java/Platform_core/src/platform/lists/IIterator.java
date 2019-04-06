package platform.lists;

public interface IIterator<T> extends java.util.Iterator<T> {
	int length();

	boolean isSecond();

	IIterator<T> subIterator();

	boolean contains(T value);

	T getLast();

	T getCurrent();

	List<T> until(String string);
}
