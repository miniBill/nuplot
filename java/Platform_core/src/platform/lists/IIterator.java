package platform.lists;

public interface IIterator<T> {
	T next();

	boolean hasNext();

	int length();

	boolean isEmpty();

	boolean isSingle();

	boolean isSecond();

	IIterator<T> subIterator();

	boolean contains(T value);

	T getLast();

	T getCurrent();

	List<T> until(String string);
}
