package platform.lists;

public interface IIterable<T> {
	int length();

	boolean isEmpty();

	boolean isSingle();

	IIterator<T> getIterator();

	boolean checkContains(IIterable<T> other);

	boolean contains(T current);

	boolean contains(T arg, int start);
}
