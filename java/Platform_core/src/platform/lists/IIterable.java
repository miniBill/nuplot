package platform.lists;

public interface IIterable<T> {
	default public int length() {
		int result = 0;
		IIterator<T> iterator = getIterator();
		while (iterator.hasNext()) {
			result++;
			iterator.next();
		}
		return result;
	}

	default public boolean isEmpty() {
		return !getIterator().hasNext();
	}

	default public boolean isSingle() {
		IIterator<T> iterator = getIterator();
		if (!iterator.hasNext())
			return false;
		iterator.next();
		return !iterator.hasNext();
	}

	default public T getFirst() {
		return getIterator().next();
	}

	IIterator<T> getIterator();

	default public boolean checkContains(IIterable<T> other) {
		final IIterator<T> it = getIterator();
		while (it.hasNext())
			if (!contains(it.next()))
				return false;
		return true;

	}

	default boolean contains(T arg) {
		return contains(arg, 0);
	}

	boolean contains(T arg, int start);
}
