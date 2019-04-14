package platform.lists;

import java.util.ArrayList;
import java.util.Iterator;

public class List<T> implements IList<T> {
	private final ArrayList<T> container = new ArrayList<>();

	public List() {
	}

	public List(T head)
	{
		add(head);
	}

	public List(Iterable<T> vals) {
		addRange(vals);
	}

	@Deprecated
	public List(T[] vals) {
		addRange(IterableExtensions.toList(vals));
	}

	public final Iterator<T> iterator() {
		return new ListIterator<>(this);
	}

	public final Iterator<T> iterator(int index) {
		return new ListIterator<>(this, index);
	}

	public final void add(final T obj) {
		container.add(obj);
	}

	public final void addRange(final Iterable<T> other) {
		for (T t : other)
			add(t);
	}

	public final int length() {
		return container.size();
	}

	public final boolean isEmpty() {
		return container.size() == 0;
	}

	public final boolean isSingle() {
		return container.size() == 1;
	}

	public final T getLast() {
		if (isEmpty())
			return null;
		return container .get(container.size() - 1);
	}

	public final T elementAt(final int index) {
		return container.get(index);
	}

	public final T getFirst() {
		return container.get(0);
	}

	public final void removeAt(final int index) {
		container.remove(index);
	}

	public final void insert(final T obj, final int index) {
		container.add(index, obj);
	}

	public final boolean equals(final Object obj) {
		if (!(obj instanceof List<?>))
			return false;
		final List<T> other = (List<T>) obj;
		return IterableExtensions.checkContains(this, other) && IterableExtensions.checkContains(other, this);
	}

	public T pop()
	{
		T result = container.get(0);
		container.remove(0);
		return result;
	}
}
