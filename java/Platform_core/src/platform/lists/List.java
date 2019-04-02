package platform.lists;

import java.util.Vector;

public class List<T> implements IIterable<T> {
	private final Vector<T> container = new Vector<>();

	public List() {
	}

	public List(T head)
	{
		add(head);
	}

	public final IIterator<T> getIterator() {
		return new ListIterator<>(this);
	}

	public final IIterator<T> getIterator(int index) {
		return new ListIterator<>(this, index);
	}

	public final boolean contains(final T exp) {
		return container.contains(exp);
	}

	public final void add(final T obj) {
		container.addElement(obj);
	}

	public final void addRange(final IIterable<T> other) {
		IIterator<T> iterator = other.getIterator();
		while (iterator.hasNext())
			add(iterator.next());
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

	public final boolean contains(final T arg, final int start) {
		return container.indexOf(arg, start) >= 0;
	}

	public final T getLast() {
		if (isEmpty())
			return null;
		return container.elementAt(container.size() - 1);
	}

	public final T elementAt(final int index) {
		return container.elementAt(index);
	}

	public final T getFirst() {
		return container.elementAt(0);
	}

	public final void removeAt(final int index) {
		container.removeElementAt(index);
	}

	public final void insert(final T obj, final int index) {
		container.insertElementAt(obj, index);
	}

	public final boolean checkContains(final IIterable<T> list) {
		final IIterator<T> it = list.getIterator();
		while (it.hasNext())
			if (!contains(it.next()))
				return false;
		return true;
	}

	public final boolean equals(final Object obj) {
		if (!(obj instanceof List<?>))
			return false;
		final List<T> other = (List<T>) obj;
		return checkContains(other) && other.checkContains(this);
	}

	public T pop()
	{
		T result = container.elementAt(0);
		container.remove(0);
		return result;
	}
}
