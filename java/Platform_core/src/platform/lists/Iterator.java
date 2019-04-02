package platform.lists;

public abstract class Iterator<T> extends AbstractIterator<T> {
	protected int start;

	protected Iterator() {
	}

	protected Iterator(final int start) {
		index = start;
		this.start = start;
	}

	protected abstract IIterable<T> getInner();

	public final boolean hasNext() {
		return length() > 0;
	}

	public final boolean isSecond() {
		return index == start + 1;
	}

	private int cached_length = -1;

	public final int length() {
		if (cached_length == -1)
			cached_length = getInner().length();
		return cached_length - index;
	}

	public final boolean contains(T value) {
		IIterator<T> clone = subIterator();
		while (clone.hasNext()) {
			T curr = clone.next();
			if (curr.equals(value))
				return true;
		}
		return false;
	}

	public final String toString() {
		IIterable<T> inner = getInner();
		if (!(inner instanceof List<?>))
			return "";
		List<T> list = (List<T>) inner;
		final StringBuilder toret = new StringBuilder();
		for (int tindex = index; tindex < inner.length(); tindex++) {
			toret.append(list.elementAt(tindex));
			if (tindex < length() - 1)
				toret.append(" > ");
		}
		return toret.toString();
	}

	public final List<T> until(String string) {
		final List<T> toret = new List<>();
		while (hasNext()) {
			final T curr = next();
			if (curr.toString().equals(string))
				break;
			toret.add(curr);
		}
		return toret;
	}
}
