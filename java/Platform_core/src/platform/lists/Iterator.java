package platform.lists;

public abstract class Iterator<T> extends AbstractIterator<T> {
	protected int start;

	protected Iterator() {
	}

	protected Iterator(final int start) {
		index = start;
		this.start = start;
	}

	protected abstract Iterable<T> getInner();

	public final boolean isSecond() {
		return index == start + 1;
	}

	private int cached_length = -1;

	public final int length() {
		if (cached_length == -1)
			cached_length = IIterable.length(getInner());
		return cached_length - index;
	}

	public final boolean contains(T value) {
		for (T curr : this)
			if (curr.equals(value))
				return true;
		return false;
	}

	public final String toString() {
		final StringBuilder toret = new StringBuilder();
		boolean first = true;
		for (T curr : this) {
			if (!first)
				toret.append(" > ");
			first = false;
			toret.append(curr);
		}
		return toret.toString();
	}
}
