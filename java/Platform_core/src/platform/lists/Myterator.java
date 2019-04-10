package platform.lists;

public abstract class Myterator<T> implements IIterator<T> {
	protected int start;
	protected int index;

	protected Myterator() {
	}

	protected Myterator(final int start) {
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
