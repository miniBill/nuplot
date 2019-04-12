package platform.lists;

import java.util.Iterator;

public abstract class Myterator<T> implements Iterator<T> {
	protected int start;
	protected int index;

	protected Myterator() {
	}

	protected Myterator(final int start) {
		index = start;
		this.start = start;
	}

	public abstract Iterable<T> clone();

	public final boolean isSecond() {
		return index == start + 1;
	}

	public final String toString() {
		final StringBuilder toret = new StringBuilder();
		boolean first = true;
		for (T curr : clone()) {
			if (!first)
				toret.append(" > ");
			first = false;
			toret.append(curr);
		}
		return toret.toString();
	}
}
