package platform.lists;

import java.util.Iterator;

public abstract class Myterator<T> implements Iterator<T>, Iterable<T>{
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
