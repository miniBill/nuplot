package platform.lists;

public abstract class ToStringList<T extends IToString> extends List<T> {
	public String toString() {
		return toString(',');
	}

	public final String toString(final char sep) {
		final StringBuffer toret = new StringBuffer();
		toString(toret, sep);
		return toret.toString();
	}

	public final void toString(final StringBuffer buffer) {
		toString(buffer, ',');
	}

	public final void toString(final StringBuffer buffer, final char sep) {
		final IIterator<T> iterator = getIterator(0);
		while (iterator.hasNext()) {
			iterator.next().toString(buffer);
			if (iterator.hasNext())
				buffer.append(sep);
		}
	}
}
