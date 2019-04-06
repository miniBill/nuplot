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
		toString(this, buffer);
	}

	public static <T extends IToString> void toString(Iterable<T> list, StringBuffer buffer) {
		toString(list, buffer, ',');
	}

	public final void toString(final StringBuffer buffer, final char sep) {
		toString(this, buffer, sep);
	}

	private static <T extends IToString> void toString(Iterable<T> list, StringBuffer buffer, char sep) {
		boolean first = true;
		for (T next : list) {
			if (!first)
				buffer.append(sep);
			first = false;
			next.toString(buffer);
		}
	}
}
