package platform.lists;

public abstract class ToStringList<T extends IToString> extends List<T> {
	public String toString() {
		return toString(this);
	}

	public final void toString(final StringBuilder buffer) {
		toString(this, buffer);
	}

	public static <T extends IToString> String toString(Iterable<T> list) {
		final StringBuilder toret = new StringBuilder();
		toString(list, toret);
		return toret.toString();
	}

	public static <T extends IToString> void toString(Iterable<T> list, StringBuilder buffer) {
		boolean first = true;
		for (T next : list) {
			if (!first)
				buffer.append(',');
			first = false;
			next.toString(buffer);
		}
	}

}
