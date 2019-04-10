package platform.lists;

public interface IIterator<T> extends java.util.Iterator<T>, java.lang.Iterable<T> {
	int length();

	static <T> List<T> until(java.util.Iterator<T> iterator, String string) {
		final List<T> toret = new List<>();
		while (iterator.hasNext()) {
			final T curr = iterator.next();
			if (curr.toString().equals(string))
				break;
			toret.add(curr);
		}
		return toret;
	}
}
