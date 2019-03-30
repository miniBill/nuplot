package platform.database;

import platform.lists.IIterator;
import platform.lists.List;

public class TableList extends List<ITable> {
	public ITable get(final String name) {
		final IIterator<ITable> iterator = getIterator();
		while (iterator.hasNext()) {
			final ITable curr = iterator.next();
			if (curr.getName().equals(name))
				return curr;
		}
		return null;
	}

	public boolean contains(final String name) {
		final IIterator<ITable> iterator = getIterator();
		while (iterator.hasNext()) {
			final ITable curr = iterator.next();
			if (curr.getName().equals(name))
				return true;
		}
		return false;
	}
}
