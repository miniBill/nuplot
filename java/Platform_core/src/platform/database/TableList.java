package platform.database;

import platform.lists.List;

public class TableList extends List<ITable> {
	public ITable get(final String name) {
		for (ITable curr : this)
			if (curr.getName().equals(name))
				return curr;
		return null;
	}

	public boolean contains(final String name) {
		for (ITable curr : this)
			if (curr.getName().equals(name))
				return true;
		return false;
	}
}
