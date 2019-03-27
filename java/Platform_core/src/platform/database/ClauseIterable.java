package platform.database;

import platform.lists.IEquatableIterable;

public interface ClauseIterable extends IEquatableIterable{
	ClauseIterator getIterator();
}
