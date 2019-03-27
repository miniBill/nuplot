package platform.database;

import platform.lists.IEquatableIterator;

public interface ClauseIterator extends IEquatableIterator{
	Clause next();
}
