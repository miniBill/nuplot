package platform.database;

import platform.lists.IIterator;

public interface IQueryble {
	long getRowCount();

	String getName();

	Query getQuery();

	IIterator<Row> execute();
}
