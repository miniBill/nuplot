package platform.database;

import platform.lists.IToString;

public interface ITable extends IQueryble, IToString{
	String getName();

	void commitChanges();

	void add(String[] values);

	IQueryble where(String tripsFrom, String name);
}
