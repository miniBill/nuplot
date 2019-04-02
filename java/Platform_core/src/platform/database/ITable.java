package platform.database;

import platform.lists.IToString;

public interface ITable extends IToString{
	String getName();

	void add(String[] values);

}
