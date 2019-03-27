package platform;

import platform.database.ITable;

public interface DatabaseBackend{
	ITable getTable(String string);

	void dropDatabase();
}
