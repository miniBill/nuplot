package platform.database;

import platform.lists.IToString;

public interface Row extends IToString{
	String get(int index);

	String get(String nameKey);
}
