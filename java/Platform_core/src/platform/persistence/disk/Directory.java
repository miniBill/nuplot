package platform.persistence.disk;

import platform.lists.IIterator;

public interface Directory {
	Directory getSubdirectory(String string);

	IIterator<File> getFiles();
}
