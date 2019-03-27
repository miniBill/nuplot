package platform.persistence.disk;


public interface Directory{
	Directory getSubdirectory(String string);

	FileIterator getFiles();
}
