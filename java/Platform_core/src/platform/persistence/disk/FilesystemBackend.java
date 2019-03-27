package platform.persistence.disk;


public interface FilesystemBackend{
	Directory getApplicationDirectory(String appName);
}
