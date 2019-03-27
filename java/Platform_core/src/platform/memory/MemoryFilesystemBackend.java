package platform.memory;

import platform.NotImplementedException;
import platform.persistence.disk.Directory;
import platform.persistence.disk.FilesystemBackend;

class MemoryFilesystemBackend implements FilesystemBackend{
	public Directory getApplicationDirectory(final String appName){
		throw new NotImplementedException();
	}
}
