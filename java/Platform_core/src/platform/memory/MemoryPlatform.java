package platform.memory;

import platform.DatabaseBackend;
import platform.IPlatform;
import platform.database.DatabaseSchema;
import platform.log.MyLogger;
import platform.persistence.SettingsBackend;
import platform.persistence.disk.FilesystemBackend;

public class MemoryPlatform extends IPlatform{
	protected FilesystemBackend getFilesystemBackendInternal(){
		return new MemoryFilesystemBackend();
	}

	protected MyLogger getLoggerInternal(){
		return new MemoryLogger();
	}

	protected SettingsBackend getSettingsBackendInternal(){
		return new MemoryPersistence();
	}

	protected DatabaseBackend getDatabaseBackendInternal(
			final DatabaseSchema schema){
		return new MemoryDatabase(schema);
	}
}
