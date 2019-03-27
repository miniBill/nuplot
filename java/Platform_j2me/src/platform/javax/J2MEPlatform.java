package platform.javax;

import platform.DatabaseBackend;
import platform.IPlatform;
import platform.NotImplementedException;
import platform.database.DatabaseSchema;
import platform.log.ConsoleLogger;
import platform.log.MyLogger;
import platform.persistence.SettingsBackend;
import platform.persistence.disk.FilesystemBackend;

public class J2MEPlatform extends IPlatform{
	protected MyLogger getLoggerInternal(){
		return new ConsoleLogger();
	}

	protected SettingsBackend getSettingsBackendInternal(){
		return new MeBackend();
	}

	public boolean isJ2ME(){
		return true;
	}

	protected FilesystemBackend getFilesystemBackendInternal(){
		throw new NotImplementedException();
	}

	protected DatabaseBackend getDatabaseBackendInternal(final DatabaseSchema schema){
		throw new NotImplementedException();
	}
}
