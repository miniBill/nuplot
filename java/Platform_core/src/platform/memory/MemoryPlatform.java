package platform.memory;

import platform.IPlatform;
import platform.log.MyLogger;
import platform.persistence.SettingsBackend;

public class MemoryPlatform extends IPlatform{

	protected MyLogger getLoggerInternal(){
		return new MemoryLogger();
	}

	protected SettingsBackend getSettingsBackendInternal(){
		return new MemoryPersistence();
	}

}
