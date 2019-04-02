package platform;

import platform.log.MyLogger;
import platform.memory.MemoryPlatform;
import platform.persistence.SettingsBackend;

public final class Platform{
	private static final IPlatform platform = new MemoryPlatform();

	private Platform(){
	}

	public static IPlatform getPlatform(){
		return platform;
	}

	public static SettingsBackend getSettingsBackend(){
		return platform.getSettingsBackend();
	}

	public static MyLogger getLogger(){
		return platform.getLogger();
	}

}
