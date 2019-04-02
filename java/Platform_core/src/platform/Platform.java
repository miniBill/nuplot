package platform;

import platform.log.MyLogger;
import platform.memory.MemoryPlatform;

public final class Platform{
	private static final IPlatform platform = new MemoryPlatform();

	private Platform(){
	}

	public static MyLogger getLogger(){
		return platform.getLogger();
	}

}
