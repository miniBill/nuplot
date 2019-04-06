package platform;

import platform.log.MyLogger;
import platform.memory.MemoryLogger;

public final class Platform{
	private static final MyLogger logger= new MemoryLogger();

	private Platform(){
	}

	public static MyLogger getLogger(){
		return logger;
	}

}
