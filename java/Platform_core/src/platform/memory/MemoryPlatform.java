package platform.memory;

import platform.IPlatform;
import platform.log.MyLogger;

public class MemoryPlatform extends IPlatform{

	protected MyLogger getLoggerInternal(){
		return new MemoryLogger();
	}

}
