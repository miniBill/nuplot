package platform;

import platform.log.MyLogger;

public abstract class IPlatform{
	private MyLogger logger;

	public MyLogger getLogger(){
		if(logger == null)
			logger = getLoggerInternal();
		return logger;
	}

	protected abstract MyLogger getLoggerInternal();

}
