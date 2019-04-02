package platform;

import platform.log.MyLogger;
import platform.persistence.SettingsBackend;

public abstract class IPlatform{
	private SettingsBackend stbackend;

	public SettingsBackend getSettingsBackend(){
		if(stbackend == null)
			stbackend = getSettingsBackendInternal();
		return stbackend;
	}

	protected abstract SettingsBackend getSettingsBackendInternal();

	private MyLogger logger;

	public MyLogger getLogger(){
		if(logger == null)
			logger = getLoggerInternal();
		return logger;
	}

	protected abstract MyLogger getLoggerInternal();

}
