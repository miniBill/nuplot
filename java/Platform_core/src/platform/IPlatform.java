package platform;

import platform.database.DatabaseSchema;
import platform.log.MyLogger;
import platform.persistence.SettingsBackend;
import platform.persistence.disk.FilesystemBackend;

public abstract class IPlatform{
	public boolean isJ2ME(){
		return false;
	}

	private SettingsBackend stbackend;

	public SettingsBackend getSettingsBackend(){
		if(stbackend == null)
			stbackend = getSettingsBackendInternal();
		return stbackend;
	}

	protected abstract SettingsBackend getSettingsBackendInternal();

	protected final void setPersistenceBackend(final SettingsBackend backend){
		stbackend = backend;
	}

	private MyLogger logger;

	public MyLogger getLogger(){
		if(logger == null)
			logger = getLoggerInternal();
		return logger;
	}

	protected abstract MyLogger getLoggerInternal();

	private FilesystemBackend fsbackend;

	public FilesystemBackend getFilesystemBackend(){
		if(fsbackend == null)
			fsbackend = getFilesystemBackendInternal();
		return fsbackend;
	}

	protected abstract FilesystemBackend getFilesystemBackendInternal();

	private DatabaseBackend dbbackend;

	public final DatabaseBackend getDatabaseBackend(final DatabaseSchema schema){
		if(dbbackend == null)
			dbbackend = getDatabaseBackendInternal(schema);
		return dbbackend;
	}

	protected abstract DatabaseBackend getDatabaseBackendInternal(
			DatabaseSchema schema);

	protected final void setDatabaseBackend(final DatabaseBackend backend){
		dbbackend = backend;
	}
}
