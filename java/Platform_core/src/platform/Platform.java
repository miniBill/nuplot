package platform;

import platform.database.DatabaseSchema;
import platform.log.MyLogger;
import platform.memory.MemoryPlatform;
import platform.persistence.SettingsBackend;
import platform.persistence.disk.FilesystemBackend;

public final class Platform{
	private static IPlatform platform = new MemoryPlatform();

	private Platform(){
	}

	public static IPlatform getPlatform(){
		return platform;
	}

	public static void set(final IPlatform platform){
		Platform.platform = platform;
	}

	public static SettingsBackend getSettingsBackend(){
		return platform.getSettingsBackend();
	}

	public static MyLogger getLogger(){
		return platform.getLogger();
	}

	public static boolean isJ2ME(){
		return platform.isJ2ME();
	}

	public static FilesystemBackend getFilesystemBackend(){
		return platform.getFilesystemBackend();
	}

	public static DatabaseBackend getDatabaseBackend(final DatabaseSchema schema){
		return platform.getDatabaseBackend(schema);
	}
}
