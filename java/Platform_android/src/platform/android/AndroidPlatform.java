package platform.android;

import platform.DatabaseBackend;
import platform.IPlatform;
import platform.android.database.AndroidDatabaseBackend;
import platform.android.log.LogCatLogger;
import platform.android.persistence.AndroidFilesystemBackend;
import platform.android.persistence.AndroidSettingsBackend;
import platform.database.DatabaseSchema;
import platform.log.MyLogger;
import platform.memory.MemoryDatabase;
import platform.persistence.NullBackend;
import platform.persistence.SettingsBackend;
import platform.persistence.disk.FilesystemBackend;
import android.content.Context;
import android.content.SharedPreferences;

public final class AndroidPlatform extends IPlatform{
	private static final AndroidPlatform ISTANCE = new AndroidPlatform();

	private AndroidPlatform(){
	}

	public static AndroidPlatform getIstance(){
		return ISTANCE;
	}

	@Override
	protected MyLogger getLoggerInternal(){
		return new LogCatLogger("AndroidPlatform");
	}

	@Override
	protected SettingsBackend getSettingsBackendInternal(){
		return new NullBackend();
	}

	public static void setSettings(final SharedPreferences settings){
		ISTANCE.setPersistenceBackend(new AndroidSettingsBackend(settings));
	}

	@Override
	protected FilesystemBackend getFilesystemBackendInternal(){
		return new AndroidFilesystemBackend();
	}

	private DatabaseSchema dbSchema;
	private Context context;
	private String dbName;

	@Override
	protected DatabaseBackend getDatabaseBackendInternal(
			final DatabaseSchema schema){
		dbSchema = schema;
		if(context != null)
			return new AndroidDatabaseBackend(context, dbName, schema);
		return new MemoryDatabase(schema);
	}

	public static void setDatabase(final Context context, final String dbName){
		ISTANCE.setDatabaseInternal(context, dbName);
	}

	public void setDatabaseInternal(final Context context, final String dbName){
		this.context = context;
		this.dbName = dbName;
		if(dbSchema != null){
			final DatabaseBackend backend = new AndroidDatabaseBackend(context,
					dbName, dbSchema);
			setDatabaseBackend(backend);
		}
	}
}
