package platform.android.database;

import platform.DatabaseBackend;
import platform.database.DatabaseSchema;
import platform.database.ITable;
import android.content.Context;

public class AndroidDatabaseBackend implements DatabaseBackend{
	private final Context context;
	private final String dbName;
	private final DatabaseSchema schema;

	public AndroidDatabaseBackend(final Context context, final String dbName,
			final DatabaseSchema schema){
		this.context = context;
		this.dbName = dbName;
		this.schema = schema;
	}

	@Override
	public ITable getTable(final String tableName){
		final DatabaseOpenHelper helper = new DatabaseOpenHelper(context,
				dbName, schema);
		return new AndroidTable(helper, tableName);
	}

	@Override
	public void dropDatabase(){
		DatabaseOpenHelper.drop(context, dbName);
	}
}
