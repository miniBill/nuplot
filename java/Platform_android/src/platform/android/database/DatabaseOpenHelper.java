package platform.android.database;

import platform.database.DatabaseSchema;
import platform.database.TableSchema;
import android.content.Context;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

final class DatabaseOpenHelper extends SQLiteOpenHelper{
	private static final int DATABASE_VERSION = 1;
	private final DatabaseSchema schema;

	DatabaseOpenHelper(final Context context, final String dbName,
			final DatabaseSchema schema){
		super(context, dbName, null, DATABASE_VERSION);
		this.schema = schema;
	}

	@Override
	public void onCreate(final SQLiteDatabase database){
		for(int i = 0; i < schema.getTableCount(); i++){
			final TableSchema table = schema.getTable(i);
			database.execSQL(tableCreateSQL(table));
		}
	}

	private static String tableCreateSQL(final TableSchema table){
		final StringBuffer buffer = new StringBuffer("CREATE TABLE ");
		buffer.append(table.getName());
		buffer.append('(');
		for(int i = 0; i < table.getColumns().length; i++){
			buffer.append(table.getColumns()[i]);
			buffer.append(" TEXT");
			if(i < table.getColumns().length - 1)
				buffer.append(", ");
			else
				buffer.append(");");
		}
		return buffer.toString();
	}

	@Override
	public void onUpgrade(final SQLiteDatabase database, final int oldVersion,
			final int newVersion){
		// Not needed yet
	}

	public String[] getColumns(final String tableName){
		return schema.getColumns(tableName);
	}

	private SQLiteDatabase database;

	public SQLiteDatabase getDb(){
		if(database == null || !database.isOpen())
			database = getWritableDatabase();
		return database;
	}

	public static void drop(final Context context, final String databaseName){
		context.deleteDatabase(databaseName);
	}
}
