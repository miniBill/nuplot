package platform.android.database;

import java.util.ArrayList;
import java.util.List;

import platform.database.Query;
import platform.database.Row;
import platform.database.StringsRow;
import platform.memory.AbstractTable;
import android.content.ContentValues;
import android.database.sqlite.SQLiteStatement;

class AndroidTable extends AbstractTable{
	private final DatabaseOpenHelper helper;
	private final String tableName;
	private final String[] columns;

	AndroidTable(final DatabaseOpenHelper helper, final String tableName){
		this.helper = helper;
		this.tableName = tableName;
		columns = helper.getColumns(tableName);
	}

	@Override
	public String getName(){
		return tableName;
	}

	private final List<Row> toadd = new ArrayList<Row>();

	@Override
	public void add(final String[] values){
		toadd.add(new StringsRow(values, columns));
	}

	@Override
	public void commitChanges(){
		for(final Row row : toadd){
			final ContentValues values = new ContentValues();
			for(int i = 0; i < columns.length; i++)
				values.put(columns[i], row.get(i));
			helper.getDb().insert(tableName, null, values);
		}
		helper.getDb().close();
	}

	@Override
	public long getRowCount(){
		final SQLiteStatement statement = helper.getDb().compileStatement(
				"SELECT COUNT(*) FROM " + tableName);
		return statement.simpleQueryForLong();
	}

	@Override
	public Query getQuery(){
		return new AndroidQuery(this);
	}

	public DatabaseOpenHelper getBackend(){
		return helper;
	}

	/*
	 * public void clear(){ final SQLiteStatement statement =
	 * helper.getDb().compileStatement( "DELETE FROM " + tableName);
	 * statement.execute(); }
	 */
}
