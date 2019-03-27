package platform.android.database;

import platform.database.Row;
import platform.database.RowIterator;
import platform.database.StringsRow;
import android.database.Cursor;

class AndroidRowIterator implements RowIterator{
	private final DatabaseOpenHelper backend;
	private final String tableName;
	private String[] columns;

	private boolean started;

	AndroidRowIterator(final AndroidTable table, final String[] columns,
			final String whereClause, final String[] whereArgs){
		backend = table.getBackend();
		this.columns = columns;
		tableName = table.getName();
		this.whereClause = whereClause;
		this.whereArgs = whereArgs;
	}

	private Cursor cursor;
	private final String whereClause;
	private final String[] whereArgs;

	@Override
	public boolean hasNext(){
		if(!started)
			initQuery();
		if(cursor.isAfterLast()){
			if(!cursor.isClosed())
				cursor.close();
			return false;
		}
		return true;
	}

	private void initQuery(){
		started = true;
		cursor = backend.getDb().query(tableName, columns, whereClause,
				whereArgs, null, null, null);
		cursor.moveToFirst();
	}

	@Override
	public Row next(){
		if(!started)
			initQuery();
		if(cursor.isAfterLast()){
			if(!cursor.isClosed())
				cursor.close();
			return null;
		}
		final String[] values = new String[getColumns().length];
		for(int i = 0; i < values.length; i++)
			values[i] = cursor.getString(i);
		final StringsRow toret = new StringsRow(values, getColumns());
		cursor.moveToNext();
		return toret;
	}

	private String[] getColumns(){
		if(columns == null)
			columns = backend.getColumns(tableName);
		return columns;
	}

}
