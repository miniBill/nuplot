package platform.memory;

import platform.database.Query;
import platform.database.Row;
import platform.database.RowIterator;
import platform.database.RowList;
import platform.database.StringsRow;

class MemoryTable extends AbstractTable{
	private RowList rows = new RowList();
	private final String name;
	private final String[] columns;

	MemoryTable(final String name, final String[] columns){
		this.name = name;
		this.columns = columns;
	}

	public RowIterator getIterator(){
		return rows.getIterator();
	}

	public String getName(){
		return name;
	}

	private void add(final Row row){
		rows.add(row);
	}

	public void commitChanges(){
		// In memory you don't update
	}

	public long getRowCount(){
		return rows.length();
	}

	public void add(final String[] values){
		add(new StringsRow(values, columns));
	}

	public Query getQuery(){
		return new MemoryQuery(this);
	}

	public void clear(){
		rows = new RowList();
	}
}
