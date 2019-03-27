package platform.database;

public final class DatabaseSchema{
	private final TableSchema[] tables;

	public DatabaseSchema(final TableSchema[] tables){
		this.tables = tables;
	}

	public String[] getColumns(final String name){
		for(int i = 0; i < tables.length; i++)
			if(tables[i].getName().equals(name))
				return tables[i].getColumns();
		return new String[0];
	}

	public int getTableCount(){
		return tables.length;
	}

	public TableSchema getTable(final int index){
		return tables[index];
	}
}
