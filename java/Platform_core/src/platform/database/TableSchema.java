package platform.database;

public final class TableSchema{
	private final String name;
	private final String[] columns;

	public TableSchema(final String name, final String[] columns){
		this.name = name;
		this.columns = columns;
	}

	public String getName(){
		return name;
	}

	public String[] getColumns(){
		return columns;
	}
}
