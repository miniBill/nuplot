package platform.memory;

import platform.DatabaseBackend;
import platform.database.DatabaseSchema;
import platform.database.ITable;
import platform.database.TableList;

public class MemoryDatabase implements DatabaseBackend{
	private TableList tables = new TableList();
	private final DatabaseSchema schema;

	public MemoryDatabase(final DatabaseSchema schema){
		this.schema = schema;
	}

	public ITable getTable(final String name){
		if(!tables.contains(name))
			tables.add(new MemoryTable(name, schema.getColumns(name)));
		return tables.get(name);
	}

	public void dropDatabase(){
		tables = new TableList();
	}
}
