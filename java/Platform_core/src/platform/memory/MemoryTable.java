package platform.memory;

import platform.database.Query;
import platform.database.Row;
import platform.database.StringsRow;
import platform.lists.IIterator;
import platform.lists.List;

class MemoryTable extends AbstractTable {
	private List<Row> rows = new List<Row>();
	private final String name;
	private final String[] columns;

	MemoryTable(final String name, final String[] columns) {
		this.name = name;
		this.columns = columns;
	}

	public IIterator<Row> getIterator() {
		return rows.getIterator();
	}

	public String getName() {
		return name;
	}

	private void add(final Row row) {
		rows.add(row);
	}

	public void commitChanges() {
		// In memory you don't update
	}

	public long getRowCount() {
		return rows.length();
	}

	public void add(final String[] values) {
		add(new StringsRow(values, columns));
	}

	public Query getQuery() {
		return new MemoryQuery(this);
	}

	public void clear() {
		rows = new List<Row>();
	}
}
