package platform.memory;

import platform.database.IQueryble;
import platform.database.ITable;
import platform.database.Row;
import platform.database.WhereRowIterable;
import platform.lists.IIterator;

public abstract class AbstractTable implements ITable {
	public final IIterator<Row> execute() {
		return getQuery().execute();
	}

	public final IQueryble where(final String column, final String value) {
		return new WhereRowIterable(this, column, value);
	}

	public final String toString() {
		return "TABLE " + getName();
	}

	public final void toString(final StringBuffer buffer) {
		buffer.append("TABLE ");
		buffer.append(getName());
	}
}
