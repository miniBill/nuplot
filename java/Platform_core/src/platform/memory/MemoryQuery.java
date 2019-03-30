package platform.memory;

import platform.database.Clause;
import platform.database.FilterRowIterator;
import platform.database.Query;
import platform.database.Row;
import platform.database.SelectClause;
import platform.database.SelectRowIterator;
import platform.database.WhereClause;
import platform.lists.IIterable;
import platform.lists.IIterator;

class MemoryQuery extends Query {
	private final MemoryTable table;

	MemoryQuery(final MemoryTable memoryTable) {
		table = memoryTable;
	}

	protected IIterator<Row> execute(final IIterable<Clause> clauses) {
		IIterator<Row> toret = table.getIterator();
		final IIterator<Clause> iterator = clauses.getIterator();
		while (iterator.hasNext()) {
			final Clause curr = iterator.next();
			if (curr instanceof WhereClause) {
				final WhereClause where = (WhereClause) curr;
				final String column = where.getColumn();
				final String value = where.getValue();
				toret = new FilterRowIterator(column, value, toret);
			}
			if (curr instanceof SelectClause) {
				final SelectClause select = (SelectClause) curr;
				final String column = select.getColumn();
				toret = new SelectRowIterator(column, toret);
			}
		}
		return toret;
	}
}
