package platform.memory;

import platform.database.Clause;
import platform.database.ClauseIterable;
import platform.database.ClauseIterator;
import platform.database.FilterRowIterator;
import platform.database.Query;
import platform.database.RowIterator;
import platform.database.SelectClause;
import platform.database.SelectRowIterator;
import platform.database.WhereClause;

class MemoryQuery extends Query{
	private final MemoryTable table;

	MemoryQuery(final MemoryTable memoryTable){
		table = memoryTable;
	}

	protected RowIterator execute(final ClauseIterable clauses){
		RowIterator toret = table.getIterator();
		final ClauseIterator iterator = clauses.getIterator();
		while(iterator.hasNext()){
			final Clause curr = iterator.next();
			if(curr instanceof WhereClause){
				final WhereClause where = (WhereClause)curr;
				final String column = where.getColumn();
				final String value = where.getValue();
				toret = new FilterRowIterator(column, value, toret);
			}
			if(curr instanceof SelectClause){
				final SelectClause select = (SelectClause)curr;
				final String column = select.getColumn();
				toret = new SelectRowIterator(column, toret);
			}
		}
		return toret;
	}
}
