package platform.android.database;

import java.util.ArrayList;
import java.util.List;

import platform.database.Clause;
import platform.database.ClauseIterable;
import platform.database.ClauseIterator;
import platform.database.Query;
import platform.database.RowIterator;
import platform.database.SelectClause;
import platform.database.WhereClause;

class AndroidQuery extends Query{
	private final AndroidTable table;

	AndroidQuery(final AndroidTable androidTable){
		table = androidTable;
	}

	@Override
	protected RowIterator execute(final ClauseIterable clauses){
		final ClauseIterator iterator = clauses.getIterator();
		final StringBuilder whereClause = new StringBuilder();
		final List<String> whereArgs = new ArrayList<String>();
		String selected = "";
		while(iterator.hasNext()){
			final Clause curr = iterator.next();
			if(curr instanceof WhereClause){
				final WhereClause where = (WhereClause)curr;
				final String column = where.getColumn();
				final String value = where.getValue();
				if(whereClause.length() != 0)
					whereClause.append(" and ");
				whereClause.append(column);
				whereClause.append(" = ?");
				whereArgs.add(value);
			}
			if(curr instanceof SelectClause && selected.length() == 0){
				final SelectClause select = (SelectClause)curr;
				selected = select.getColumn();
			}
		}

		final String[] whereArgsArray = new String[whereArgs.size()];
		whereArgs.toArray(whereArgsArray);

		final String[] nselected = selected.length() == 0 ? null
				: new String[]{selected};
		final String nWhereClause = whereClause.length() > 0 ? whereClause
				.toString() : null;
		final String[] nWhereArgsArray = whereArgsArray.length > 0 ? whereArgsArray
				: null;
		return new AndroidRowIterator(table, nselected, nWhereClause,
				nWhereArgsArray);
	}
}
