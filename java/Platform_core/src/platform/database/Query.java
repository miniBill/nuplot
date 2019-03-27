package platform.database;

public abstract class Query{
	private final ClauseList clauses = new ClauseList();

	public final void addWhereClause(final String column, final String value){
		addClause(new WhereClause(column, value));
	}

	public final void addSelectClause(final String column){
		addClause(new SelectClause(column));
	}

	private void addClause(final Clause clause){
		clauses.add(clause);
	}

	public final RowIterator execute(){
		return execute(clauses);
	}

	protected abstract RowIterator execute(ClauseIterable clauses);
}
