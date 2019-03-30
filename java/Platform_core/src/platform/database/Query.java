package platform.database;

import platform.lists.IIterable;
import platform.lists.IIterator;
import platform.lists.List;

public abstract class Query {
	private final List<Clause> clauses = new List<Clause>();

	public final void addWhereClause(final String column, final String value) {
		addClause(new WhereClause(column, value));
	}

	public final void addSelectClause(final String column) {
		addClause(new SelectClause(column));
	}

	private void addClause(final Clause clause) {
		clauses.add(clause);
	}

	public final IIterator<Row> execute() {
		return execute(clauses);
	}

	protected abstract IIterator<Row> execute(IIterable<Clause> clauses);
}
