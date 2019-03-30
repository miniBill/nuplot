package platform.database;

import platform.lists.IIterator;

abstract class OperationQueryble implements IQueryble {
	private final IQueryble inner;

	protected OperationQueryble(final IQueryble inner) {
		this.inner = inner;
	}

	public final String getName() {
		return inner.getName();
	}

	public abstract Query getQuery();

	public long getRowCount() {
		long count = 0;
		for (final IIterator<Row> iterator = getQuery().execute(); iterator.hasNext(); iterator.next())
			count++;
		return count;
	}

	public final IIterator<Row> execute() {
		return getQuery().execute();
	}

	public IQueryble getInner() {
		return inner;
	}
}
