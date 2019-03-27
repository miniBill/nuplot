package platform.database;

abstract class OperationQueryble implements IQueryble{
	private final IQueryble inner;

	protected OperationQueryble(final IQueryble inner){
		this.inner = inner;
	}

	public final String getName(){
		return inner.getName();
	}

	public abstract Query getQuery();

	public long getRowCount(){
		long count = 0;
		for(final RowIterator iterator = getQuery().execute(); iterator
				.hasNext(); iterator.next())
			count++;
		return count;
	}

	public final RowIterator execute(){
		return getQuery().execute();
	}

	public IQueryble getInner(){
		return inner;
	}
}
