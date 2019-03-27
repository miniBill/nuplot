package platform.database;

public class SelectRowIterator implements RowIterator{
	private final RowIterator iterator;
	private final String column;

	public SelectRowIterator(final String column, final RowIterator iterator){
		this.iterator = iterator;
		this.column = column;
	}

	public Row next(){
		final Row curr = iterator.next();
		return new StringsRow(curr.get(column), column);
	}

	public boolean hasNext(){
		return iterator.hasNext();
	}
}
