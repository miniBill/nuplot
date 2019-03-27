package platform.database;

public class FilterRowIterator implements RowIterator{
	private final RowIterator inner;
	private Row temp;
	private final String column;
	private final String value;

	public FilterRowIterator(final String column, final String value,
			final RowIterator iterator){
		this.column = column;
		this.value = value;
		inner = iterator;
	}

	public Row next(){
		if(!hasNext())
			return null;

		final Row toret = temp;
		temp = null;
		return toret;
	}

	public boolean hasNext(){
		if(temp != null)
			return true;
		do{
			temp = inner.next();
			if(temp == null)
				return false;
			if(temp.get(column).equals(value))
				return true;
		}while(inner.hasNext());
		temp = null;
		return false;
	}
}
