package platform.database;

public class WhereRowIterable extends OperationQueryble{
	private final String column;
	private final String value;

	public WhereRowIterable(final IQueryble inner, final String column,
			final String value){
		super(inner);
		this.column = column;
		this.value = value;
	}

	public Query getQuery(){
		final Query toret = getInner().getQuery();
		toret.addWhereClause(column, value);
		return toret;
	}
}
