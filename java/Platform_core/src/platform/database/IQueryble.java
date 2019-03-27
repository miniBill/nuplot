package platform.database;

public interface IQueryble{
	long getRowCount();

	String getName();

	Query getQuery();

	RowIterator execute();
}
