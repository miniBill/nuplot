package platform.database;

public interface RowIterator{
	Row next();

	boolean hasNext();
}
