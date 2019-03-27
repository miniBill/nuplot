package platform.lists;

public interface IEquatableIterable extends IIterable{
	boolean checkContains(IEquatableIterable addends);

	IEquatableIterator egetIterator();

	boolean contains(Object current);

	boolean contains(Object arg, int start);
}
