package platform.lists;

public interface IEquatableIterator extends IIterator{
	Object enext();
	
	boolean contains(final Object arg);
}
