package platform.lists;

public abstract class AbstractIterator implements IIterator{
	protected int index;

	public final boolean isEmpty(){
		return length() == 0;
	}

	public final boolean isSingle(){
		return length() == 1;
	}
}