package platform.lists;

public abstract class AbstractIterator<T> implements IIterator<T> {
	protected int index;

    public final boolean isSingle() {
		return length() == 1;
	}
}