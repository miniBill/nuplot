package platform.lists;

public abstract class ToStringIterator<T> extends Iterator<T> implements IIterator<T> {
	protected ToStringIterator(final int index) {
		super(index);
	}

	protected ToStringIterator() {
	}
}
