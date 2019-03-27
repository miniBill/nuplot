package platform.lists;

public abstract class EquatableIterator extends Iterator implements IEquatableIterator{
	protected EquatableIterator(int index){
		super(index);
	}

	protected EquatableIterator(){
	}

	public final boolean contains(final Object arg){
		return egetInner().contains(arg, index);
	}

	protected final IIterable ggetInner(){
		return egetInner();
	}

	protected abstract IEquatableIterable egetInner();
}
