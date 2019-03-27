package platform.lists;

public abstract class ToStringIterator extends EquatableIterator implements
		IToStringIterator{
	protected ToStringIterator(final int index){
		super(index);
	}

	protected ToStringIterator(){
	}

	public abstract IToString tnext();

	public final Object enext(){
		return tnext();
	}
}
