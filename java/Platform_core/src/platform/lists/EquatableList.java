package platform.lists;

public abstract class EquatableList extends List implements IEquatableIterable{
	public final boolean checkContains(final IEquatableIterable list){
		final IEquatableIterator it = list.egetIterator();
		while(it.hasNext())
			if(!contains(it.enext()))
				return false;
		return true;
	}

	public final boolean equals(final Object obj){
		if(!(obj instanceof EquatableList))
			return false;
		final EquatableList other = (EquatableList)obj;
		return checkContains(other) && other.checkContains(this);
	}

	public final IEquatableIterator egetIterator(){
		return egetIterator(0);
	}

	public abstract IEquatableIterator egetIterator(int index);
}
