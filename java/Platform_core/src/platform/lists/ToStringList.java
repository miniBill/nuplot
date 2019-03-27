package platform.lists;

public abstract class ToStringList extends EquatableList{
	public String toString(){
		return toString(',');
	}

	public final String toString(final char sep){
		final StringBuffer toret = new StringBuffer();
		toString(toret, sep);
		return toret.toString();
	}

	public final void toString(final StringBuffer buffer){
		toString(buffer, ',');
	}

	public final void toString(final StringBuffer buffer, final char sep){
		final IToStringIterator iterator = tgetIterator(0);
		while(iterator.hasNext()){
			iterator.tnext().toString(buffer);
			if(iterator.hasNext())
				buffer.append(sep);
		}
	}

	public final IEquatableIterator egetIterator(final int index){
		return tgetIterator(index);
	}

	protected abstract IToStringIterator tgetIterator(final int index);
}
