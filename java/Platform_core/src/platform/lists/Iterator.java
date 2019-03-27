package platform.lists;

public abstract class Iterator extends AbstractIterator{
	protected Iterator(){
	}

	protected Iterator(final int start){
		index = start;
	}

	protected abstract IIterable ggetInner();

	public final boolean hasNext(){
		return length() > 0;
	}

	private int cached_length = -1;

	public final int length(){
		if(cached_length == -1)
			cached_length = ggetInner().length();
		return cached_length - index;
	}

	public final String toString(){
		IIterable inner = ggetInner();
		if(!(inner instanceof List))
			return "";
		List list = (List)inner;
		final StringBuffer toret = new StringBuffer();
		for(int tindex = index; tindex < length(); tindex++){
			toret.append(list.gelementAt(tindex));
			if(tindex < length() - 1)
				toret.append(" > ");
		}
		return toret.toString();
	}
}
