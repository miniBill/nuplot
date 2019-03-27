package platform.persistence.disk;

import platform.lists.IEquatableIterable;
import platform.lists.IToString;
import platform.lists.ToStringIterator;

public class FileIterator extends ToStringIterator{
	public static final FileIterator EMPTY = new FileIterator(FileList.EMPTY, 0);
	private final FileList inner;

	public FileIterator(final FileList list, final int index){
		super(index);
		inner = list;
	}

	public File next(){
		return inner.elementAt(index++);
	}

	protected IEquatableIterable egetInner(){
		return inner;
	}

	public IToString tnext(){
		return next();
	}
}
