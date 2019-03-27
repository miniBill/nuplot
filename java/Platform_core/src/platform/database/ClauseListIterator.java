package platform.database;

import platform.lists.IEquatableIterable;
import platform.lists.IToString;
import platform.lists.ToStringIterator;

class ClauseListIterator extends ToStringIterator implements
		ClauseIterator{
	private final ClauseList inner;

	ClauseListIterator(final ClauseList head, final int index){
		super(index);
		inner = head;
	}

	public Clause next(){
		return inner.elementAt(index++);
	}

	public IToString tnext(){
		return next();
	}

	protected IEquatableIterable egetInner(){
		return inner;
	}
}
