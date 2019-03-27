package platform.database;

import platform.lists.IEquatableIterable;
import platform.lists.IToString;
import platform.lists.ToStringIterator;

class TableListIterator extends ToStringIterator{
	private final TableList inner;

	TableListIterator(final TableList head, final int index){
		super(index);
		inner = head;
	}

	public ITable next(){
		return inner.elementAt(index++);
	}

	public IToString tnext(){
		return next();
	}

	protected IEquatableIterable egetInner(){
		return inner;
	}
}
