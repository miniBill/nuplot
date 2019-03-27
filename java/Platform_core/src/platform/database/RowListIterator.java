package platform.database;

import platform.lists.IIterable;
import platform.lists.Iterator;

class RowListIterator extends Iterator implements RowIterator{
	private final RowList inner;

	RowListIterator(final RowList head, final int index){
		super(index);
		inner = head;
	}

	RowListIterator(final RowList head){
		inner = head;
	}

	public Row next(){
		return inner.elementAt(index++);
	}

	protected IIterable ggetInner(){
		return inner;
	}
}
