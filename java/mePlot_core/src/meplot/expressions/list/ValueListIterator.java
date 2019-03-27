package meplot.expressions.list;

import platform.lists.IIterable;
import platform.lists.Iterator;

final class ValueListIterator extends Iterator implements IValueListIterator{
	private final IValueList inner;

	ValueListIterator(final ValueList head){
		inner = head;
	}

	public IValueNode next(){
		return inner.elementAt(index++);
	}

	protected IIterable ggetInner(){
		return inner;
	}
}
