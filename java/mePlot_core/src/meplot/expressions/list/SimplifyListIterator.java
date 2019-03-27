package meplot.expressions.list;

import meplot.expressions.ISimplifyNode;
import platform.lists.IIterable;
import platform.lists.Iterator;

public final class SimplifyListIterator extends Iterator{
	private final SimplifyList inner;

	public SimplifyListIterator(final SimplifyList head){
		inner = head;
	}

	protected IIterable ggetInner(){
		return inner;
	}

	public ISimplifyNode next(){
		return inner.elementAt(index++);
	}
}
