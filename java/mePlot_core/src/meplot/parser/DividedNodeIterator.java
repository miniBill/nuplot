package meplot.parser;

import platform.lists.IIterable;
import platform.lists.Iterator;

final class DividedNodeIterator extends Iterator{
	private final DividedNodeList inner;

	DividedNodeIterator(final DividedNodeList head, final int index){
		super(index);
		inner = head;
	}

	public DividedNode next(){
		return inner.elementAt(index++);
	}

	protected IIterable ggetInner(){
		return inner;
	}
}
