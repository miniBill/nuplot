package meplot.gui;

import platform.lists.IIterable;
import platform.lists.Iterator;

final class SymbolListIterator extends Iterator{
	private final SymbolListList inner;

	SymbolListIterator(final SymbolListList head){
		inner = head;
	}

	SymbolListIterator(final SymbolListList head, final int index){
		super(index);
		inner = head;
	}

	public SymbolList next(){
		return inner.elementAt(index++);
	}

	protected IIterable ggetInner(){
		return inner;
	}
}
