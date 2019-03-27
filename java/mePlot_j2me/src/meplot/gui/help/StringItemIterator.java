package meplot.gui.help;

import javax.microedition.lcdui.Item;

import platform.lists.IIterable;
import platform.lists.Iterator;

public final class StringItemIterator extends Iterator{
	private final StringItemDictionary inner;

	StringItemIterator(final StringItemDictionary head){
		inner = head;
	}

	StringItemIterator(final StringItemDictionary head, final int index){
		super(index);
		inner = head;
	}

	public String getCurrentKey(){
		return inner.keyAt(index);
	}

	public Item next(){
		return inner.elementAt(index++);
	}

	protected IIterable ggetInner(){
		return inner;
	}

	public Item search(final String key){
		while(hasNext()){
			if(getCurrentKey().equals(key))
				return next();
			next();
		}
		return null;
	}
}
