package meplot.gui;

import javax.microedition.lcdui.TextField;

import platform.lists.IIterable;
import platform.lists.Iterator;

public final class TextFieldIterator extends Iterator{
	private final TextFieldList inner;

	TextFieldIterator(final TextFieldList head){
		inner = head;
	}

	TextFieldIterator(final TextFieldList head, final int index){
		super(index);
		inner = head;
	}

	public TextField next(){
		return inner.elementAt(index++);
	}

	protected IIterable ggetInner(){
		return inner;
	}
}
