package meplot.gui;

import javax.microedition.lcdui.TextField;

import platform.lists.List;

public final class TextFieldList extends List{
	public void add(final TextField field){
		super.add(field);
	}

	public TextFieldIterator getIterator(){
		return new TextFieldIterator(this);
	}

	public TextField elementAt(final int index){
		return (TextField)gelementAt(index);
	}

	public void dequeue(){
		super.removeAt(length() - 1);
	}

	public TextField getLast(){
		return (TextField)ggetLast();
	}
}
