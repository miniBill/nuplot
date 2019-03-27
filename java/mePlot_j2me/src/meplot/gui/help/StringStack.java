package meplot.gui.help;

import java.util.Vector;

public final class StringStack{
	private final Vector container = new Vector();

	public void clear(){
		container.removeAllElements();
	}

	public void push(final String page){
		container.addElement(page);
	}

	public boolean isEmpty(){
		return container.size() == 0;
	}

	public String pop(){
		final String toret = (String)container.lastElement();
		container.removeElementAt(container.size() - 1);
		return toret;
	}

}
