package platform.lists;

import java.util.Vector;

public abstract class List implements IIterable{
	private final Vector container = new Vector();

	public final boolean contains(final Object exp){
		return container.contains(exp);
	}

	protected final void add(final Object obj){
		container.addElement(obj);
	}

	public final int length(){
		return container.size();
	}

	public final boolean isEmpty(){
		return container.size() == 0;
	}

	public final boolean isSingle(){
		return container.size() == 1;
	}

	public final boolean contains(final Object arg, final int start){
		return container.indexOf(arg, start) >= 0;
	}

	public final int indexOf(final Object arg){
		return container.indexOf(arg);
	}

	protected final Object ggetLast(){
		if(isEmpty())
			return null;
		return container.elementAt(container.size() - 1);
	}

	public final Object gelementAt(final int index){
		return container.elementAt(index);
	}

	protected final Object ggetFirst(){
		return container.elementAt(0);
	}

	public final void removeAt(final int index){
		container.removeElementAt(index);
	}

	protected final void insert(final Object obj, final int index){
		container.insertElementAt(obj, index);
	}
}
