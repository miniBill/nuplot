package platform.persistence.listeners;

import platform.lists.IIterable;
import platform.lists.Iterator;

public class BooleanSettingsListenerIterator extends Iterator{
	private final BooleanSettingsListenerList inner;

	BooleanSettingsListenerIterator(final BooleanSettingsListenerList head, final int index){
		super(index);
		inner = head;
	}

	BooleanSettingsListenerIterator(final BooleanSettingsListenerList booleanSettingsListenerList){
		inner = booleanSettingsListenerList;
	}

	public BooleanSettingsListener next(){
		return inner.elementAt(index++);
	}

	protected IIterable ggetInner(){
		return inner;
	}
}
