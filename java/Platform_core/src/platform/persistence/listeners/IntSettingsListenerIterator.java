package platform.persistence.listeners;

import platform.lists.IIterable;
import platform.lists.Iterator;

public class IntSettingsListenerIterator extends Iterator{
	private final IntSettingsListenerList inner;

	IntSettingsListenerIterator(final IntSettingsListenerList head, final int index){
		super(index);
		inner = head;
	}

	IntSettingsListenerIterator(final IntSettingsListenerList intSettingsListenerList){
		inner = intSettingsListenerList;
	}

	public IntSettingsListener next(){
		return inner.elementAt(index++);
	}

	protected IIterable ggetInner(){
		return inner;
	}
}
