package platform.persistence.listeners;

import platform.lists.IIterable;
import platform.lists.Iterator;

public class StringSettingsListenerIterator extends Iterator{
	private final StringSettingsListenerList inner;

	StringSettingsListenerIterator(final StringSettingsListenerList head, final int index){
		super(index);
		inner = head;
	}

	StringSettingsListenerIterator(final StringSettingsListenerList stringSettingsListenerList){
		inner = stringSettingsListenerList;
	}

	public StringSettingsListener next(){
		return inner.elementAt(index++);
	}

	protected IIterable ggetInner(){
		return inner;
	}
}
