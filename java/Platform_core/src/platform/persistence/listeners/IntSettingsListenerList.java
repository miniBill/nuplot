package platform.persistence.listeners;

import platform.lists.List;

public class IntSettingsListenerList extends List{
	public void add(final IntSettingsListener list){
		super.add(list);
	}

	public IntSettingsListener elementAt(final int index){
		return (IntSettingsListener)gelementAt(index);
	}

	public IntSettingsListenerIterator getIterator(){
		return new IntSettingsListenerIterator(this);
	}
}
