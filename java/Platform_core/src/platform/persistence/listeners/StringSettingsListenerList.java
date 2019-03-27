package platform.persistence.listeners;

import platform.lists.List;

public class StringSettingsListenerList extends List{
	public void add(final StringSettingsListener list){
		super.add(list);
	}

	public StringSettingsListener elementAt(final int index){
		return (StringSettingsListener)gelementAt(index);
	}

	public StringSettingsListenerIterator getIterator(){
		return new StringSettingsListenerIterator(this);
	}
}
