package platform.persistence.listeners;

import platform.lists.List;

public class BooleanSettingsListenerList extends List{
	public void add(final BooleanSettingsListener list){
		super.add(list);
	}

	public BooleanSettingsListener elementAt(final int index){
		return (BooleanSettingsListener)gelementAt(index);
	}

	public BooleanSettingsListenerIterator getIterator(){
		return new BooleanSettingsListenerIterator(this);
	}
}
