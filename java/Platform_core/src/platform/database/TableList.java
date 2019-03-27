package platform.database;

import platform.lists.EquatableList;
import platform.lists.IEquatableIterator;

public class TableList extends EquatableList{
	public final void add(final ITable table){
		super.add(table);
	}

	public ITable get(final String name){
		final TableListIterator iterator = new TableListIterator(this, 0);
		while(iterator.hasNext()){
			final ITable curr = iterator.next();
			if(curr.getName().equals(name))
				return curr;
		}
		return null;
	}

	public ITable elementAt(final int index){
		return (ITable)gelementAt(index);
	}

	public IEquatableIterator egetIterator(int index){
		return new TableListIterator(this, index);
	}
}
