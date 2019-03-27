package platform.database;

import platform.lists.EquatableList;
import platform.lists.IEquatableIterator;

final class ClauseList extends EquatableList implements ClauseIterable{
	public void add(final Clause clause){
		super.add(clause);
	}

	private ClauseIterator getIterator(final int index){
		return new ClauseListIterator(this, index);
	}

	public Clause elementAt(final int index){
		return (Clause)gelementAt(index);
	}

	public ClauseIterator getIterator(){
		return getIterator(0);
	}

	public IEquatableIterator egetIterator(int index){
		return getIterator(index);
	}
}
