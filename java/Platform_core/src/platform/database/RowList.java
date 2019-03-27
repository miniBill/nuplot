package platform.database;

import platform.lists.List;

public class RowList extends List{
	public final void add(final Row row){
		super.add(row);
	}

	public Row elementAt(final int index){
		return (Row)gelementAt(index);
	}

	public RowIterator getIterator(){
		return new RowListIterator(this);
	}
}
