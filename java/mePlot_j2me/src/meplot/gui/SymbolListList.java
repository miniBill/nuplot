package meplot.gui;

import platform.lists.List;

final class SymbolListList extends List{
	public void add(final SymbolList list){
		super.add(list);
	}

	public SymbolListIterator getIterator(){
		return new SymbolListIterator(this);
	}

	public SymbolList getCategory(final String string){
		for(int i = 0; i < length(); i++){
			final SymbolList elementAt = elementAt(i);
			if(elementAt.getTitle().equals(string))
				return elementAt;
		}
		return null;
	}

	public SymbolList elementAt(final int index){
		return (SymbolList)gelementAt(index);
	}
}
