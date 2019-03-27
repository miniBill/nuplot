package meplot.gui.help;

import javax.microedition.lcdui.Item;

import platform.lists.List;

public final class StringItemDictionary extends List{
	public void add(final String string, final Item item){
		super.add(new Object[]{string, item});
	}

	public StringItemIterator getIterator(){
		return new StringItemIterator(this);
	}

	public Item elementAt(final int index){
		return (Item)coupleAt(index)[0];
	}

	public String keyAt(final int index){
		return (String)coupleAt(index)[1];
	}

	private Object[] coupleAt(final int index){
		return (Object[])gelementAt(index);
	}

	public Item get(final String key){
		return getIterator().search(key);
	}
}
