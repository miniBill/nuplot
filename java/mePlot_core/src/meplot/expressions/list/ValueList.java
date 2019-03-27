package meplot.expressions.list;

import meplot.expressions.Expression;
import meplot.expressions.numbers.Int;
import platform.lists.List;

public final class ValueList extends List implements IValueList{
	public static final IValueList EMPTY = new ValueList();

	/**
	 * Creates empty list.
	 */
	public ValueList(){
		// Creates empty list
	}

	public ValueList(final char start, final Expression expr){
		add(start, expr);
	}

	public ValueList(final char first, final Expression firste, final char second,
			final Expression seconde){
		add(first, firste);
		add(second, seconde);
	}

	public void add(final char var, final Expression expr){
		add(new ValueNode(var, expr));
	}

	public void add(final IValueNode list){
		super.add(list);
	}

	public boolean contains(final char letter){
		for(int i = 0; i < length(); i++)
			if(elementAt(i).getLetter() == letter)
				return true;
		return false;
	}

	public void set(final char letter, final Expression val){
		for(int i = 0; i < length(); i++){
			final IValueNode curr = elementAt(i);
			if(curr.getLetter() == letter)
				curr.setValue(val);
		}
	}

	public Expression value(final char letter){
		for(int i = 0; i < length(); i++){
			final IValueNode curr = elementAt(i);
			if(curr.getLetter() == letter)
				return curr.getValue();
		}
		return Int.ZERO;
	}

	public IValueNode elementAt(final int index){
		return (IValueNode)gelementAt(index);
	}

	public IValueListIterator getIterator(){
		return new ValueListIterator(this);
	}
}
