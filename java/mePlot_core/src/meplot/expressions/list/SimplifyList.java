package meplot.expressions.list;

import meplot.expressions.Expression;
import meplot.expressions.ISimplifyNode;
import platform.lists.List;

public final class SimplifyList extends List{
	public Expression simplify(final Expression next){
		final SimplifyListIterator iterator = getIterator();
		Expression toret = next;
		while(iterator.hasNext())
			toret = iterator.next().simplify(toret);
		return toret;
	}

	public SimplifyListIterator getIterator(){
		return new SimplifyListIterator(this);
	}

	public ISimplifyNode elementAt(final int index){
		return (ISimplifyNode)gelementAt(index);
	}
}
