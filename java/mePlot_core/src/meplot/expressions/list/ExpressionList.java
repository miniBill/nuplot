package meplot.expressions.list;

import meplot.expressions.Expression;
import meplot.parser.utils.Cleaner;
import platform.lists.IToStringIterator;
import platform.lists.ToStringList;

public final class ExpressionList extends ToStringList implements IExpressionList{
	private static final IExpressionList EMPTY = new ExpressionList();

	public static IExpressionList getEmpty(){
		return EMPTY;
	}

	public ExpressionList(final Expression expr){
		add(expr);
	}

	/**
	 * Creates empty list.
	 */
	public ExpressionList(){
		// Creates empty list
	}

	public ExpressionList(final Expression expr, final IExpressionIterator expressionList){
		add(expr);
		addRange(expressionList);
	}

	public ExpressionList(final Expression expr1, final Expression expr2){
		add(expr1);
		add(expr2);
	}

	public ExpressionList(final Expression[] expressionList){
		for(int c = 0; c < expressionList.length; c++)
			add(expressionList[c]);
	}

	public ExpressionList(final IExpressionIterator iterator1, final IExpressionIterator iterator2){
		addRange(iterator1);
		addRange(iterator2);
	}

	public ExpressionList(final IExpressionList left, final IExpressionList right){
		addRange(left);
		addRange(right);
	}

	public ExpressionList(final IExpressionList left){
		addRange(left);
	}

	public ExpressionList(final IExpressionIterator iterator){
		addRange(iterator);
	}

	public void add(final Expression expr){
		super.add(expr);
	}

	public IToStringIterator tgetIterator(final int index){
		return getIterator(index);
	}

	public void addRange(final IExpressionList range){
		addRange(range.getIterator());
	}

	public void addRange(final IExpressionIterator toAdd){
		while(toAdd.hasNext())
			add(toAdd.next());
	}

	public boolean hasLetter(final char letter){
		for(int i = 0; i < length(); i++)
			if(elementAt(i).hasLetter(letter))
				return true;
		return false;
	}

	public Expression[] toArray(){
		final Expression[] toret = new Expression[length()];
		final IExpressionIterator iterator = getIterator();
		for(int c = 0; iterator.hasNext(); c++)
			toret[c] = iterator.next();
		return toret;
	}

	/**
	 * Returns a new list with duplicates squashed.
	 * 
	 * @return The list with no duplicates
	 */
	public IExpressionList fold(){
		final IExpressionList toret = new ExpressionList();
		final IExpressionIterator iterator = getIterator();
		while(iterator.hasNext()){
			final Expression current = iterator.next();
			if(!toret.contains(current))
				toret.add(current);
		}
		return toret;
	}

	public Expression getLast(){
		return (Expression)super.ggetLast();
	}

	public Expression elementAt(final int index){
		return (Expression)super.gelementAt(index);
	}

	public Expression getFirst(){
		return (Expression)super.ggetFirst();
	}

	public IExpressionIterator getIterator(){
		return new ExpressionIterator(this);
	}

	private IExpressionIterator getIterator(final int index){
		return new ExpressionIterator(this, index);
	}

	public void toCleanString(final char separator, final StringBuffer buffer){
		final IExpressionIterator iterator = getIterator();
		while(iterator.hasNext()){
			buffer.append(Cleaner.dematrix(iterator.next().toCleanString()));
			if(iterator.hasNext())
				buffer.append(separator);
		}
	}

	public String toCleanString(final char separator){
		final StringBuffer toret = new StringBuffer();
		toCleanString(separator, toret);
		return toret.toString();
	}
}
